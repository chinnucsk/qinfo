/// @file   plaza2_driver_dll.cpp
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/11/2012 09:30:56 PM

#include "precomp.h"
#include "application.h"
#include "connection.h"
#include "plaza2_driver_dll.h"

#include <ei_cxx/tuple.h>
#include <ei_cxx/port.h>
#include <ei_cxx/atom.h>
#include <ei_cxx/list.h>
#include <qinfo_common/log_wrapper.h>

#include <erl_driver.h>
#include <ei.h>

using namespace Plaza2;

struct PortData;

void process_init(PortData*, ei_cxx::ITuple&);
void process_connect(PortData*, ei_cxx::ITuple&);
void process_disconnect(PortData*);

BOOL APIENTRY DllMain( HANDLE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
                )
{
   switch (ul_reason_for_call)
   {
   case DLL_PROCESS_ATTACH:
   case DLL_THREAD_ATTACH:
   case DLL_THREAD_DETACH:
   case DLL_PROCESS_DETACH:
      break;
   }
    return TRUE;
}

struct PortData
{
   ei_cxx::Port port;
   Connection* conn;
};

//------------------------------------------------------------------------------------------------------------------------//
PLAZA2_DRIVER_DLL_API ErlDrvData start(ErlDrvPort port, char *buff)
{
   PortData* d = (PortData*)driver_alloc(sizeof(PortData));
   d->port = port;
   d->conn = new Connection(d->port);
   return reinterpret_cast<ErlDrvData>(d);
}

//------------------------------------------------------------------------------------------------------------------------//
PLAZA2_DRIVER_DLL_API void stop(ErlDrvData handle)
{
   PortData* pd = reinterpret_cast<PortData*>(handle);
   pd->conn->disconnect();
   delete pd->conn;
   driver_free((char*)handle);
}

//------------------------------------------------------------------------------------------------------------------------//
PLAZA2_DRIVER_DLL_API void received(ErlDrvData drv_data, ErlIOVec *ev)
{
   PortData* pd = reinterpret_cast<PortData*>(drv_data);
   try
   {
      using namespace ei_cxx;
      for(size_t i = 1; i < ev->vsize; ++i)
      {
         IBinary bin(ev->binv[i]);
         if (bin.get_type() == IBinary::ErlSmallTuple || bin.get_type() == IBinary::ErlLargeTuple)
         {
            ITuple tuple;
            bin >> tuple;
            Atom command_name;
            tuple >> command_name;
            if (command_name.get() == "init")
            {
               process_init(pd, tuple);
            }
            if (command_name.get() == "disconnect")
            {
               process_disconnect(pd);
            }
            else if (command_name.get() == "connect")
            {
               process_connect(pd, tuple);
            }
            else if (command_name.get() == "log_level")
            {
               Atom llevel;
               tuple >> llevel;
               log_level = LogLevel::fromString(llevel.get());
            }
            else
            {
               THROW(std::runtime_error, "Unknown command");
            }
         }
         else
         {
            THROW(std::runtime_error, "Unknown command");
         }
      }
   }
   catch(std::exception const& err)
   {
      ERL_LOG_ERROR(pd->port, err.what());
   }
   catch(_com_error const& err)
   {
      ERL_LOG_ERROR(pd->port,
         "received(): COM error. Error=" << err.Error() << ", Message=" << err.ErrorMessage());
   }
}

/// {init, iniFile}
void process_init(PortData*, ei_cxx::ITuple& t)
{
   std::string iniFile;
   t >> iniFile;
   static Application app(iniFile);
}

// {connect, iniFile, "host", port, "app_name", "password", log_level, [{FUT_TRADE_REPL, iniFile, StreamType}]}.
void process_connect(PortData* pd, ei_cxx::ITuple& t)
{
   using namespace ei_cxx;
   std::string host;
   unsigned long port;
   std::string appName;
   std::string password;
   Atom llevel;
   t >> host >> port >> appName >> password >> llevel;
   log_level = LogLevel::fromString(llevel.get());

   pd->conn->connect(host, port, appName, password);

   IList streams;
   t >> streams;
   size_t len = streams.size();
   for(size_t i = 0; i < len; ++i)
   {
      ITuple stream;
      streams >> stream;
      std::string streamName;
      std::string iniFile;
      Atom streamType;
      stream >> streamName >> iniFile >> streamType;
      pd->conn->addStream(streamName, iniFile, StreamType::fromString(streamType.get()));
   }
}

// {disconnect}
void process_disconnect(PortData* pd)
{
   pd->conn->disconnect();
}

PLAZA2_DRIVER_DLL_API ErlDrvEntry plaza2_driver_entry =
{
   NULL,               /* F_PTR init, N/A */
   start,              /* L_PTR start, called when port is opened */
   stop,               /* F_PTR stop, called when port is closed */
   NULL,               /* F_PTR output, called when erlang has sent
                        data to the port */
   NULL,               /* F_PTR ready_input,
                        called when input descriptor ready to read*/
   NULL,               /* F_PTR ready_output,
                        called when output descriptor ready to write */
   "plaza2_driver",    /* char *driver_name, the argument to open_port */
   NULL,               /* F_PTR finish, called when unloaded */
   NULL,               /* handle  */
   NULL,               /* F_PTR control, port_command callback */
   NULL,               /* F_PTR timeout, reserved */
   received,           /* F_PTR outputv, reserved */
   NULL,
   NULL,
   NULL,
   NULL,
   ERL_DRV_EXTENDED_MARKER,
   ERL_DRV_EXTENDED_MAJOR_VERSION,
   ERL_DRV_EXTENDED_MINOR_VERSION,
   0,
   NULL,
   NULL,
   NULL
};

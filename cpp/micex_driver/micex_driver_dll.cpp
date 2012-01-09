/// @file   micex_driver_dll.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/04/2012 08:50:51 AM

#include "connection.h"
#include "micex_driver_dll.h"

#include <ei_cxx/tuple.h>
#include <ei_cxx/port.h>
#include <ei_cxx/atom.h>
#include <ei_cxx/list.h>

#include <erl_driver.h>
#include <ei.h>

using namespace micex;

//Connection g_conn;

ei_cxx::Port g_port;

void process_connect(ei_cxx::ITuple&);
void process_disconnect();

void sendError(std::string const& err)
{
   using namespace ei_cxx;
   OTuple t(2);
   t  << Atom("error")
      << err;
   t.send(g_port);
}

void sendOk(std::string const& text)
{
   using namespace ei_cxx;
   OTuple t(2);
   t << Atom("ok") << Atom(text);
   t.send(g_port);
}


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
	ErlDrvPort port;
};

MICEX_DRIVER_DLL_API ErlDrvData start(ErlDrvPort port, char *buff)
{
	PortData* d = (PortData*)driver_alloc(sizeof(PortData));
    d->port = port;
    if (!g_port)
    {
      g_port = port;
    }
    return (ErlDrvData)d;
}

MICEX_DRIVER_DLL_API void stop(ErlDrvData handle)
{
   //Connection::instance()->disconnect();
   driver_free((char*)handle);
   g_port = NULL;
}

//------------------------------------------------------------------------------------------------------------------------//
MICEX_DRIVER_DLL_API void received(ErlDrvData drv_data, ErlIOVec *ev)
{
   //try
   //{
   //   using namespace ei_cxx;
   //   for(size_t i = 1; i < ev->vsize; ++i)
   //   {
	//      IBinary bin(ev->binv[i]);
	//      if (bin.get_type() == IBinary::ErlSmallTuple || bin.get_type() == IBinary::ErlLargeTuple)
	//      {
	//         ITuple tuple;
	//         bin >> tuple;
	//         Atom command_name;
	//         tuple >> command_name;
	//         if (command_name.get() == "disconnect")
	//         {
	//            process_disconnect();
	//         }
	//         else if (command_name.get() == "connect")
	//         {
	//            process_connect(tuple);
	//         }
	//         else if (command_name.get() == "log_level")
	//         {
   //            Atom llevel;
   //            tuple >> llevel;
   //            log_level = LogLevel::fromString(llevel.get());
	//         }
	//         else
	//         {
	//            THROW(std::runtime_error, "Unknown command");
	//         }
	//      }
	//      else
	//      {
	//         THROW(std::runtime_error, "Unknown command");
	//      }
   //   }
   //}
   //catch(std::exception const& err)
   //{
   //   sendError(err.what());
   //}
   //catch(_com_error const& err)
   //{
   //   LOG_ERROR(g_port,
   //      "received(): COM error. Error=" << err.Error() << ", Message=" << err.ErrorMessage());
   //}
}

//// {connect, iniFile, "host", port, "app_name", "password", log_level, [{FUT_TRADE_REPL, iniFile, StreamType}]}.
//void process_connect(ei_cxx::ITuple& t)
//{
//   using namespace ei_cxx;
//   std::string iniFile;
//   t >> iniFile;
//   Application::instance()->init(iniFile);

//   std::string host;
//   unsigned long port;
//   std::string appName;
//   std::string password;
//   Atom llevel;
//   t >> host >> port >> appName >> password >> llevel;
//   log_level = LogLevel::fromString(llevel.get());

//   Connection::instance()->connect(host, port, appName, password);

//   IList streams;
//   t >> streams;
//   size_t len = streams.size();
//   for(size_t i = 0; i < len; ++i)
//   {
//      ITuple stream;
//      streams >> stream;
//      std::string streamName;
//      std::string iniFile;
//      Atom streamType;
//      stream >> streamName >> iniFile >> streamType;
//      Connection::instance()->addStream(streamName, iniFile, StreamType::fromString(streamType.get()));
//   }
//}

//// {disconnect}
//void process_disconnect()
//{
//   Connection::instance()->disconnect();
//}

MICEX_DRIVER_DLL_API ErlDrvEntry micex_driver_entry =
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
   "micex_driver",    /* char *driver_name, the argument to open_port */
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

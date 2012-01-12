/// @file   micex_driver_dll.cpp
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/04/2012 08:50:51 AM

#include "micex_application.h"
#include "micex_driver_dll.h"

#include <ei_cxx/tuple.h>
#include <ei_cxx/port.h>
#include <ei_cxx/atom.h>
#include <ei_cxx/list.h>

#include <boost/cstdint.hpp>

#include <ei.h>

ei_cxx::Port g_port;

class MicexApplication;

void process_connect(MicexApplication**, ei_cxx::ITuple&);
void process_disconnect(MicexApplication*);
void addTable(MicexApplication* app, ei_cxx::ITuple& table);

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
   MicexApplication* app;
};

MICEX_DRIVER_DLL_API ErlDrvData start(ErlDrvPort port, char *buff)
{
   PortData* d = (PortData*)driver_alloc(sizeof(PortData));
    d->port = port;
    if (!g_port)
    {
      g_port = port;
    }
    d->app = NULL;
    return (ErlDrvData)d;
}

MICEX_DRIVER_DLL_API void stop(ErlDrvData handle)
{
   PortData* d = (PortData*)handle;
   if (d->app)
   {
      d->app->close();
      delete d->app;
      d->app = NULL;
   }
   g_port = NULL;
}

//------------------------------------------------------------------------------------------------------------------------//
MICEX_DRIVER_DLL_API void received(ErlDrvData drv_data, ErlIOVec *ev)
{
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
            if (command_name.get() == "disconnect")
            {
               PortData* pd = (PortData*)drv_data;
               process_disconnect(pd->app);
               delete pd->app;
               pd->app = NULL;
            }
            else if (command_name.get() == "connect")
            {
               PortData* pd = (PortData*)drv_data;
               process_connect(&pd->app, tuple);
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
      ERL_LOG_ERROR(g_port, err.what());
   }
}

// {connect, LibraryFullName, LogLevel, ConnParams, Tables}.
//    LibraryFullName = String()
//    ConnParams = binary()
//    ParamName = String()
//    Value     = String()
//    LogLevel  = atom()
//    Tables    = [Table]
//    Table     = {TableName, CompleteLoad, RefreshEnabled, InValues, RequiredFields}
//    TableName = String()
//    CompleteLoad = boolean()
//    RefreshEnabled = boolean()
//    InValues  = [InValue]
//    InValues  = {FieldName, FieldVal]
//    FieldName = String()
//    FieldVal  = String()
//    RequiredFields = [ReqField]
//    ReqField  = String()
void process_connect(MicexApplication** app, ei_cxx::ITuple& t)
{
   using namespace ei_cxx;
   std::string libFullName;
   t >> libFullName;
   Atom logLevel;
   t >> logLevel;

   ei_cxx::bytes connParams;
   t >> connParams;
   *app = new MicexApplication(g_port, libFullName, LogLevel::fromString(logLevel.get()));

   IList tables;
   t >> tables;
   size_t sz = tables.size();
   for(size_t i = 0; i < sz; ++i)
   {
      ITuple table;
      tables >> table;
      addTable(*app, table);
   }
   (*app)->open(std::string((char*)&connParams[0], connParams.size()));
}

void addTable(MicexApplication* app, ei_cxx::ITuple& table)
{
   using namespace ei_cxx;
   using namespace mtesrl;

   std::string tblName;
   bool completeLoad;
   bool refreshEnabled;
   IList inValuesList;
   IList reqOutFieldsList;
   table >> tblName >> completeLoad >> refreshEnabled >> inValuesList >> reqOutFieldsList;
   InValues inValues;
   RequiredOutFields reqOutFields;
   size_t sz = inValuesList.size();
   for(size_t i = 0; i < sz; ++i)
   {
      ITuple item;
      inValuesList >> item;
      std::string name;
      std::string value;
      item >> name >> value;
      inValues.insert(std::make_pair(name, value));
   }
   sz = reqOutFieldsList.size();
   for(size_t i = 0; i < sz; ++i)
   {
      std::string item;
      reqOutFieldsList >> item;
      reqOutFields.push_back(item);
   }
   app->addTable(tblName, completeLoad, refreshEnabled, inValues, reqOutFields);
}

// {disconnect}
void process_disconnect(MicexApplication* app)
{
   app->close();
}

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

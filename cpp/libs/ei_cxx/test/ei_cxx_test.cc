/// @file   ei_cxx_port.cc
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 04/09/2011 10:12:02 PM

#include "../include/ei_cxx/ibinary.h"
#include "../include/ei_cxx/tuple.h"
#include "../include/ei_cxx/port.h"
#include "../include/ei_cxx/atom.h"
#include "../include//ei_cxx/list.h"

#include <erl_driver.h>

#include <iostream>
#include <list>
#include <string>

using namespace ei_cxx;

Port  g_port;

struct PortData
{
	ErlDrvPort port;
};

void test1(ErlDrvBinary*);
void test2(ErlDrvBinary*);
void test3(ErlDrvBinary*);
void test4(ErlDrvBinary*);
void test5(ErlDrvBinary*);
void test6(ErlDrvBinary*);
void test7(ErlDrvBinary*);
void test8(ErlDrvBinary*);
void test9(ErlDrvBinary*);
void test10(ErlDrvBinary*);
void test11(ErlDrvBinary*);
void test12(ErlDrvBinary*);
void test13(ErlDrvBinary*);
void test14(ErlDrvBinary*);

//------------------------------------------------------------------------------------------------------------------------//
ErlDrvData start(ErlDrvPort port, char *buff)
{
   PortData* d = (PortData*)driver_alloc(sizeof(PortData));
   d->port = port;
   if (!g_port)
   {
      g_port = port;
   }
   return (ErlDrvData)d;
}

//------------------------------------------------------------------------------------------------------------------------//
void stop(ErlDrvData handle)
{
   driver_free((char*)handle);
   g_port = NULL;
}

//------------------------------------------------------------------------------------------------------------------------//
void outputv(ErlDrvData drv_data, ErlIOVec *ev)
{
   try
   {
      using namespace ei_cxx;
      for(size_t i = 1; i < ev->vsize; ++i)
      {
         int8_t test = *(int8_t*)ev->binv[i]->orig_bytes;
         ErlDrvBinary* binv = driver_alloc_binary(ev->binv[i]->orig_size - sizeof(int8_t));
         memcpy(binv->orig_bytes, ev->binv[i]->orig_bytes + sizeof(int8_t), binv->orig_size);
         switch (test)
         {
            case 1: test1(binv); break;
            case 2: test2(binv); break;
            case 3: test3(binv); break;
            case 4: test4(binv); break;
            case 5: test5(binv); break;
            case 6: test6(binv); break;
            case 7: test7(binv); break;
            case 8: test8(binv); break;
            case 9: test9(binv); break;
            case 10: test10(binv); break;
            case 11: test11(binv); break;
            case 12: test12(binv); break;
            case 13: test13(binv); break;
            case 14: test14(binv); break;
         }
         driver_free_binary(binv);
      }
   }
   catch(std::exception const& err)
   {
      OTuple output(2);
      output << Atom("error") << std::string(err.what());
      output.send(g_port);
   }
}

//------------------------------------------------------------------------------------------------------------------------//
ErlDrvEntry ei_cxx_test_entry =
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
   "libei_cxx_test",   /* char *driver_name, the argument to open_port */
   NULL,               /* F_PTR finish, called when unloaded */
   NULL,               /* handle  */
   NULL,               /* F_PTR control, port_command callback */
   NULL,               /* F_PTR timeout, reserved */
   outputv             /* F_PTR outputv, reserved */
};

extern "C"
{

DRIVER_INIT(libei_cxx_test) /* must match name in driver_entry */
{
   return &ei_cxx_test_entry;
}

}

void test1(ErlDrvBinary* bin)
{
   IBinary b(bin);
   long val;
   b >> val;
   val += 100;
   OBinary reply;
   reply << val;
   reply.send(g_port);
}

void test2(ErlDrvBinary* bin)
{
   IBinary b(bin);
   std::string val;
   b >> val;
   val += "_REPLY";
   OBinary reply;
   reply << val;
   reply.send(g_port);
}

void test3(ErlDrvBinary* bin)
{
   IBinary b(bin);
   Atom val;
   b >> val;
   Atom rval(val.get() + "_REPLY");
   OBinary reply;
   reply << rval;
   reply.send(g_port);
}

void test4(ErlDrvBinary* bin)
{
   IBinary b(bin);
   double val;
   b >> val;
   val += 200.0;
   OBinary reply;
   reply << val;
   reply.send(g_port);
}

void test5(ErlDrvBinary* bin)
{
   IList b(bin);
   std::list<long> lst = b.to_std_list<long>();
   lst.push_back(100);
   lst.push_back(200);
   lst.push_back(300);
   OList reply = OList::from_std_list(lst);
   reply.send(g_port);
}

void test6(ErlDrvBinary* bin)
{
   IBinary b(bin);
   long l;
   b >> l;
   l -= 10000;
   OBinary reply;
   reply << l;
   reply.send(g_port);
}

void test7(ErlDrvBinary* bin)
{
   IList b(bin);
   OList reply(b.size() + 3);
   for(int i = 0; i < b.size(); ++i)
   {
      long val;
      b >> val;
      reply << val;
   }
   reply << 100L << 200L << 300L;
   reply.send(g_port);
}

void test8(ErlDrvBinary* bin)
{
   IList b(bin);
   OList reply(b.size());
   std::list<std::string> lst = b.to_std_list<std::string>();
   lst.reverse();
   for(std::list<std::string>::const_iterator it = lst.begin(); it != lst.end(); ++it)
   {
      reply << *it;
   }
   reply.send(g_port);
}

void test9(ErlDrvBinary* bin)
{
   IList in(bin);
   long a = 0;
   std::string b;
   double c = 0.0;
   Atom d;
   in >> a >> b >> c >> d;
   OList reply(4);
   reply << d << c << b << a;
   reply.send(g_port);
}

void test10(ErlDrvBinary* bin)
{
   IList in(bin);
   ITuple a, b, c;
   in >> a >> b >> c;
   OTuple reply(6);
   long i, ii;
   a >> i >> ii;
   reply << i << ii;
   std::string k, kk;
   b >> k >> kk;
   reply << k << kk;
   Atom q, qq;
   c >> q >> qq;
   reply << q << qq;
   reply.send(g_port);
}

void test11(ErlDrvBinary* bin)
{
   IBinary b(bin);
   bytes buff;
   b >> buff;
   OBinary reply;
   reply << buff;
   reply.send(g_port);
}

void test12(ErlDrvBinary* bin)
{
   IList in(bin);
   std::list<long> lst = in.to_std_list<long>();
   lst.reverse();
   OList reply = OList::from_std_list(lst);
   reply.send(g_port);
}

void test13(ErlDrvBinary* bin)
{
   IList in(bin);
   bytes a, b;
   in >> a >> b;
   OList reply(2);
   reply << b << a;
   reply.send(g_port);
}

void test14(ErlDrvBinary* bin)
{
   IBinary in(bin);
   bytes a;
   in >> a;
   OBinary reply;
   reply << a;
   reply.send(g_port);
}

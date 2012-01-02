/// @file   port.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 03/26/2011 01:46:31 PM

#ifndef EI_CXX_PORT_H
#define EI_CXX_PORT_H

#include <erl_driver.h>

#include <boost/thread/mutex.hpp>

#include <sstream>

namespace ei_cxx
{

/// @class port
/// @brief port wrapper
class Port
{
public:
   Port();
   Port(ErlDrvPort& port);
   bool operator ! ();
   Port& operator = (ErlDrvPort p);
   void send(char* buf, size_t len);
   void send(ErlDrvBinary* bin, size_t len);
private:
   typedef boost::mutex::scoped_lock MutexScopedLock;
   ErlDrvPort m_port;
   boost::mutex m_portLock;
};

} // namespace ei_cxx


#endif // EI_CXX_PORT_H

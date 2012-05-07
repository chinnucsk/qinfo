/// @file   Port.cc
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 03/26/2011 01:48:31 PM

#include "ei_cxx/port.h"
#include <qinfo_common/exception.h>

namespace ei_cxx
{

Port::Port() : m_port(NULL)
{
}

//------------------------------------------------------------------------------------------------------------------------//
Port::Port(ErlDrvPort& Port) : m_port(Port)
{
}

//------------------------------------------------------------------------------------------------------------------------//
bool Port::operator ! ()
{
   return m_port == NULL;
}

//------------------------------------------------------------------------------------------------------------------------//
Port& Port::operator = (ErlDrvPort p)
{
   m_port = p;
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
void Port::send(char* buf, size_t len)
{
   if (!m_port)
   {
      THROW(std::runtime_error, "m_Port == NULL");
   }
   MutexScopedLock lock(m_portLock);
   driver_output(m_port, &buf[0], len);
}

//------------------------------------------------------------------------------------------------------------------------//
void Port::send(ErlDrvBinary* bin, size_t len)
{
   if (!m_port)
   {
      THROW(std::runtime_error, "m_Port == NULL");
   }
   MutexScopedLock lock(m_portLock);
   driver_output_binary(m_port, NULL, 0, bin, 0, len);
}

} // namespace EiCxx

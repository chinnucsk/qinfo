/// @file   connection.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/03/2012 05:26:41 PM

#include "precomp.h"
#include "connection.h"

#include <mtesrl/public.h>

extern ei_cxx::Port g_port;

namespace micex
{

//---------------------------------------------------------------------------------------------------------------------//
Connection::Connection()
{
}

//---------------------------------------------------------------------------------------------------------------------//
Connection::~Connection()
{
   close();
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::open(std::string const& connParams)
{
   char err[MTE_ERRMSG_SIZE] = {};
   LOG_INFO(g_port, "Connecting <" << connParams << ">...");
   char params[MTE_CONNPARAMS_SIZE] = {};
   if (-1 == _snprintf_s(params, sizeof(params), sizeof(params) - 1, "%s", connParams.c_str()))
   {
      THROW(std::runtime_error, "Params too long");
   }
   m_connDescr = MTEConnect(params, err);
   if (m_connDescr < MTE_OK)
   {
      THROW(std::runtime_error, FMT("Connection error. Error=%1%, Description = %2%.", m_connDescr % err));
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::close()
{
   if (m_connDescr > 0)
   {
      MTEDisconnect(m_connDescr);
      m_connDescr = 0;
   }
}
//---------------------------------------------------------------------------------------------------------------------//
void Connection::addTable(TablePtr table)
{
   if (m_connDescr > 0)
   {
      THROW(std::runtime_error, "Close connection before table add");
   }
   for(Tables::const_iterator it = m_tables.begin(); it != m_tables.end(); ++it)
   {
      if ((*it)->name() == table->name())
      {
         THROW(std::runtime_error, FMT("Table %1% already added", table->name()));
      }
   }
   m_tables.push_back(table);
}

} // namespace micex

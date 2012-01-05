/// @file   connection.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/03/2012 05:26:41 PM

#include "precomp.h"
#include "connection.h"

#include <mtesrl/public.h>
#include <boost/bind.hpp>
#include <boost/algorithm/string/trim.hpp>

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
   if (!m_worker)
   {
      m_stop = false;
      m_worker.reset(new boost::thread(boost::bind(&Connection::run, this)));
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::close()
{
   m_stop = true;
   m_worker.join();
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::run()
{
   try
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

      initTables();

      while(!m_stop)
      {
         // TODO
      }

      if (m_connDescr > 0)
      {
         MTEDisconnect(m_connDescr);
         m_connDescr = 0;
      }
   }
   catch(std::exception const& err)
   {
      LOG_ERROR(g_port, err.what());
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::addTable(
         std::string const& name,
         bool completeLoad,
         bool refreshEnabled,
         std::string const& requiredFields = "");
{
   if (m_connDescr > 0)
   {
      THROW(std::runtime_error, "Close connection before table add");
   }
   for(Tables::const_iterator it = m_tables.begin(); it != m_tables.end(); ++it)
   {
      if ((*it)->name() == name)
      {
         THROW(std::runtime_error, FMT("Table %1% already added", name));
      }
   }
   m_tables.push_back(TablePtr(new Table(name, completeLoad, refreshEnabled, requiredFields)));
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::initTables()
{
   using boost::int32_t;

   MTEMsg* msg;
   int err = MTEStructure(m_connDescr, msg);
   if (err < 0)
   {
      THROW(
            std::runtime_error,
            FMT("Unable to obtain MTE structure. Error = %1%, Description = %2%",
               err % std::string(msg->data, msg->len)));
   }

   char const* buff = reinterpret_cast<char const*>(&msg->data);

   LOG_INFO(g_port, "Interface name:" << get_string(buff));
   LOG_INFO(g_port, "Interface description:" << get_string(buff));

   int32_t tablesNum = get_int32(buff);

   for(int32_t i = 0; i < tablesNum; ++i)
   {
      std::string const tableName = boost::algorithm::trim_copy(get_string(buff));
      bool found = false;
      for(Tables::iterator it = m_tables.begin(); it != m_tables.end(); ++it)
      {
         if ((*it)->name() == tableName)
         {
            (*it)->init(buff);
            found = true;
            LOG_INFO(g_port, FMT("Table %1% has been initialized."));
            break;
         }
         if (!found)
         {
            //skip table structure
            Table::skip(buff);
            LOG_INFO(g_port, FMT("Table %1% not found. Skipped."));
         }
      }
   }
}

} // namespace micex

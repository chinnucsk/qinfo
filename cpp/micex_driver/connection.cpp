/// @file   connection.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/03/2012 05:26:41 PM

#include "precomp.h"
#include "connection.h"
#include "connection_callback.h"

#include <mtesrl/public.h>
#include <boost/bind.hpp>
#include <boost/algorithm/string/trim.hpp>

extern ei_cxx::Port g_port;

namespace micex
{

namespace
{

void skip_enums(char const*& data)
{
   using namespace boost;
   int32_t numEnums = get_int32(data);
   for(int32_t i = 0; i < numEnums; ++i)
   {
      get_string(data); // name
      get_string(data); // description
      get_int32(data);  // size
      get_int32(data);  // type
      int32_t numVal = get_int32(data);
      for(int32_t j = 0; j < numVal; ++j)
      {
         get_string(data);
      }
   }
}

} // namespace

//---------------------------------------------------------------------------------------------------------------------//
Connection::Connection(std::string const& fileName, ConnectionCallback& cback, LogLevel::type_t llevel)
   : m_cback(cback), m_connDescr(-1), m_stop(false)
{
   MTELoadLibrary(fileName);
   setLogLevel(llevel);
}

//---------------------------------------------------------------------------------------------------------------------//
Connection::~Connection()
{
   close();
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::open(std::string const& connParams)
{
   if (!m_worker.get())
   {
      m_stop = false;
      m_worker.reset(new boost::thread(boost::bind(&Connection::run, this, connParams)));
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::close()
{
   m_stop = true;
   m_worker->join();
   m_worker.reset(NULL);
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::run(std::string const& connParams)
{
   boost::scoped_array<char> params(new char[connParams.length() + 1]);
   memcpy(params.get(), connParams.c_str(), connParams.length());
   params[connParams.length()] = '\0';
   char err[MTE_ERRMSG_SIZE] = {};

   while(!m_stop)
   {
      try
      {
         if (m_connDescr < MTE_OK)
         {
            m_connDescr = MTEConnect(params.get(), err);
            if (m_connDescr < MTE_OK)
            {
               throw MteError(m_connDescr,
                     FMT("Connection error. Error = %1%, Description = %2%", m_connDescr % err));
            }
            m_cback.onConnectionStatus(ConnectionStatus::Connected);
            initTables();
            openTables();
         }
         processTables();
      }
      catch(MteError const& err)
      {
         LOG_ERROR(g_port, err.what());
         if (err.error() != MTE_SRVUNAVAIL && err.error() != MTE_INVALIDCONNECT && err.error() == MTE_NOTCONNECTED &&
               err.error() != MTE_WRITE && err.error() != MTE_READ && err.error() != MTE_TSMR)
         {
            break;
         }
         else
         {
            closeMTEConnection();
            Sleep(1000);
         }
      }
      catch(std::exception const& err)
      {
         LOG_ERROR(g_port, err.what());
         break;
      }
   }
   closeTables();
   closeMTEConnection();
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::closeMTEConnection()
{
   if (m_connDescr > 0)
   {
      MTEDisconnect(m_connDescr);
      m_connDescr = -1;
      m_cback.onConnectionStatus(ConnectionStatus::Disconnected);
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::processTables()
{
   bool added = false;
   for(Tables::iterator it = m_tables.begin(); it != m_tables.end(); ++it)
   {
      if ((*it)->refreshEnabled())
      {
         boost::int32_t err = MTEAddTable(m_connDescr, (*it)->descriptor(), (*it)->ref());
         if (err < MTE_OK)
         {
            throw MteError(err, FMT("Unable to add table %1%. Error = %2%", (*it)->name() % err));
         }
         added = true;
      }
   }
   if (!added)
   {
      LOG_INFO(g_port, "Nothing to refresh. Will be stopped.");
      m_stop = true;
   }
   else
   {
      refresh();
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::addTable(
         std::string const& name,
         bool completeLoad,
         bool refreshEnabled,
         InValues const& inValues,
         RequiredOutFields const& reqOutFields)
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
   m_tables.push_back(TablePtr(new Table(name, m_cback, completeLoad, refreshEnabled, inValues, reqOutFields)));
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::initTables()
{
   using boost::int32_t;

   MTEMsg* msg;
   int err = MTEStructure(m_connDescr, &msg);
   if (err < 0)
   {
      THROW(
            std::runtime_error,
            FMT("Unable to obtain MTE structure. Error = %1%, Description = %2%",
               err % std::string(msg->data, msg->len)));
   }

   char const* data = reinterpret_cast<char const*>(&msg->data);

   LOG_INFO(g_port, "Interface name:" << get_string(data));
   LOG_INFO(g_port, "Interface description:" << get_string(data));

   skip_enums(data);

   int32_t tablesNum = get_int32(data);

   for(int32_t i = 0; i < tablesNum; ++i)
   {
      std::string const tableName = boost::algorithm::trim_copy(get_string(data));
      bool found = false;
      for(Tables::iterator it = m_tables.begin(); it != m_tables.end(); ++it)
      {
         if ((*it)->name() == tableName)
         {
            (*it)->init(data);
            found = true;
            LOG_INFO(g_port, FMT("Table %1% has been initialized.", tableName));
            break;
         }
      }
      if (!found)
      {
         //skip table structure
         Table::skip(data);
         LOG_INFO(g_port, FMT("Table %1% not found. Skipped.", tableName));
      }
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::openTables()
{
   for(Tables::iterator it = m_tables.begin(); it != m_tables.end(); ++it)
   {
      (*it)->open(m_connDescr);
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::closeTables()
{
   for(Tables::iterator it = m_tables.begin(); it != m_tables.end(); ++it)
   {
      (*it)->close(m_connDescr);
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void Connection::refresh()
{
   using namespace boost;
   MTEMsg* msg;
   int32_t err = MTERefresh(m_connDescr, &msg);
   if (err < MTE_OK)
   {
      std::string descr;
      if (err == MTE_TSMR)
      {
         descr = std::string(reinterpret_cast<char const*>(&msg->data), msg->len);
      }
      throw MteError(err, FMT("Unable to refresh tables. Error = %1%, Description = %2%", err % descr));
   }
   MTETables* tables = reinterpret_cast<MTETables*>(&msg->data);
   if (tables->numTables > 0)
   {
      char const* data = reinterpret_cast<char const*>(&tables->tables);
      for(int32_t i = 0; i < tables->numTables; ++i)
      {
         MTETable const* table = reinterpret_cast<MTETable const*>(data);
         LOG_DEBUG(g_port, FMT("Table row = %1%, ref = %2%", table->numRows % table->ref));
         bool found = false;
         for(Tables::iterator it = m_tables.begin(); it != m_tables.end(); ++it)
         {
            if ((*it)->ref() == table->ref)
            {
               (*it)->parse(data);
               found = true;
               break;
            }
         }
         if (!found)
         {
            LOG_ERROR(g_port, FMT("Table with ref %1% not found. Skipped.", table->ref));
         }
         data += table->size();
      }
   }
}

} // namespace micex

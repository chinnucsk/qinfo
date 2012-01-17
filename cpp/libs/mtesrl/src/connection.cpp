/// @file   connection.cpp
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/03/2012 05:26:41 PM

#include "../include/mtesrl/precomp.h"
#include "../include/mtesrl/mtesrl_lib.h"
#include "../include/mtesrl/connection.h"
#include "../include/mtesrl/connection_callback.h"

#include <common/log_wrapper.h>
#include <common/exception.h>

#include <boost/bind.hpp>
#include <boost/algorithm/string/trim.hpp>

namespace mtesrl
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
Connection::Connection(std::string const& libPath, ConnectionCallback& cback, LogLevel::type_t llevel)
   : m_libPath(libPath), m_cback(cback), m_connDescr(-1), m_stop(false)
{
   log_level = llevel;
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
   if (m_worker.get())
   {
      m_worker->join();
      m_worker.reset(NULL);
   }
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
            MTELoadLibrary(m_libPath);
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
         MTESRL_LOG_ERROR(m_cback, err.what());
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
         MTESRL_LOG_ERROR(m_cback, err.what());
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
      MTESRL_LOG_INFO(m_cback, "Nothing to refresh. Will be stopped.");
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

   MTESRL_LOG_INFO(m_cback, "Interface name:" << get_string(data));
   MTESRL_LOG_INFO(m_cback, "Interface description:" << get_string(data));

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
            MTESRL_LOG_INFO(m_cback, FMT("Table %1% has been initialized.", tableName));
            break;
         }
      }
      if (!found)
      {
         //skip table structure
         Table::skip(data);
         MTESRL_LOG_INFO(m_cback, FMT("Table %1% not found. Skipped.", tableName));
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
         MTESRL_LOG_DEBUG(m_cback, FMT("Table row = %1%, ref = %2%", table->numRows % table->ref));
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
            MTESRL_LOG_ERROR(m_cback, FMT("Table with ref %1% not found. Skipped.", table->ref));
         }
         data += table->size();
      }
   }
}

} // namespace mtesrl

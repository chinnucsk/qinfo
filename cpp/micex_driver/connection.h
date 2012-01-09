/// @file   connection.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/03/2012 05:11:01 PM

#ifndef MICEX_CONNECTION_H
#define MICEX_CONNECTION_H

#include "table.h"

#include <boost/thread.hpp>

#include <list>
#include <string>

namespace micex
{

class ConnectionCallback;

class Connection
{
public:
   Connection(ConnectionCallback& cback, LogLevel::type_t llevel);
   ~Connection();
   void open(std::string const& connParams);
   void close();
   void addTable(
         std::string const& name,
         bool completeLoad,
         bool refreshEnabled,
         InValues const& inValues = InValues());
private:
   void run(std::string const& connParams);
   void initTables();
   void refresh();
   void openTables();
   void closeTables();
   void closeMTEConnection();
   void processTables();
private:
   typedef std::list<TablePtr> Tables;
   ConnectionCallback&           m_cback;
   long                          m_connDescr;
   Tables                        m_tables;
   std::auto_ptr<boost::thread>  m_worker;
   bool                          m_stop;
};

} // namespace micex

#endif // MICEX_CONNECTION_H

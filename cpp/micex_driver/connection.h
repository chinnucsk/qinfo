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

class Connection
{
public:
   Connection();
   ~Connection();
   void open(std::string const& connParams);
   void close();
   void addTable(
         std::string const& name,
         bool completeLoad,
         bool refreshEnabled,
         std::string const& requiredFields = "");
private:
   void initTables();
private:
   typedef std::list<TablePtr> Tables;
   long                          m_connDescr;
   Tables                        m_tables;
   std::auto_ptr<boost::thread>  m_worker;
};

} // namespace micex

#endif // MICEX_CONNECTION_H

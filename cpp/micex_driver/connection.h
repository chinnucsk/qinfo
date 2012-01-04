/// @file   connection.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/03/2012 05:11:01 PM

#ifndef MICEX_CONNECTION_H
#define MICEX_CONNECTION_H

#include "table.h"

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
   void addTable(TablePtr table);
private:
   typedef std::list<TablePtr> Tables;
   long m_connDescr;
   Tables m_tables;
};

} // namespace micex

#endif // MICEX_CONNECTION_H

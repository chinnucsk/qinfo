/// @file   connection.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/03/2012 05:11:01 PM

#ifndef MTESRL_CONNECTION_H
#define MTESRL_CONNECTION_H

#include <string>

namespace mtesrl
{

class Connection
{
public:
   Connection();
   ~Connection();
   void open(std::string const& connParams);
   void close();
   void addTable(Table& table);
private:
   typedef std::list<TablePtr> Tables;
   long m_connDescr;
   Tables m_tables;
};

};

#endif // MTESRL_CONNECTION_H

/// @file   table.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/04/2012 08:35:49 AM

#ifndef MTESRL_DRIVER_TABLE_H
#define MTESRL_DRIVER_TABLE_H

#include <string>

namespace mtesrl
{

class Table
{
public:
   Table(std::string const& name, bool completeLoad, bool refreshEnabled);
   void init(char const* buff);
private:
   std::string const m_name;
   bool const m_completeLoad;
   bool const m_refreshEnabled;
   long       m_ref;
   static long m_cookie;
};

typedef boost::smart_ptr<Table> TablePtr;

} // namespace


#endif // MTESRL_DRIVER_TABLE_H

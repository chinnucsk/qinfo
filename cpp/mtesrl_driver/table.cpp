/// @file   table.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/04/2012 08:38:11 AM

#include "table.h"

namespace mtesrl
{

long Table::m_cookie = 0;

Table::Table(std::string const& name, bool completeLoad, bool refreshEnabled)
   : m_name(name), m_completeLoad(completeLoad), m_refreshEnabled(refreshEnabled), m_ref(++m_cookie)
{
}

//---------------------------------------------------------------------------------------------------------------------//
void Table::init(char const* buff)
{
}

} // namespace mtesrl

/// @file   table.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/04/2012 08:38:11 AM

#include "table.h"

namespace micex
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

//---------------------------------------------------------------------------------------------------------------------//
void Table::skip(char const*& buff)
{
   using namespace boost;
   get_string(buff); // decription;
   get_int32(buff); // attributes;
   int32_t const inFields = get_int32(buff);
   for(int32_t i = 0; i < inFields; ++i)
   {
      InField::skip(buff);
   }
   int32_t const outFields = get_int32(buff);
   for(int32_t i = 0; i < outFields; ++i)
   {
      OutField::skip(buff);
   }
}

} // namespace micex

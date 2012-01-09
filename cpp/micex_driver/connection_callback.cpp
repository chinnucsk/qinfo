/// @file   connection_callback.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/09/2012 09:00:34 AM

#include "connection_callback.h"

namespace micex
{

OutFieldPtr OutRow::getField(std::string const& fieldName) const
{
   OutFields::const_iterator it = m_fields.find(fieldName);
   if (it == m_fields.end())
   {
      return OutFields();
   }
   return it->second;
}

//---------------------------------------------------------------------------------------------------------------------//
OutFieldPtr OutRow::first()
{
   if (m_fields.empty())
   {
      return OutFieldPtr();
   }
   m_cursor = m_fields.begin();
   return m_cursor->second;
}

//---------------------------------------------------------------------------------------------------------------------//
OutFieldPtr OutRow::next()
{
   if (++m_cursor == m_fields.end())
   {
      return OutFieldPtr();
   }
   return m_cursor->second;
}

//---------------------------------------------------------------------------------------------------------------------//
void OutRow::addField(OutFieldPtr field)
{
   m_fields.insert(std::make_pair(field->name(), field));
}


} // namespace micex

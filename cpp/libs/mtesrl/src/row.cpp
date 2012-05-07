/// @file   row.cpp
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/10/2012 10:40:28 PM

#include "mtesrl/row.h"

namespace mtesrl
{

//---------------------------------------------------------------------------------------------------------------------//
Row::Row() : m_cursor(m_fields.end())
{
}

//---------------------------------------------------------------------------------------------------------------------//
OutFieldPtr Row::getField(std::string const& fieldName) const
{
   Index::const_iterator it = m_index.find(fieldName);
   if (it == m_index.end())
   {
      return OutFieldPtr();
   }
   return it->second;
}

//---------------------------------------------------------------------------------------------------------------------//
OutFieldPtr Row::first() const
{
   if (m_fields.empty())
   {
      return OutFieldPtr();
   }
   m_cursor = m_fields.begin();
   return *m_cursor;
}

//---------------------------------------------------------------------------------------------------------------------//
OutFieldPtr Row::next() const
{
   if (++m_cursor == m_fields.end())
   {
      return OutFieldPtr();
   }
   return *m_cursor;
}

//---------------------------------------------------------------------------------------------------------------------//
void Row::addField(OutFieldPtr field)
{
   m_fields.push_back(field);
   m_index.insert(std::make_pair(field->name(), field));
}

//------------------------------------------------------------------------------------------------------------------------//
void Row::reset()
{
   for(OutFields::iterator it = m_fields.begin(); it != m_fields.end(); ++it)
   {
      (*it)->reset();
   }
}

} // namespace mtesrl

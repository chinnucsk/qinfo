/// @file   connection_callback.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/09/2012 09:00:34 AM

#include "connection_callback.h"

#include <boost/lambda/lambda.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/lexical_cast.hpp>

#include <locale>

namespace micex
{

namespace
{

std::locale ruLocale("Russian");

boost::int64_t pow10(int p)
{
   boost::int64_t arr[13] = 
   {
      1,
      10,
      100,
      1000,
      10000,
      100000,
      1000000,
      10000000,
      100000000,
      1000000000,
      10000000000,
      100000000000,
      1000000000000
   };
   return arr[p];
}

} // namespace

//---------------------------------------------------------------------------------------------------------------------//
boost::optional<boost::int64_t> OutRow::getAsInt64(std::string const& fieldName) const
{
   Fields::const_iterator it = m_fields.find(fieldName);
   if (it == m_fields.end())
   {
      return boost::none;
   }
   std::string tmp = boost::trim_left_copy_if(it->second, boost::lambda::_1 == '0');
   if (tmp.empty())
   {
      tmp = "0";
   }
   return boost::optional<boost::int64_t>(boost::lexical_cast<boost::int64_t>(tmp));
}

//---------------------------------------------------------------------------------------------------------------------//
boost::optional<float> OutRow::getAsFloat(std::string const& fieldName, unsigned int precision) const
{
   Fields::const_iterator it = m_fields.find(fieldName);
   if (it == m_fields.end())
   {
      return boost::none;
   }
   float const tmp = boost::lexical_cast<float>(boost::trim_left_copy_if(it->second, boost::lambda::_1 == '0'));
   return boost::optional<float>(tmp / pow10(precision));
}

//---------------------------------------------------------------------------------------------------------------------//
boost::optional<std::string> OutRow::getAsString(std::string const& fieldName) const
{
   Fields::const_iterator it = m_fields.find(fieldName);
   if (it == m_fields.end())
   {
      return boost::none;
   }
   return boost::trim_copy(it->second, ruLocale);
}

//---------------------------------------------------------------------------------------------------------------------//
void OutRow::addField(std::string const& name, std::string const& value)
{
   m_fields.insert(std::make_pair(name, value));
}

} // namespace micex

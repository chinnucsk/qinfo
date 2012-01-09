/// @file   field.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/05/2012 08:51:45 AM

#include "field.h"

#include <boost/algorithm/string/erase.hpp>

#include <sstream>

namespace micex
{

namespace
{

std::string string2micex(std::string val, boost::int32_t size)
{
   for(boost::int32_t i = val.length(); i < size; ++i)
   {
      val += ' ';
   }
   return val;
}

std::string int2micex(std::string const& val, boost::int32_t size)
{
   std::ostringstream ost;
   ost << std::setw(size) << std::setfill('0') << val;
   if (ost.str().length() > size)
   {
      THROW(std::runtime_error, FMT("Value '%1%' has a length greater than %2%", val % size));
   }
   return ost.str();
}

std::string float2micex(std::string const& val, boost::int32_t size, unsigned int precision)
{
   std::ostringstream ost;
   ost << std::setw(size + 1)
       << std::setfill('0')
       << std::fixed
       << std::setprecision(precision)
       << boost::lexical_cast<float>(val);
   std::string tmp = ost.str();
   boost::erase_all(tmp, ".");
   return tmp;
}

std::string value2micex(std::string const& val, int type, boost::int32_t size, unsigned int precision)
{
   if (type == Field::charType || type == Field::dateType || type == Field::timeType)
   {
      return string2micex(val, size);
   }
   else if (type == Field::intType)
   {
      return int2micex(val, size);
   }
   else if (type == Field::floatType)
   {
      return float2micex(val, size, precision);
   }
   else if (type == Field::fixedType)
   {
      return float2micex(val, size, FixedPrec);
   }
   else
   {
      THROW(std::runtime_error, FMT("Unknown type %1%", type));
   }
   return "";
}

} // namespace

//---------------------------------------------------------------------------------------------------------------------//
Field::Field(char const*& data)
{
   m_name = get_string(data);
   get_string(data); // skip description
   m_size = get_int32(data);
   m_type = get_int32(data);
   m_flags = get_int32(data);
   get_string(data); // skip enums
}

//---------------------------------------------------------------------------------------------------------------------//
void Field::skip(char const*& data)
{
   get_string(data); // name
   get_string(data); // description
   get_int32(data);  // size
   get_int32(data);  // type
   get_int32(data);  // attribute
   get_string(data); // enum
}

//---------------------------------------------------------------------------------------------------------------------//
InField::InField(char const*& data, InValues const& inValues, unsigned int precision) : Field(data)
{
   std::string defVal = get_string(data);
   InValues::const_iterator it = inValues.find(name());
   if (it != inValues.end())
   {
      defVal = it->second;
   }
   m_value = value2micex(defVal, type(), size(), precision);
}

//---------------------------------------------------------------------------------------------------------------------//
void InField::skip(char const*& data)
{
   Field::skip(data);
   get_string(data); // default value
}

//---------------------------------------------------------------------------------------------------------------------//
OutField::OutField(char const*& data) : Field(data)
{
}

//---------------------------------------------------------------------------------------------------------------------//
void OutField::skip(char const*& data)
{
   Field::skip(data);
}

//---------------------------------------------------------------------------------------------------------------------/
std::string OutField::parse(char const*& data)
{
   std::string val(data, size());
   data += size();
   return val;
}
} // namespace micex

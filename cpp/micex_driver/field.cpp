/// @file   field.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/05/2012 08:51:45 AM

#include "field.h"

#include <boost/algorithm/string/erase.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/lexical_cast.hpp>

#include <locale>
#include <sstream>

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
   if (type == FieldType::charType || type == FieldType::dateType || type == FieldType::timeType)
   {
      return string2micex(val, size);
   }
   else if (type == FieldType::intType)
   {
      return int2micex(val, size);
   }
   else if (type == FieldType::floatType)
   {
      return float2micex(val, size, precision);
   }
   else if (type == FieldType::fixedType)
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
void OutField::parse(char const*& data)
{
   m_value = std::string(data, size());
   data += size();
}

//---------------------------------------------------------------------------------------------------------------------//
boost::optional<boost::int64_t> OutField::getAsInt64() const
{
   if (!m_value)
   {
      return boost::none;
   }
   std::string tmp = boost::trim_left_copy_if(m_value, boost::lambda::_1 == '0');
   if (tmp.empty())
   {
      tmp = "0";
   }
   return boost::optional<boost::int64_t>(boost::lexical_cast<boost::int64_t>(tmp));
}

//---------------------------------------------------------------------------------------------------------------------//
boost::optional<float> OutField::getAsFloat(unsigned int precision) const
{
   if (!m_value)
   {
      return boost::none;
   }
   float const tmp = boost::lexical_cast<float>(boost::trim_left_copy_if(m_value, boost::lambda::_1 == '0'));
   return boost::optional<float>(tmp / pow10(precision));
}

//---------------------------------------------------------------------------------------------------------------------//
boost::optional<std::string> OutField::getAsString() const
{
   if (!m_value)
   {
      return boost::none;
   }
   return boost::trim_copy(*m_value, ruLocale);
}

//------------------------------------------------------------------------------------------------------------------------//
void OutField::reset()
{
   m_value = boost::none;
}

} // namespace micex

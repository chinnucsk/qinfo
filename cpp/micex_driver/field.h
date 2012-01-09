/// @file   field.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/05/2012 08:48:44 AM

#ifndef MICEX_DRIVER_FIELD_H
#define MICEX_DRIVER_FIELD_H

#include "precom.h"

#include <boost/shared_ptr.hpp>
#include <boost/cstdint.hpp>
#include <boost/optional.hpp>

#include <string>

namespace micex
{

namespace
{

unsigned int const FixedPrec = 2;

} // namespace

class Field
{
public:
   static int const charType  = 0;
   static int const intType   = 1;
   static int const fixedType = 2;
   static int const floatType = 3;
   static int const dateType  = 4;
   static int const timeType  = 5;
public:
   Field(char const*& data);
   static void skip(char const*& data);
   std::string const& name() const { return m_name; }
   boost::int32_t size() const { return m_size; }
   int type() const { return m_type; }
private:
   std::string    m_name;
   boost::int32_t m_size;
   int            m_type;
   boost::int32_t m_flags;
};

class InField : public Field
{
public:
   InField(char const*& data, InValues const& inValues, unsigned int precision = FixedPrec);
   static void skip(char const*& data);
   std::string const& value() const { return m_value; }
private:
   std::string m_value;
};

class OutField : public Field
{
public:
   OutField(char const*& data);
   static void skip(char const*& data);
   std::string parse(char const*& data);
};

typedef boost::shared_ptr<InField> InFieldPtr;
typedef boost::shared_ptr<OutField> OutFieldPtr;

} // namespace micex

#endif // MICEX_DRIVER_FIELD_H

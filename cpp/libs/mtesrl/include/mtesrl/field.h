/// @file   field.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/05/2012 08:48:44 AM

#ifndef MTESRL_FIELD_H
#define MTESRL_FIELD_H

#include <qinfo_common/smart_enum.h>
#include <boost/shared_ptr.hpp>
#include <boost/cstdint.hpp>
#include <boost/optional.hpp>

#include <string>
#include <map>

namespace mtesrl
{

namespace
{

unsigned int const FixedPrec = 2;

} // namespace

typedef std::map<std::string, std::string> InValues;

DECLARE_ENUM
(
   FieldType, int,
   ENTRY(charType,   0)
   ENTRY(intType,    1)
   ENTRY(fixedType,  2)
   ENTRY(floatType,  3)
   ENTRY(dateType,   4)
   ENTRY(timeType,   5)
)

class Field
{
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
   explicit OutField(char const*& data);
public:
   boost::optional<boost::int64_t> getAsInt64() const;
   boost::optional<float> getAsFloat(unsigned int precision) const;
   boost::optional<std::string> getAsString() const;
   static void skip(char const*& data);
   void parse(char const*& data);
   void reset();
private:
   boost::optional<std::string> m_value;
};

typedef boost::shared_ptr<InField> InFieldPtr;
typedef boost::shared_ptr<OutField> OutFieldPtr;

} // namespace mtesrl

#endif // MTESRL_FIELD_H

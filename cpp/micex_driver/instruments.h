/// @file   instruments.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/12/2012 09:31:58 AM


#ifndef MICEX_DRIVER_INSTRUMENT_H
#define MICEX_DRIVER_INSTRUMENT_H

#include <mtesrl/field.h>
#include <mtesrl/row.h>
#include <mtesrl/table.h>

#include <common/exception.h>

#include <boost/any.hpp>
#include <boost/shared_ptr.hpp>

#include <list>
#include <map>
#include <string>

namespace ei_cxx
{
class Port;
} // namespace ei_cxx

class Instrument
{
public:
   Instrument(ei_cxx::Port& port, mtesrl::RequiredOutFields const& reqOutFields);
   void onTableData(mtesrl::Row const& row);
   void reset();
   template<typename T>
   void updateField(std::string const& fieldName, mtesrl::FieldType::type_t type, boost::optional<T> const& val)
   {
      if (val)
      {
         Index::iterator it = m_index.find(fieldName);
         if (it == m_index.end())
         {
            THROW(
                  std::runtime_error,
                  FMT("Field %1% not found among required fields in SECURITIES table", fieldName))
         }
         it->second->type = type;
         it->second->value = val;
      }
   }
   void sendRow();
   unsigned int decimals() const { return m_decimals; }
private:
   struct Field
   {
      mtesrl::FieldType::type_t type;
      boost::any value;
   };
   typedef std::list<Field> Fields;
   typedef std::map<std::string, Field*> Index;
private:
   ei_cxx::Port&  m_port;
   unsigned int   m_decimals;
   Fields         m_fields;
   Index          m_index;
};

typedef boost::shared_ptr<Instrument> InstrumentPtr;

class Instruments
{
public:
   Instruments(ei_cxx::Port& port, mtesrl::RequiredOutFields const& reqOutFields);
   void onTableData(mtesrl::Row const& row);
   void reset();
   unsigned int getDecimals(mtesrl::Row const& row);
private:
   typedef std::map<std::string, InstrumentPtr> Container;
   ei_cxx::Port&              m_port;
   mtesrl::RequiredOutFields  m_reqOutFields;
   Container                  m_instruments;
};

typedef boost::shared_ptr<Instruments> InstrumentsPtr;

#endif // MICEX_DRIVER_INSTRUMENT_H

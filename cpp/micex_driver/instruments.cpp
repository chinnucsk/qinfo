/// @file   instruments.cpp
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/12/2012 09:44:33 AM

#include "instruments.h"

#include <ei_cxx/port.h>
#include <ei_cxx/atom.h>
#include <ei_cxx/tuple.h>

#include <boost/cstdint.hpp>

Instrument::Instrument(ei_cxx:Port& port, mtesrl::RequireqOutFields const& reqOutFields)
   :   m_port(port), m_decimals(mtesrl::FixedPrec)
{
   for(mtesrl::RequireqOutFields::const_iterator it = reqOutFields.begin(); it != reqOutFields.end(); ++it)
   {
     m_fields.push_back(Field());
     m_index.insert(std::make_pair(*it, &(*m_fields.rbegin())));
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void Instrument::onTableData(mtesrl::Row const& row)
{
   using namespace mtesrl;

   OutFieldPtr fld = row.first();
   while(fld)
   {
      if (fld->type() == FieldType::charType || fld->type() == FieldType::timeType || fld->type() == FieldType::dateType)
      {
         updateField(fld->name(), fld->type(), fld->getAsString());
      }
      else if (fld->type() == FieldType::intType)
      {
         updateField(fld->name(), fld->type(), fld->getAsInt64());
      }
      else if (fld->type() == FieldType::fixedType)
      {
         updateField(fld->name(), fld->type(), fld->getAsFLoat(mtesrl::FixedPrec));
      }
      else if (fld->type() == FieldType::floatType)
      {
         updateField(fld->name(), fld->type(), fld->getAsFLoat(m_decimals));
      }
      fld = row.next();
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void Instrument::reset()
{
   m_decimals = mtesrl::FixedPrec;
   for(Fields::iterator it = m_fields.begin(); it != m_fields.end(); ++it)
   {
      it->value = boost::any();
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void Instrument::sendRow()
{
   using namespace ei_cxx;
   using namespace mtesrl;

   OTuple erlRow(m_fields.size() + 2);
   erlRow << Atom("data_row") << Atom("SECURITIES");
   for(Fields::iterator it = m_fields.begin(); it != m_fields.end(); ++it)
   {
      Field const& fld = *it;
      if (field.value.empty())
      {
         erlRow << Atom("undef");
      }
      else if (field.type == FieldType::charType || field.type == FieldType::timeType || field.type == FieldType::dateType)
      {
         erlRow << boost::any_cast<std::string>(fld.value);
      }
      else if (field.type == FieldType::intType)
      {
         erlRow << boost::any_cast<boost::int64_t>(fld.value);
      }
      else if (field.type == FieldType::fixedType || field.type == FieldType::floatType)
      {
         erlRow << boost::any_cast<float>(fld.value);
      }
   }
   erlRow.send(m_port);
}

//---------------------------------------------------------------------------------------------------------------------//
Instruments::Instruments(ei_cxx::Port& port, mtesrl::RequireqOutFields const& reqOutFields)
   :   m_port(port), m_reqOutFields(reqOutFields)
{
}

//---------------------------------------------------------------------------------------------------------------------//
Instruments::reset()
{
   m_instruments.clear();
}

//---------------------------------------------------------------------------------------------------------------------//
Instruments::getDecimals(std::string const& key)
{
   Container::const_iterator it = m_instruments.find(key);
   if (it == m_instruments.end())
   {
      THROW(std:runtime_error, FMT("Decimals not found for key %1%", key));
   }
   return it->second->decimals();
}

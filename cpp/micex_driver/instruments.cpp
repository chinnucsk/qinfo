/// @file   instruments.cpp
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/12/2012 09:44:33 AM

#include "instruments.h"

#include <ei_cxx/port.h>
#include <ei_cxx/atom.h>
#include <ei_cxx/tuple.h>
#include <common/exception.h>

#include <boost/cstdint.hpp>

//---------------------------------------------------------------------------------------------------------------------//
Instrument::Instrument(ei_cxx::Port& port, mtesrl::RequiredOutFields const& reqOutFields)
   :   m_port(port), m_decimals(mtesrl::FixedPrec)
{
   for(mtesrl::RequiredOutFields::const_iterator it = reqOutFields.begin(); it != reqOutFields.end(); ++it)
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
         updateField(fld->name(), fld->type(), fld->getAsFloat(mtesrl::FixedPrec));
      }
      else if (fld->type() == FieldType::floatType)
      {
         updateField(fld->name(), fld->type(), fld->getAsFloat(m_decimals));
      }
      fld = row.next();
   }
   sendRow();
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
      Field const& field = *it;
      if (field.value.empty())
      {
         erlRow << Atom("undef");
      }
      else if (field.type == FieldType::charType || field.type == FieldType::timeType || field.type == FieldType::dateType)
      {
         erlRow << boost::any_cast<std::string>(field.value);
      }
      else if (field.type == FieldType::intType)
      {
         erlRow << boost::any_cast<boost::int64_t>(field.value);
      }
      else if (field.type == FieldType::fixedType || field.type == FieldType::floatType)
      {
         erlRow << boost::any_cast<float>(field.value);
      }
   }
   erlRow.send(m_port);
}

//---------------------------------------------------------------------------------------------------------------------//
Instruments::Instruments(ei_cxx::Port& port, mtesrl::RequiredOutFields const& reqOutFields)
   :   m_port(port), m_reqOutFields(reqOutFields)
{
}

//---------------------------------------------------------------------------------------------------------------------//
void Instruments::reset()
{
   m_instruments.clear();
}

//---------------------------------------------------------------------------------------------------------------------//
unsigned int Instruments::getDecimals(mtesrl::Row const& row)
{
   std::string const key = *row.getField("SECBOARD")->getAsString() + "_" +
      *row.getField("SECCODE")->getAsString();

   Container::const_iterator it = m_instruments.find(key);
   if (it == m_instruments.end())
   {
      THROW(std::runtime_error, FMT("Decimals not found for key %1%", key));
   }
   return it->second->decimals();
}

//---------------------------------------------------------------------------------------------------------------------/
void Instruments::onTableData(mtesrl::Row const& row)
{
   std::string const key = *row.getField("SECBOARD")->getAsString() + "_" +
      *row.getField("SECCODE")->getAsString();
   Container::iterator it = m_instruments.find(key);
   if (it == m_instruments.end())
   {
      it = m_instruments.insert(std::make_pair(key, new Instrument(m_port, m_reqOutFields))).first;
   }
   it->second->onTableData(row);
}

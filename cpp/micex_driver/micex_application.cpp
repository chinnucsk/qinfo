/// @file   micex_application.cpp
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/12/2012 06:42:26 PM

#include "micex_application.h"

#include <ei_cxx/port.h>
#include <ei_cxx/tuple.h>
#include <ei_cxx/atom.h>

//---------------------------------------------------------------------------------------------------------------------//
MicexApplication::MicexApplication(ei_cxx::Port& port, std::string const& fileName, LogLevel::type_t llevel)
   :   m_port(port), m_conn(fileName, *this, llevel)
{
}

//---------------------------------------------------------------------------------------------------------------------//
void MicexApplication::addTable(
      std::string const& name,
      bool completeLoad,
      bool refreshEnabled,
      mtesrl::InValues const& inValues,
      mtesrl::RequiredOutFields const& reqOutFields)
{
   m_conn.addTable(name, completeLoad, refreshEnabled, inValues, reqOutFields);
   if (name == "SECURITIES")
   {
      m_instruments = InstrumentsPtr(new Instruments(m_port, reqOutFields));
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void MicexApplication::open(std::string const& connParams)
{
   m_conn.open(connParams);
}

//---------------------------------------------------------------------------------------------------------------------//
void MicexApplication::close()
{
   m_conn.close();
   m_instruments.reset();
}

//---------------------------------------------------------------------------------------------------------------------//
void MicexApplication::onConnectionStatus(mtesrl::ConnectionStatus::type_t status)
{
   using namespace ei_cxx;
   OTuple t(2);
   t << Atom("connection_status") << mtesrl::ConnectionStatus::toString(status);
   t.send(m_port);
}

//---------------------------------------------------------------------------------------------------------------------//
void MicexApplication::onTableDataBegin(std::string const& tblName)
{
   using namespace ei_cxx;
   OTuple t(2);
   t << Atom("data_begin") << tblName;
   t.send(m_port);
}

//---------------------------------------------------------------------------------------------------------------------//
void MicexApplication::onTableDataEnd(std::string const& tblName)
{
   using namespace ei_cxx;
   OTuple t(2);
   t << Atom("data_end") << tblName;
   t.send(m_port);
}

//---------------------------------------------------------------------------------------------------------------------//
void MicexApplication::onTableData(std::string const& tblName, mtesrl::Row const& row)
{
   using namespace ei_cxx;
   using namespace mtesrl;
   if (tblName == "SECURITIES")
   {
      m_instruments->onTableData(row);
   }
   else
   {
      OTuple erlRow(row.size() + 2);
      erlRow << Atom("data_row") << Atom(tblName);
      OutFieldPtr fld = row.first();
      while(fld)
      {
         if (fld->type() == FieldType::charType || fld->type() == FieldType::timeType ||
               fld->type() == FieldType::timeType || fld->type() == FieldType::dateType)
         {
            erlRow << *fld->getAsString();
         }
         else if (fld->type() == FieldType::intType)
         {
            erlRow << *fld->getAsInt64();
         }
         else if (fld->type() == FieldType::fixedType)
         {
            erlRow << *fld->getAsFloat(mtesrl::FixedPrec);
         }
         else if (fld->type() == FieldType::floatType)
         {
            erlRow << *fld->getAsFloat(m_instruments->getDecimals(row));
         }
         fld = row.next();
      }
      erlRow.send(m_port);
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void MicexApplication::onLog(LogLevel::type_t llevel, std::string const& txt)
{
   using namespace ei_cxx;
   OTuple t(3);
   t << Atom("log") << Atom(LogLevel::toString(llevel)) << txt;
   t.send(m_port);
}

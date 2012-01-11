/// @file   table.cpp/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/04/2012 08:38:11 AM

#include "precomp.h"
#include "table.h"
#include "connection_callback.h"

#include <boost/scoped_array.hpp>
#include <boost/format.hpp>

extern ei_cxx::Port g_port;

namespace micex
{

long Table::m_refCounter = 0;

Table::Table(
      std::string const& name,
      ConnectionCallback& cback,
      bool completeLoad,
      bool refreshEnabled,
      InValues const& inValues,
      RequiredOutFields const& reqOutFields)
   :  m_name(name),
      m_cback(cback),
      m_completeLoad(completeLoad),
      m_refreshEnabled(refreshEnabled),
      m_inValues(inValues),
      m_reqOutFields(reqOutFields),
      m_ref(++m_refCounter),
      m_descriptor(-1)
{
}

//---------------------------------------------------------------------------------------------------------------------//
void Table::init(char const*& data)
{
   using namespace boost;
   get_string(data); // skip description
   int32_t flags = get_int32(data);
   if ((flags & MteTableUpdatable) == 0)
   {
      m_refreshEnabled = false;
      LOG_WARN(g_port, FMT("Table %1% doesn't support refreshes.", m_name));
   }
   int32_t numFields = get_int32(data);
   for(int32_t i = 0; i < numFields; ++i)
   {
      m_inFields.push_back(InFieldPtr(new InField(data, m_inValues)));
   }
   numFields = get_int32(data);
   for(int32_t i = 0; i < numFields; ++i)
   {
      m_outFields.push_back(OutFieldPtr(new OutField(data)));
   }
   // non-effective, but...
   for(RequiredOutFields::const_iterator it = m_reqOutFields.begin(); it != m_reqOutFields.end(); ++it)
   {
      bool found = false;
      for(OutFields::const_iterator jt = m_outFields.begin(); jt != m_outFields.end(); ++jt)
      {
         if ((*jt)->name() == *it)
         {
            m_outRow.addField(*jt);
            found = true;
         }
      }
      if (!found)
      {
         THROW(std::runtime_error, FMT("Required field %1% not found among table %2% fields", *it % m_name));
      }
   }
}

//---------------------------------------------------------------------------------------------------------------------//
void Table::open(long connDescr)
{
   if (m_descriptor > 0)
   {
      return;
   }
   boost::scoped_array<char> name(new char[m_name.length() + 1]);
   memcpy(name.get(), m_name.c_str(), m_name.length());
   name[m_name.length()] = '\0';

   std::string const tmp = constructParams();
   boost::scoped_array<char> params(new char[tmp.length() + 1]);
   memcpy(params.get(), tmp.c_str(), tmp.length());
   params[tmp.length()] = '\0';

   MTEMsg* msg = NULL;
   m_descriptor = MTEOpenTable(connDescr, name.get(), params.get(), m_completeLoad, &msg);
   if (m_descriptor < MTE_OK)
   {
      std::string errDescr;
      if (msg)
      {
         errDescr = std::string(reinterpret_cast<char const*>(msg->data), msg->len);
      }
      throw MteError(
         m_descriptor,
         FMT("Unable to open table %1%. Error = %2%, Description = %3%", m_name % m_descriptor % errDescr));
   }
   char const* data = msg->data;
   parse(data);
}

//---------------------------------------------------------------------------------------------------------------------//
void Table::close(long connDescr)
{
   if (m_descriptor < 0)
   {
      return;
   }
   MTECloseTable(connDescr, m_descriptor);
   m_descriptor = -1;
}

//---------------------------------------------------------------------------------------------------------------------//
void Table::skip(char const*& data)
{
   using namespace boost;
   get_string(data); // decription;
   get_int32(data); // attributes;
   int32_t const inFields = get_int32(data);
   for(int32_t i = 0; i < inFields; ++i)
   {
      InField::skip(data);
   }
   int32_t const outFields = get_int32(data);
   for(int32_t i = 0; i < outFields; ++i)
   {
      OutField::skip(data);
   }
}

//---------------------------------------------------------------------------------------------------------------------//
std::string Table::constructParams()
{
   std::string res;
   for(InFields::const_iterator it = m_inFields.begin(); it != m_inFields.end(); ++it)
   {
      res += (*it)->value();
   }
   return res;
}

//---------------------------------------------------------------------------------------------------------------------//
void Table::parse(char const*& data)
{
   m_cback.onTableDataBegin(m_name);

   MTETable const* table = reinterpret_cast<MTETable const*>(data);

   using namespace boost;
   int32_t numRows = table->numRows;
   MTERow const* row = reinterpret_cast<MTERow const*>(&table->rows);
   for(int32_t i = 0; i < numRows; ++i)
   {
      int32_t const numFields = row->numFields == 0 ? m_outFields.size() : row->numFields;
      data = row->data + row->numFields - 1;
      m_outRow.reset();
      for(int32_t j = 0; j < numFields; ++j)
      {
         OutFieldPtr field = m_outFields[row->numFields == 0 ? j : row->fieldNumbers[j]];
         field->parse(data);
      }
      row = reinterpret_cast<MTERow const*>(data);
      try
      {
         m_cback.onTableData(m_name, m_outRow);
      }
      catch(...)
      {
         LOG_ERROR(g_port, "Unexpected exception from callback.");
      }
   }
   m_cback.onTableDataEnd(m_name);
}


} // namespace micex

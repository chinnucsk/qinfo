/// @file   table.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/04/2012 08:35:49 AM

#ifndef MTESRL_TABLE_H
#define MTESRL_TABLE_H

#include "field.h"
#include "row.h"

#include <boost/shared_ptr.hpp>

#include <string>
#include <map>

namespace mtesrl
{

class ConnectionCallback;

typedef std::list<std::string> RequiredOutFields;

class Table
{
public:
   Table(
         std::string const& name,
         ConnectionCallback& cback,
         bool completeLoad,
         bool refreshEnabled,
         InValues const& inValues = InValues(),
         RequiredOutFields const& reqOutFields = RequiredOutFields());
   std::string const& name() const { return m_name; }
   bool refreshEnabled() const { return m_refreshEnabled; }
   bool completeLoad() const { return m_completeLoad; }
   long ref() const { return m_ref; }
   boost::int32_t descriptor() const { return m_descriptor; }
   void init(char const*& data);
   void open(long connDescr);
   void close(long connDescr);
   static void skip(char const*& data);
   void parse(char const*& data);
private:
   std::string constructParams();
private:
   typedef std::list<InFieldPtr> InFields;
   typedef std::vector<OutFieldPtr> OutFields;
   static long         m_refCounter;
   std::string const   m_name;
   ConnectionCallback& m_cback;
   bool const          m_completeLoad;
   bool                m_refreshEnabled;
   InValues            m_inValues;
   RequiredOutFields   m_reqOutFields;
   long                m_ref;
   boost::int32_t      m_descriptor;
   InFields            m_inFields;
   OutFields           m_outFields;
   Row                 m_outRow;
};

typedef boost::shared_ptr<Table> TablePtr;

} // namespace mtesrl


#endif // MTESRL_TABLE_H

/// @file   connection_callback.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/09/2012 08:50:04 AM

#ifndef MICEX_DRIVER_CONNECTION_CALLBACK_H
#define MICEX_DRIVER_CONNECTION_CALLBACK_H

#include "field.h"

#include <common/smart_enum.h>

#include <boost/optional.hpp>
#include <boost/cstdint.hpp>

#include <string>
#include <map>

namespace micex
{

class OutRow
{
   friend class Table;
public:
   OutField();
   OutFieldPtr getField(std::string const& fieldName) const;
   OutFieldPtr first();
   OutFieldPtr next();
private:
   void addField(OutFieldPtr outField);
private:
   typedef std::map<std::string, OutFieldPtr> OutFields;
   OutFields m_fields;
   OutFields::const_iterator m_cursor;
};


DECLARE_ENUM
(
   ConnectionStatus, int,
   ENTRY(Connected,    1)
   ENTRY(Disconnected, 2)
)

class ConnectionCallback
{
public:
   virtual void onConnectionStatus(ConnectionStatus::type_t status) = 0;
   virtual void onTableDataBegin(std::string const& tblName) = 0;
   virtual void onTableData(std::string const& tblName, OutRow const& row) = 0;
   virtual void onTableDataEnd(std::string const& tblName) = 0;
};

} // namespace micex

#endif // MICEX_DRIVER_CONNECTION_CALLBACK_H

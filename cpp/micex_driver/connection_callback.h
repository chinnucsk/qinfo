/// @file   connection_callback.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/09/2012 08:50:04 AM

#ifndef MICEX_DRIVER_CONNECTION_CALLBACK_H
#define MICEX_DRIVER_CONNECTION_CALLBACK_H

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
   boost::optional<boost::int64_t> getAsInt64(std::string const& fieldName) const;
   boost::optional<float> getAsFloat(std::string const& fieldName, unsigned int precision) const;
   boost::optional<std::string> getAsString(std::string const& fieldName) const;
private:
   void addField(std::string const& name, std::string const& value);
private:
   typedef std::map<std::string, std::string> Fields;
   Fields m_fields;
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
   virtual void onTableData(std::sring const& tblName, OutRow const& row) = 0;
   virtual void onTableDataEnd(std::string const& tblName) = 0;
};

} // namespace micex

#endif // MICEX_DRIVER_CONNECTION_CALLBACK_H

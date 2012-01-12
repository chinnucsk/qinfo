/// @file   connection_callback.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/09/2012 08:50:04 AM

#ifndef MTESRL_CONNECTION_CALLBACK_H
#define MTESRL_CONNECTION_CALLBACK_H

#include "field.h"

#include <common/smart_enum.h>
#include <common/log_wrapper.h>

#include <boost/optional.hpp>
#include <boost/cstdint.hpp>

#include <string>
#include <map>
#include <list>

namespace mtesrl
{

class Row;

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
   virtual void onTableData(std::string const& tblName, Row const& row) = 0;
   virtual void onTableDataEnd(std::string const& tblName) = 0;
   virtual void onLog(LogLevel::type_t llevel, std::string const& txt) = 0;
};

} // namespace mtesrl

#endif // MTESRL_CONNECTION_CALLBACK_H

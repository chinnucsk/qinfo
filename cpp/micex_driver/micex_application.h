/// @file   micex_application.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/12/2012 07:00:01 PM

#ifndef MICEX_DRIVER_APPLICATION_H
#define MICEX_DRIVER_APPLICATION_H

#include <mtesrl/connection.h>
#include <mtesrl/connection_callback.h>
#include "instruments.h"

#include <common/log_wrapper.h>

#include <string>
#include <map>

namespace mtesrl
{
class Row;
} // namespace mtesrl

namespace ei_cxx
{
class Port;
} // namespace ei_cxx


class MicexApplication : public mtesrl::ConnectionCallback
{
public:
   MicexApplication(ei_cxx::Port& port, std::string const& fileName, LogLevel::type_t llevel);
   void MicexApplication::addTable(
      std::string const& name,
      bool completeLoad,
      bool refreshEnabled,
      mtesrl::InValues const& inValues,
      mtesrl::RequiredOutFields const& reqOutFields);
   void open(std::string const& connParams);
   void close();
private:
   virtual void onConnectionStatus(mtesrl::ConnectionStatus::type_t status);
   virtual void onTableDataBegin(std::string const& tblName);
   virtual void onTableDataEnd(std::string const& tblName);
   virtual void onTableData(std::string const& tblName, mtesrl::Row const& row);
   virtual void onLog(LogLevel::type_t llevel, std::string const& txt);
private:
   typedef std::map<std::string, unsigned int> Decimals;
   mtesrl::Connection   m_conn;
   InstrumentsPtr       m_instruments;
   ei_cxx::Port&        m_port;
};

#endif // MICEX_DRIVER_APPLICATION_H

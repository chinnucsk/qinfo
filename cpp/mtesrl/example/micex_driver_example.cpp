/// @file   micex_driver_example.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/09/2012 10:29:33 AM

#include <mtesrl/connection.h>
#include <mtesrl/connection_callback.h>
#include <ei_cxx/port.h>

#include <boost/assign/list_of.hpp>

#include <tchar.h>

class Data : public mtesrl::ConnectionCallback
{
public:
   virtual void onConnectionStatus(mtesrl::ConnectionStatus::type_t status)
   {
      std::cout << "Connection status: " << mtesrl::ConnectionStatus::toString(status) << std::endl;
   }
   virtual void onTableDataBegin(std::string const& tblName)
   {
      std::cout << "Table data begin: " << tblName << std::endl;
   }
   virtual void onTableDataEnd(std::string const& tblName)
   {
      std::cout << "Table data end: " << tblName << std::endl;
   }
   virtual void onTableData(std::string const& tblName, mtesrl::Row const& row)
   {
      if (tblName == "SECURITIES")
      {
         std::string key = *row.getField("SECBOARD")->getAsString() + *row.getField("SECCODE")->getAsString();
         if (m_decimals.find(key) == m_decimals.end())
         {
            std::cout << "Table data: SECBOARD = " << *row.getField("SECBOARD")->getAsString()
                      << ", SECNAME = " << *row.getField("SECCODE")->getAsString()
                      << ", DECIMALS = " << *row.getField("DECIMALS")->getAsInt64() << std::endl;

            m_decimals.insert(std::make_pair(key, *row.getField("DECIMALS")->getAsInt64()));
         }
      }
      else if (tblName == "ORDERS")
      {
         Decimals::iterator it = m_decimals.find(*row.getField("SECBOARD")->getAsString() +
               *row.getField("SECCODE")->getAsString());
         std::cout << "ORDERNO = " << *row.getField("ORDERNO")->getAsInt64()
                   << ", PRICE = " << *row.getField("PRICE")->getAsFloat(it->second)
                   << std::endl;
      }
   }
   void onLog(LogLevel::type_t llevel, std::string const& txt)
   {
      std::cout << LogLevel::toString(llevel) << ": " << txt << std::endl;
   }
private:
   typedef std::map<std::string, int> Decimals;
   Decimals m_decimals;
};


int _tmain(int argc, _TCHAR* argv[])
{
   Data data;
   mtesrl::Connection conn("mtesrl.dll", data);
   conn.addTable("SECURITIES", true, false, boost::assign::map_list_of("BOARDID", "EQBR"),
         boost::assign::list_of("SECBOARD")("SECCODE")("DECIMALS"));
   conn.addTable("ORDERS", false, true);
   conn.open("HOST=xxxx:3128\r\nSERVER=XXXX\r\nUSERID=xxxx\r\nPASSWORD=\r\nINTERFACE=IFCBroker_15\r\nFEEDBACK=info\r\n");
   Sleep(10000000);
   return 0;
}

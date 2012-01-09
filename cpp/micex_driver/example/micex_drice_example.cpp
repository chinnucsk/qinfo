/// @file   micex_drice_example.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/09/2012 10:29:33 AM

#include "../connection.h"
#include "../connection_callback.h"

#include <ei_cxx/port.h>

#include <boost/assign/list_of.hpp>

ei_cxx:Port g_port;

TWinDynDriverCallbacks WinDynDriverCallbacks;

class Data : public micex::ConnectionCallback
{
public:
   virtual void onConnectionStatus(micex::ConnectionStatus::type_t status)
   {
      std::cout << "Connection status: " << micex::ConnectionStatus::toString(status) << std::endl;
   }
   virtual void onTableDataBegin(std::string const& tlbName)
   {
      std::cout << "Table data begin: " << tblName << std::endl;
   }
   virtual void onTableDataEnd(std::string const& tlbName)
   {
      std::cout << "Table data end: " << tblName << std::endl;
   }
   virtual void onTableData(std::string const& tblName, micex::OutRow const& row)
   {
      if (tblName == "SECURITIES")
      {
         std::string key = *row.getAsString("SECBOARD") + *row.getAsString("SECCODE");
         if (m_decimals.find(key) == m_decimals.end())
         {
            std::cout << "Table data: SECBOARD = " << *row.getAsString("SECBOARD")
                      << ", SECNAME = " << *row.getAsString("SECCODE")
                      << ", DECIMALS = " << *row.getAsInt64("DECIMALS") << std::endl;

            m_decimals.insert(std::make_pair(key, *row.getAsInt64("DECIMALS")));
         }
      }
      else if (tblName == "ORDERS")
      {
         Decimals::iterator it = m_decimals.find(*row.getAsString("SECBOARD") + *row.getAsString("SECCODE"));
         std::cout << "ORDERNO = " << *row.getAsInt64("ORDERNO")
                   << ", PRICE = " << *row.getAsFloat("PRICE", it->second)
                   << std::endl;
      }
   }
private:
   typedef std::map<std::string, int> Decimals;
   Decimals m_decimals;
};


int _tmain(int argc, _TCHAR* argv[])
{
   Data data;
   micex::Connection conn(data);
   conn.addTable("SECURITIES", true, false, boost::assign::map_list_of("BOARDID", "EQBR"));
   conn.addTable("ORDERS", false, true);
   conn.open("HOST=xxxx:3128\r\nSERVER=XXXX\r\nUSERID=xxxx\r\nPASSWORD=\r\nINTERFACE=IFCBroker_15\r\nFEEDBACK=info\r\n");
   Sleep(10000000);
   return 0;
}

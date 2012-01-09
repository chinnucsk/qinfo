/// @file   mtesrl.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/03/2012 11:32:59 AM

#ifndef MTESRL_PUBLIC_H
#define MTESRL_PUBLIC_H

#pragma pack(push)
#pragma pack(1)

#include <windows.h>

#include <boost/cstdint.hpp>

extern "C"
{

struct MTEMsg
{
   boost::int32_t len;
   char data[1];
};

struct MTERow
{
   char numFields;
   boost::int32_t  len;
   char fieldNumbers[1];
   char data[1];
   size_t size() const { return sizeof(numFields) + sizeof(len) + numFields + len; }
};

struct MTETable
{
   boost::int32_t ref;
   boost::int32_t numRows;
   MTERow rows[1];
   size_t size() const
   {
      size_t sz = sizeof(ref) + sizeof(numRows);
      MTERow const* row = reinterpret_cast<MTERow const*>(&rows);
      for(int i = 0; i < numRows; ++i)
      {
         sz += row->size();
         row = reinterpret_cast<MTERow const*>(reinterpret_cast<char const*>(row) + row->size());
      }
      return sz;
   }
};

struct MTETables
{
   boost::int32_t numTables;
   MTETable tables[1];
};

namespace
{

int const MteTableUpdatable     = 1;
int const MteTableClearOnUpdate = 2;

int const ZlibCompressed = 0x1;
int const FlagEncrypted  = 0x2;
int const FlagSigningOn  = 0x4;

} // namespace

struct ConnectionStats
{
   boost::int32_t size;
   DWORD properties;
   DWORD SendPacket;
   DWORD RecvPacket;
   DWORD SentBytes;
   DWORD RecvBytes;
};

boost::int32_t WINAPI MTEOpenTable(boost::int32_t clientIdx, char* tableName, char* params, boost::int32_t completeFlag, MTEMsg** msg);
boost::int32_t WINAPI MTEAddTable(boost::int32_t clientIdx, boost::int32_t htable, boost::int32_t ref);
boost::int32_t WINAPI MTEExecTrans(boost::int32_t clientIdx, char* tranName, char* params, char* resultMsg);
boost::int32_t WINAPI MTEStructure(boost::int32_t idx, MTEMsg** msg);
boost::int32_t WINAPI MTEConnect(char* params, char* errMsg);
boost::int32_t WINAPI MTERefresh(boost::int32_t clientIdx, MTEMsg** msg);
boost::int32_t WINAPI MTECloseTable(boost::int32_t clientIdx, boost::int32_t htable);
boost::int32_t WINAPI MTEDisconnect(boost::int32_t clientIdx);
char* WINAPI MTEErrorMsg(boost::int32_t errorCode);
boost::int32_t WINAPI MTEFreeBuffer(boost::int32_t clientIdx);
boost::int32_t WINAPI MTEGetShapshot(boost::int32_t clientIdx, char** snapshot, int* len);
boost::int32_t WINAPI MTESetShapshot(boost::int32_t clientIdx, char* snapshot, int len, char* error);
boost::int32_t WINAPI MTEConnectionStats(boost::int32_t clientIdx, ConnectionStats* stats);


} // extern "C"

#define MTE_OK               0
#define MTE_CONFIG          -1
#define MTE_SRVUNAVAIL      -2
#define MTE_LOGERROR        -3
#define MTE_INVALIDCONNECT  -4
#define MTE_NOTCONNECTED    -5
#define MTE_WRITE           -6
#define MTE_READ            -7
#define MTE_TSMR            -8
#define MTE_NOMEMORY        -9
#define MTE_ZLIB           -10
#define MTE_PKTINPROGRESS  -11
#define MTE_PKTNOTSTARTED  -12
#define MTE_LOGON          -13
#define MTE_FATALERROR     MTE_LOGON // not used because of cabalistic signification
#define MTE_INVALIDHANDLE  -14
#define MTE_DSROFF         -15
#define MTE_ERRUNKNOWN     -16
#define MTE_BADPTR         -17
#define MTE_WRONGPARAM     MTE_BADPTR
#define MTE_TRANSREJECTED  -18
#define MTE_REJECTION      MTE_TRANSREJECTED
#define MTE_TOOSLOWCONNECT -19
#define MTE_TEUNAVAIL      MTE_TOOSLOWCONNECT
#define MTE_CRYPTO_ERROR   -20


//Don't forget update this define
#define _MTELastError       21

#pragma pack(pop)

#endif // MTESRL_PUBLIC_H

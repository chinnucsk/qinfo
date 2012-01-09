/// @file   mtesrl.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/03/2012 11:32:59 AM

#ifndef MTESRL_PUBLIC_H
#define MTESRL_PUBLIC_H

#pragma pack(push)
#pragma pack(1)

#include <windows.h>

#include <string>

#include <boost/cstdint.hpp>

namespace
{

int const MTE_OK             =  0
int const MTE_CONFIG         = -1
int const MTE_SRVUNAVAIL     = -2
int const MTE_LOGERROR       = -3
int const MTE_INVALIDCONNECT = -4
int const MTE_NOTCONNECTED   = -5
int const MTE_WRITE          = -6
int const MTE_READ           = -7
int const MTE_TSMR           = -8
int const MTE_NOMEMORY       = -9
int const MTE_ZLIB           = -10
int const MTE_PKTINPROGRESS  = -11
int const MTE_PKTNOTSTARTED  = -12
int const MTE_LOGON          = -13
int const MTE_FATALERROR     = MTE_LOGON // not used because of cabalistic signification
int const MTE_INVALIDHANDLE  = -14
int const MTE_DSROFF         = -15
int const MTE_ERRUNKNOWN     = -16
int const MTE_BADPTR         = -17
int const MTE_WRONGPARAM     = MTE_BADPTR
int const MTE_TRANSREJECTED  = -18
int const MTE_REJECTION      = MTE_TRANSREJECTED
int const MTE_TOOSLOWCONNECT = -19
int const MTE_TEUNAVAIL      = MTE_TOOSLOWCONNECT
int const MTE_CRYPTO_ERROR   = -20

//Don't forget update this define
#define _MTELastError       21

int const MteTableUpdatable     = 1;
int const MteTableClearOnUpdate = 2;

int const ZlibCompressed = 0x1;
int const FlagEncrypted  = 0x2;
int const FlagSigningOn  = 0x4;

} // namespace

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

struct ConnectionStats
{
   boost::int32_t size;
   DWORD properties;
   DWORD SendPacket;
   DWORD RecvPacket;
   DWORD SentBytes;
   DWORD RecvBytes;
};


void MTELoadLibrary(std::string const& fileName);
boost::int32_t MTEOpenTable(boost::int32_t clientIdx, char* tableName, char* params, boost::int32_t completeFlag, MTEMsg** msg);
boost::int32_t MTEAddTable(boost::int32_t clientIdx, boost::int32_t htable, boost::int32_t ref);
boost::int32_t MTEExecTrans(boost::int32_t clientIdx, char* tranName, char* params, char* resultMsg);
boost::int32_t MTEStructure(boost::int32_t clientIdx, MTEMsg** msg);
boost::int32_t MTEConnect(char* params, char* errMsg);
boost::int32_t MTERefresh(boost::int32_t clientIdx, MTEMsg** msg);
boost::int32_t MTECloseTable(boost::int32_t clientIdx, boost::int32_t htable);
boost::int32_t MTEDisconnect(boost::int32_t clientIdx);
char* MTEErrorMsg(boost::int32_t errorCode);
boost::int32_t MTEFreeBuffer(boost::int32_t clientIdx);
boost::int32_t MTEGetShapshot(boost::int32_t clientIdx, char** snapshot, int* len);
boost::int32_t MTESetShapshot(boost::int32_t clientIdx, char* snapshot, int len, char* error);
boost::int32_t MTEConnectionStats(boost::int32_t clientIdx, ConnectionStats* stats);

#pragma pack(pop)

#endif // MTESRL_PUBLIC_H

/// @file   mtesrl.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/03/2012 11:32:59 AM

#ifndef MTESRL_PUBLIC_H
#define MTESRL_PUBLIC_H

#include <windows.h>

extern "C"
{

struct MTEMsg
{
   long len;
   char data[1];
};

struct MTERow
{
   char numFields;
   long  len;
   char* fieldNumbers;
   char* data;
   size_t size() const { return sizeof(numFields) + sizeof(len) + numFields + len; }
};

struct MTETable
{
   long ref;
   long numRows;
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
   }
};

struct MTETables
{
   long numTable;
   MTETable* tables;
};

namespace
{

int const ZlibCompressed = 0x1;
int const FlagEncrypted  = 0x2;
int const FlagSigningOn  = 0x4;

} // namespace

struct ConnectionStats
{
   long size;
   DWORD properties;
   DWORD SendPacket;
   DWORD RecvPacket;
   DWORD SentBytes;
   DWORD RecvBytes;
};

long WINAPI MTEOpenTable(long clientIdx, char* tableName, char* params, long completeFlag, MTEMsg** msg);
long WINAPI MTEAddTable(long clientIdx, long htable, long ref);
long WINAPI MTEExecTrans(long clientIdx, char* tranName, char* params, char* resultMsg);
long WINAPI MTEStructure(long idx, MTEMsg** msg);
long WINAPI MTEConnect(char* params, char* errMsg);
long WINAPI MTERefresh(long clientIdx, MTEMsg** msg);
long WINAPI MTECloseTable(long clientIdx, long htable);
long WINAPI MTEDisconnect(long clientIdx);
char* WINAPI MTEErrorMsg(long errorCode);
long WINAPI MTEFreeBuffer(long clientIdx);
long WINAPI MTEGetShapshot(long clientIdx, char** snapshot, int* len);
long WINAPI MTESetShapshot(long clientIdx, char* snapshot, int len, char* error);
long WINAPI MTEConnectionStats(long clientIdx, ConnectionStats* stats);


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
#define MTE_FATALERROR		MTE_LOGON // not used because of cabalistic signification
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


#endif // MTESRL_PUBLIC_H

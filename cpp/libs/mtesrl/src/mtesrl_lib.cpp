/// @file   mtesrl.cpp
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/09/2012 01:31:57 PM

#include "../include/mtesrl/mtesrl_lib.h"

#include <sstream>

typedef boost::int32_t (CALLBACK* MTEOpenTablePtr)(boost::int32_t, char*, char*, boost::int32_t, MTEMsg**);
typedef boost::int32_t (CALLBACK* MTEAddTablePtr)(boost::int32_t, boost::int32_t, boost::int32_t);
typedef boost::int32_t (CALLBACK* MTEExecTransPtr)(boost::int32_t, char*, char*, char*);
typedef boost::int32_t (CALLBACK* MTEStructurePtr)(boost::int32_t, MTEMsg**);
typedef boost::int32_t (CALLBACK* MTEConnectPtr)(char*, char*);
typedef boost::int32_t (CALLBACK* MTERefreshPtr)(boost::int32_t, MTEMsg**);
typedef boost::int32_t (CALLBACK* MTECloseTablePtr)(boost::int32_t, boost::int32_t);
typedef boost::int32_t (CALLBACK* MTEDisconnectPtr)(boost::int32_t);
typedef char* (CALLBACK* MTEErrorMsgPtr)(boost::int32_t);
typedef boost::int32_t (CALLBACK* MTEFreeBufferPtr)(boost::int32_t);
typedef boost::int32_t (CALLBACK* MTEGetShapshotPtr)(boost::int32_t, char**, int*);
typedef boost::int32_t (CALLBACK* MTESetShapshotPtr)(boost::int32_t, char*, int, char*);
typedef boost::int32_t (CALLBACK* MTEConnectionStatsPtr)(boost::int32_t, ConnectionStats*);

HMODULE g_library = NULL;

MTEOpenTablePtr         g_MTEOpenTable = NULL;
MTEAddTablePtr          g_MTEAddTable = NULL;
MTEExecTransPtr         g_MTEExecTrans = NULL;
MTEStructurePtr         g_MTEStructure = NULL;
MTEConnectPtr           g_MTEConnect = NULL;
MTERefreshPtr           g_MTERefresh = NULL;
MTECloseTablePtr        g_MTECloseTable = NULL;
MTEDisconnectPtr        g_MTEDisconnect = NULL;
MTEErrorMsgPtr          g_MTEErrorMsg = NULL;
MTEFreeBufferPtr        g_MTEFreeBuffer = NULL;
MTEGetShapshotPtr       g_MTEGetShapshot = NULL;
MTESetShapshotPtr       g_MTESetShapshot = NULL;
MTEConnectionStatsPtr   g_MTEConnectionStats = NULL;

//---------------------------------------------------------------------------------------------------------------------//
void MTELoadLibrary(std::string const& fileName)
{
   if (g_library)
   {
      return;
   }
   g_library = LoadLibrary(fileName.c_str());
   if (!g_library)
   {
      std::ostringstream ost;
      ost << "Failed to load " << fileName  << " library. Error = " << GetLastError();
      throw std::runtime_error(ost.str());
   }
   g_MTEOpenTable =  (MTEOpenTablePtr)GetProcAddress(g_library, "MTEOpenTable");
   g_MTEAddTable =   (MTEAddTablePtr)GetProcAddress(g_library, "MTEAddTable");
   g_MTEExecTrans =  (MTEExecTransPtr)GetProcAddress(g_library, "MTEExecTrans");
   g_MTEStructure =  (MTEStructurePtr)GetProcAddress(g_library, "MTEStructure");
   g_MTEConnect =    (MTEConnectPtr)GetProcAddress(g_library, "MTEConnect");
   g_MTERefresh =    (MTERefreshPtr)GetProcAddress(g_library, "MTERefresh");
   g_MTECloseTable = (MTECloseTablePtr)GetProcAddress(g_library, "MTECloseTable");
   g_MTEDisconnect = (MTEDisconnectPtr)GetProcAddress(g_library, "MTEDisconnect");
   g_MTEErrorMsg =   (MTEErrorMsgPtr)GetProcAddress(g_library, "MTEErrorMsg");
   g_MTEFreeBuffer =  (MTEFreeBufferPtr)GetProcAddress(g_library, "MTEFreeBuffer");
   g_MTEGetShapshot = (MTEGetShapshotPtr)GetProcAddress(g_library, "MTEGetShapshot");
   g_MTESetShapshot = (MTESetShapshotPtr)GetProcAddress(g_library, "MTESetShapshot");
   g_MTEConnectionStats = (MTEConnectionStatsPtr)GetProcAddress(g_library, "MTEConnectionStats");
}

//---------------------------------------------------------------------------------------------------------------------//
boost::int32_t MTEOpenTable(boost::int32_t clientIdx, char* tableName, char* params, boost::int32_t completeFlag, MTEMsg** msg)
{
   return g_MTEOpenTable(clientIdx, tableName, params, completeFlag, msg);
}

//---------------------------------------------------------------------------------------------------------------------//
boost::int32_t MTEAddTable(boost::int32_t clientIdx, boost::int32_t htable, boost::int32_t ref)
{
   return g_MTEAddTable(clientIdx, htable, ref);
}

//---------------------------------------------------------------------------------------------------------------------//
boost::int32_t MTEExecTrans(boost::int32_t clientIdx, char* tranName, char* params, char* resultMsg)
{
   return g_MTEExecTrans(clientIdx, tranName, params, resultMsg);
}

//---------------------------------------------------------------------------------------------------------------------//
boost::int32_t MTEStructure(boost::int32_t clientIdx, MTEMsg** msg)
{
   return g_MTEStructure(clientIdx, msg);
}

//---------------------------------------------------------------------------------------------------------------------//
boost::int32_t MTEConnect(char* params, char* errMsg)
{
   return g_MTEConnect(params, errMsg);
}

//---------------------------------------------------------------------------------------------------------------------//
boost::int32_t MTERefresh(boost::int32_t clientIdx, MTEMsg** msg)
{
   return g_MTERefresh(clientIdx, msg);
}

//---------------------------------------------------------------------------------------------------------------------//
boost::int32_t MTECloseTable(boost::int32_t clientIdx, boost::int32_t htable)
{
   return g_MTECloseTable(clientIdx, htable);
}

//---------------------------------------------------------------------------------------------------------------------//
boost::int32_t MTEDisconnect(boost::int32_t clientIdx)
{
   return g_MTEDisconnect(clientIdx);
}

//---------------------------------------------------------------------------------------------------------------------//
char* MTEErrorMsg(boost::int32_t errorCode)
{
   return g_MTEErrorMsg(errorCode);
}

//---------------------------------------------------------------------------------------------------------------------//
boost::int32_t MTEFreeBuffer(boost::int32_t clientIdx)
{
   return g_MTEFreeBuffer(clientIdx);
}

//---------------------------------------------------------------------------------------------------------------------//
boost::int32_t MTEGetShapshot(boost::int32_t clientIdx, char** snapshot, int* len)
{
   return g_MTEGetShapshot(clientIdx, snapshot, len);
}

//---------------------------------------------------------------------------------------------------------------------//
boost::int32_t MTESetShapshot(boost::int32_t clientIdx, char* snapshot, int len, char* error)
{
   return g_MTESetShapshot(clientIdx, snapshot, len, error);
}

//---------------------------------------------------------------------------------------------------------------------//
boost::int32_t MTEConnectionStats(boost::int32_t clientIdx, ConnectionStats* stats)
{
   return g_MTEConnectionStats(clientIdx, stats);
}

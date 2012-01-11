#pragma once

#include <sstream>

#define _WIN32_WINNT 0x0501

#include <atlbase.h>
#include <atlcom.h>
#include <atldbcli.h>

#include <exception>
#include <sstream>
#include <string>

#include <common/log_wrapper.h>

#define WIN32_LEAN_AND_MEAN
#define VC_EXTRALEAN
#define NOMINMAX

#include <Windows.h>

#import "tlb/P2ClientGateMTA.tlb" named_guids

#define _ATL_FREE_THREADED

DECLARE_ENUM
(
   ConnectionStatus, long,
   ENTRY(DISCONNECTED,                0x1)
   ENTRY(CONNECTED,                   0x2)
   ENTRY(INVALID,                     0x4)
   ENTRY(BUSY,                        0x8)
   ENTRY(ROUTER_DISCONNECTED,     0x10000)
   ENTRY(ROUTER_RECONNECTING,     0x20000)
   ENTRY(ROUTER_CONNECTED,        0x40000)
   ENTRY(ROUTER_LOGINFAILED,      0x80000)
   ENTRY(ROUTER_NOCONNECT,       0x100000)
)

DECLARE_ENUM
(
   StreamState, unsigned long,
   ENTRY(Close,            0)
   ENTRY(LocalSnapshot,    1)
   ENTRY(RemoteSnapshot,   2)
   ENTRY(Online,           3)
   ENTRY(CloseComplete,    4)
   ENTRY(Reopen,           5)
   ENTRY(Error,            6)
)


DECLARE_ENUM
(
   ErrorCode, int,
   ENTRY(P2ERR_COMMON, 0)
   ENTRY(P2ERR_OK, P2ERR_COMMON)
)

DECLARE_ENUM
(
   StreamType, long,
   ENTRY(RT_LOCAL,               0)
   ENTRY(RT_COMBINED_SNAPSHOT,   1)
   ENTRY(RT_COMBINED_DYNAMIC,    2)
   ENTRY(RT_REMOTE_SNAPSHOT,     3)
   ENTRY(RT_REMOTE_DELETED,      4)
   ENTRY(RT_REMOTE_ONLINE,       8)
)

class TryAgain : public std::exception
{
public:
   explicit TryAgain(std::string const& text) : std::exception(text.c_str())
   {
   }
};


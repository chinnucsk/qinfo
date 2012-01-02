/// @file   common.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 06/24/2011 06:28:04 PM
/// @version $Id: Exp $

#ifndef PLAZA2_PORT_COMMON_H
#define PLAZA2_PORT_COMMON_H

#include <ei_cxx/tuple.h>
#include <ei_cxx/atom.h>
#include "smart_enum.h"

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
   LogLevel, long,
   ENTRY(error,   0)
   ENTRY(warning, 1)
   ENTRY(info,    2)
   ENTRY(debug,   3)
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

extern LogLevel::type_t log_level;

#define LOG_DEBUG(port, Str)            \
if (log_level >= LogLevel::debug)       \
{                                       \
   using namespace ei_cxx;              \
   std::ostringstream ost;              \
   ost << Str;                          \
   OTuple t(2);                         \
   t << Atom("log_debug") << ost.str(); \
   t.send(port);                        \
}

#define LOG_INFO(port, Str)            \
if (log_level >= LogLevel::info)       \
{                                      \
   using namespace ei_cxx;             \
   std::ostringstream ost;             \
   ost << Str;                         \
   OTuple t(2);                        \
   t << Atom("log_info") << ost.str(); \
   t.send(port);                       \
}

#define LOG_WARN(port, Str)            \
if (log_level >= LogLevel::warning)    \
{                                      \
   using namespace ei_cxx;             \
   std::ostringstream ost;             \
   ost << Str;                         \
   OTuple t(2);                        \
   t << Atom("log_warn") << ost.str(); \
   t.send(port);                       \
}

#define LOG_ERROR(port, Str)            \
if (log_level >= LogLevel::error)       \
{                                       \
   using namespace ei_cxx;              \
   std::ostringstream ost;              \
   ost << Str;                          \
   OTuple t(2);                         \
   t << Atom("log_error") << ost.str(); \
   t.send(port);                        \
}

#endif // PLAZA2_PORT_COMMON_H

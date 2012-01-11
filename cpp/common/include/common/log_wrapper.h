/// @file   log_wrapper.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 06/24/2011 06:28:04 PM
/// @version $Id: Exp $

#ifndef COMMON_LOG_WRAPPER_H
#define COMMON_LOG_WRAPPER_H

#include "smart_enum.h"

DECLARE_ENUM
(
   LogLevel, long,
   ENTRY(error,   0)
   ENTRY(warning, 1)
   ENTRY(info,    2)
   ENTRY(debug,   3)
)

extern LogLevel::type_t log_level;

#define ERL_LOG_DEBUG(port, Str)        \
if (log_level >= LogLevel::debug)       \
{                                       \
   using namespace ei_cxx;              \
   std::ostringstream ost;              \
   ost << Str;                          \
   OTuple t(3);                         \
   t << Atom("log")                     \
     << Atom(LogLevel::toString(LogLevel::debug) << ost.str();     \
   t.send(port);                        \
}

#define ERL_LOG_INFO(port, Str)        \
if (log_level >= LogLevel::info)       \
{                                      \
   using namespace ei_cxx;             \
   std::ostringstream ost;             \
   ost << Str;                         \
   OTuple t(3);                        \
   t << Atom("log")                    \
     << Atom(LogLevel::toString(LogLevel::info) << ost.str();     \
   t.send(port);                       \
}

#define ERL_LOG_WARN(port, Str)        \
if (log_level >= LogLevel::warning)    \
{                                      \
   using namespace ei_cxx;             \
   std::ostringstream ost;             \
   ost << Str;                         \
   OTuple t(3);                        \
   t << Atom("log")                    \
     << Atom(LogLevel::toString(LogLevel::warning) << ost.str(); \
   t.send(port);                       \
}

#define ERL_LOG_ERROR(port, Str)       \
if (log_level >= LogLevel::error)       \
{                                       \
   using namespace ei_cxx;              \
   std::ostringstream ost;              \
   ost << Str;                          \
   OTuple t(3);                                                \
   t << Atom("log")                                            \
     << Atom(LogLevel::toString(LogLevel::error) << ost.str(); \
   t.send(port);                                               \
}

#endif // COMMON_ERL_LOG_WRAPPER_H

/// @file   log_wrapper.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 06/24/2011 06:28:04 PM
/// @version $Id: Exp $

#ifndef EI_CXX_LOG_WRAPPER_H
#define EI_CXX_LOG_WRAPPER_H

#include "atom.h"
#include "tuple.h"

#include <common/smart_enum.h>

DECLARE_ENUM
(
   LogLevel, long,
   ENTRY(error,   0)
   ENTRY(warning, 1)
   ENTRY(info,    2)
   ENTRY(debug,   3)
)

extern LogLevel::type_t log_level;

#define LOG_DEBUG(port, Str)            \
if (log_level >= LogLevel::debug)       \
{                                       \
   using namespace ei_cxx;              \
   std::ostringstream ost;              \
   ost << Str;                          \
   ei_cxx::OTuple t(2);                 \
   t << Atom("log_debug") << ost.str(); \
   t.send(port);                        \
}

#define LOG_INFO(port, Str)            \
if (log_level >= LogLevel::info)       \
{                                      \
   using namespace ei_cxx;             \
   std::ostringstream ost;             \
   ost << Str;                         \
   ei_cxx::OTuple t(2);                \
   t << Atom("log_info") << ost.str(); \
   t.send(port);                       \
}

#define LOG_WARN(port, Str)            \
if (log_level >= LogLevel::warning)    \
{                                      \
   using namespace ei_cxx;             \
   std::ostringstream ost;             \
   ost << Str;                         \
   ei_cxx::OTuple t(2);                \
   t << Atom("log_warn") << ost.str(); \
   t.send(port);                       \
}

#define LOG_ERROR(port, Str)            \
if (log_level >= LogLevel::error)       \
{                                       \
   using namespace ei_cxx;              \
   std::ostringstream ost;              \
   ost << Str;                          \
   ei_cxx::OTuple t(2);                 \
   t << Atom("log_error") << ost.str(); \
   t.send(port);                        \
}

#endif // EI_CXX_LOG_WRAPPER_H

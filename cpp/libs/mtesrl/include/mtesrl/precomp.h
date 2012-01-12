/// @file   precomp.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/04/2012 04:25:04 PM

#ifndef MTESRL_PRECOMP_H
#define MTESRL_PRECOMP_H

#define NOMINMAX
#define VC_EXTRALEAN
#define WIN32_LEAN_AND_MEAN

#include <Windows.h>

#include <common/log_wrapper.h>

#include <list>
#include <map>
#include <set>
#include <string>
#include <exception>
#include <sstream>

#include <boost/cstdint.hpp>

int const MTE_ERRMSG_SIZE = 256 + 1;

inline std::string get_string(char const*& buff)
{
   boost::int32_t sz = *(boost::int32_t*)buff;
   std::string res(buff + sizeof(boost::int32_t), sz);
   buff += (sz + sizeof(boost::int32_t));
   return res;
}

inline boost::int32_t get_int32(char const*& buff)
{
   boost::int32_t res = *(boost::int32_t*)buff;
   buff += sizeof(boost::int32_t);
   return res;
}

class MteError : public std::exception
{
public:
   MteError(int err, std::string const& descr) : m_error(err), m_what(descr)
   {
   }
   int error() const { return m_error; }
   virtual char const* what() const { return m_what.c_str(); }
private:
   int m_error;
   std::string m_what;
};

#define MTESRL_LOG_DEBUG(cback, Str)        \
if (log_level >= LogLevel::debug)           \
{                                           \
   std::ostringstream ost;                  \
   ost << Str;                              \
   cback.onLog(LogLevel::debug, ost.str()); \
}

#define MTESRL_LOG_INFO(cback, Str)          \
if (log_level >= LogLevel::info)             \
{                                            \
   std::ostringstream ost;                   \
   ost << Str;                               \
   cback.onLog(LogLevel::info, ost.str());   \
}

#define MTESRL_LOG_WARN(cback, Str)           \
if (log_level >= LogLevel::warning)           \
{                                             \
   std::ostringstream ost;                    \
   ost << Str;                                \
   cback.onLog(LogLevel::warning, ost.str()); \
}

#define MTESRL_LOG_ERROR(cback, Str)        \
if (log_level >= LogLevel::error)           \
{                                           \
   std::ostringstream ost;                  \
   ost << Str;                              \
   cback.onLog(LogLevel::error, ost.str()); \
}

#endif // MTESRL_PRECOMP_H

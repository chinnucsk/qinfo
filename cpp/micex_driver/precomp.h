/// @file   precomp.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/04/2012 04:25:04 PM

#ifndef MICEX_DRIVER_PRECOMP_H
#define MICEX_DRIVER_PRECOMP_H

#include <ei_cxx/log_wrapper.h>

#include <list>
#include <string>
#include <exception>

#include <boost/cstdint.hpp>

#define NOMINMAX

#include <Windows.h>

namespace
{

int const MTE_ERRMSG_SIZE = 256 + 1;

typedef std::map<std::string, std::string> InValues;

void setLogLevel(LogLevel::type_t llevel);

std::string get_string(char const*& buff)
{
   boost::int32_t sz = *(boost::int32_t*)buff;
   std::string res(buff + sizeof(boost::int32_t), sz);
   buff += (sz + sizeof(boost::int32_t));
   return res;
}

boost::int32_t get_int32(char const*& buff)
{
   boost::int32_t res *(boost::int32_t*)buff;
   buff += sizeof(boost::int32);
   return res;
}

class MteError : public std::exception
{
public:
   MteError(int err, std::sring const& descr) : m_error(err), m_what(descr)
   {
   }
   int error() const { return m_error; }
   virtual char const* what() const { return m_what.c_str(); }
priate:
   int m_error;
   std::string m_what;
};

} // namespace

#endif // MICEX_DRIVER_PRECOMP_H

/// @file   binary.cc
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 04/11/2011 09:59:06 PM

#include "../include/ei_cxx/ibinary.h"
#include "../include/ei_cxx/atom.h"
#include "../include/ei_cxx/exception.h"
#include "../include/ei_cxx/tuple.h"
#include "../include/ei_cxx/list.h"

#include <boost/cstdint.hpp>

#define EI_CXX_DECODE "decode"

namespace ei_cxx
{

namespace
{

bool is_bigendian()
{
   static int const i = 1;
   return *(char*)&i == 0;
}

} // namespace

//------------------------------------------------------------------------------------------------------------------------//
IBinary::IBinary() : m_erlBin(NULL), m_index(0), m_buff(NULL), m_len(0)
{
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary::~IBinary()
{
   if (m_erlBin)
   {
      driver_binary_dec_refc(m_erlBin);
   }
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary::IBinary(ErlDrvBinary* bin) : m_erlBin(bin), m_index(0), m_buff(NULL), m_len(bin->orig_size)
{
   if (!bin)
   {
      throw std::invalid_argument("IBinary::IBinary(ErlDrvBinary) argument is NULL.");
   }
   driver_binary_inc_refc(bin);
   int ver = 0;
   CHECK(ei_decode_version(get_buff(), &m_index, &ver), EI_CXX_DECODE, "version", "");
   int size = 0;
   CHECK(ei_get_type(get_buff(), &m_index, &m_type, &size), EI_CXX_DECODE, "binary", "unable to get type");
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary::IBinary(char* buff, size_t len) : m_erlBin(NULL), m_index(0), m_buff(buff), m_len(len)
{
   if (!buff || len == 0)
   {
      throw std::invalid_argument("IBinary::IBinary(char*, size_t) invalid argument");
   }
   int ver = 0;
   CHECK(ei_decode_version(get_buff(), &m_index, &ver), EI_CXX_DECODE, "version", "");
   int size = 0;
   CHECK(ei_get_type(get_buff(), &m_index, &m_type, &size), EI_CXX_DECODE, "binary", "unable to get type");
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary& IBinary::operator >> (bool& val)
{
   int tmp = 0;
   CHECK(ei_decode_boolean(get_buff(), &m_index, &tmp), EI_CXX_DECODE, "bool", "");
   val = (tmp == 0) ? false : true;
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary& IBinary::operator >> (long& val)
{
   CHECK(ei_decode_long(get_buff(), &m_index, &val), EI_CXX_DECODE, "long", "");
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary& IBinary::operator >> (unsigned long& val)
{
   CHECK(ei_decode_ulong(get_buff(), &m_index, &val), EI_CXX_DECODE, "ulong", "");
   return *this;
}

#ifdef EI_CXX_ENABLE_INT64
//------------------------------------------------------------------------------------------------------------------------//
IBinary& IBinary::operator >> (boost::int64_t& val)
{
   CHECK(ei_decode_longlong(get_buff(), &m_index, &val), EI_CXX_DECODE, "longlong", "");
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary& IBinary::operator >> (boost::uint64_t& val)
{
   CHECK(ei_decode_ulonglong(get_buff(), &m_index, &val), EI_CXX_DECODE, "ulonglong", "");
   return *this;
}
#endif // EI_CXX_ENABLE_INT64

//------------------------------------------------------------------------------------------------------------------------//
IBinary& IBinary::operator >> (double& val)
{
   CHECK(ei_decode_double(get_buff(), &m_index, &val), EI_CXX_DECODE, "double", "");
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary& IBinary::operator >> (unsigned char& val)
{
   CHECK(ei_decode_char(get_buff(), &m_index, reinterpret_cast<char*>(&val)), EI_CXX_DECODE, "unsigned char", "");
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary& IBinary::operator >> (std::string& val)
{
   char buff[65535];
   size_t const start = m_index;
   CHECK(ei_decode_string(get_buff(), &m_index, buff), EI_CXX_DECODE, "string", "");
   val = buff;
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary& IBinary::operator >> (Atom& val)
{
   char buff[255];
   size_t const start = m_index;

   CHECK(ei_decode_atom(get_buff(), &m_index, buff), EI_CXX_DECODE, "atom", "");
   val.set(std::string(buff, m_index - start - ATOM_SIZE));
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary& IBinary::operator >> (bytes& val)
{
   int type = 0;
   int size = 0;
   char* buff = get_buff();
   CHECK(ei_get_type(buff, &m_index, &type, &size), EI_CXX_DECODE, "binary", "unable to get type");
   if (type == ErlBitBinary) // bit-binary
   {
      if (is_bigendian())
      {
		  size = *(boost::int32_t*)(buff + m_index + 1);
      }
      else
      {
         size = *(buff + m_index + 1) << 24 |
                *(buff + m_index + 2) << 16 |
                *(buff + m_index + 3) << 8 |
                *(buff + m_index + 4);
      }
      int used_bits = *(buff + m_index + BIT_BINARY_SIZE - 1);
      val.resize(size);
      memcpy(&val[0], buff + m_index + BIT_BINARY_SIZE, size);
      val[size - 1] = val[size - 1] >> (8 - used_bits);
      m_index += BIT_BINARY_SIZE + size;
   }
   else
   {
      val.resize(size);
      CHECK(
            ei_decode_binary(
               buff, &m_index, &val[0], reinterpret_cast<long*>(&size)), EI_CXX_DECODE, "binary", "unable to decode");
   }
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary& IBinary::operator >> (ITuple& val)
{
   int old_index = m_index;
   CHECK(ei_skip_term(get_buff(), &m_index), EI_CXX_DECODE, "tuple", "unable to skip term");
   val.set_buff(m_erlBin, m_buff, old_index, m_index);
   val.set_type(get_type());
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary& IBinary::operator >> (IList& val)
{
   int old_index = m_index;
   CHECK(ei_skip_term(get_buff(), &m_index), EI_CXX_DECODE, "list", "unable to skip term");
   val.set_buff(m_erlBin, m_buff, old_index, m_index);
   val.set_type(get_type());
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
IBinary& IBinary::operator >> (bpt::ptime& val)
{
   ITuple now;
   *this >> now;

   long msec = 0;
   long sec = 0;
   long microsec = 0;
   now >> msec >> sec >> microsec;

   val = val + bpt::seconds(msec * 1000000 + sec) + bpt::microseconds(microsec);

   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
void IBinary::set_buff(ErlDrvBinary* erlBin, char* buff, size_t index, size_t len, bool decode_ver)
{
   if (m_erlBin)
   {
      driver_binary_inc_refc(erlBin);
   }
   m_erlBin = erlBin;
   m_buff = buff;
   m_index = index;
   m_len = len;

   if (decode_ver)
   {
      int ver = 0;
      CHECK(ei_decode_version(get_buff(), &m_index, &ver), EI_CXX_DECODE, "version", "");
   }
}

//------------------------------------------------------------------------------------------------------------------------//
char* IBinary::get_buff() const
{
   if (m_index >= m_len)
   {
      THROW(std::runtime_error, "Nothing to decode. Binary has been decoded completely.");
   }
   char* buff = (m_erlBin == NULL) ? m_buff : m_erlBin->orig_bytes;
   if (!buff)
   {
      THROW(std::invalid_argument, "Binary is NULL.");
   }
   return buff;
}

//------------------------------------------------------------------------------------------------------------------------//
int IBinary::decode_tuple_header()
{
   int arity = 0;
   CHECK(ei_decode_tuple_header(get_buff(), &m_index, &arity), EI_CXX_DECODE, "tuple header", "");
   return arity;
}

//------------------------------------------------------------------------------------------------------------------------//
bool IBinary::decode_list_header(int& size)
{
   int type = 0;
   CHECK(ei_get_type(get_buff(), &m_index, &type, &size), EI_CXX_DECODE, "binary:decode_list_header", "unable to get type");
   if (type == ErlList)
   {
      CHECK(ei_decode_list_header(get_buff(), &m_index, &size), EI_CXX_DECODE, "list header", "");
      return true;
   }
   return false;
}

} // namespace ei_cxx

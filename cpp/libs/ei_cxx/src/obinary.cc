/// @file   OBinary.cc
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 04/11/2011 09:59:06 PM

#include "../include/ei_cxx/obinary.h"
#include "../include/ei_cxx/atom.h"
#include "../include/ei_cxx/tuple.h"
#include "../include/ei_cxx/port.h"
#include "../include/ei_cxx/list.h"

namespace ei_cxx
{

size_t const BIN_SIZE = 1024;


//------------------------------------------------------------------------------------------------------------------------//
OBinary::OBinary() : m_erlBin(NULL), m_index(0)
{
   char* buff = get_buff(VER_SIZE);
   CHECK(ei_encode_version(buff, &m_index), EI_CXX_ENCODE, "ver = ", 131);
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary::OBinary(OBinary const& rhs) : m_erlBin(NULL), m_index(0)
{
   copy(rhs);
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary::~OBinary()
{
   if (m_erlBin)
   {
      driver_free_binary(m_erlBin);
   }
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator = (OBinary const& rhs)
{
   copy(rhs);
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
void OBinary::copy(OBinary const& rhs)
{
   m_index = rhs.m_index;
   if (m_index <= sizeof(m_heapBuff))
   {
      memcpy(m_heapBuff, rhs.m_heapBuff, sizeof(m_heapBuff));
   }
   else if (rhs.m_erlBin)
   {
      m_erlBin = driver_alloc_binary(rhs.m_erlBin->orig_size);
      memcpy(m_erlBin->orig_bytes, rhs.m_erlBin->orig_bytes, m_index);
   }
}

//------------------------------------------------------------------------------------------------------------------------//
void OBinary::encode_tuple_header(int arity)
{
   char* buff = get_buff(arity <= 0xff ? SMALL_TUPLE_EXT_SIZE : LARGE_TUPLE_EXT_SIZE);
   CHECK(ei_encode_tuple_header(buff, &m_index, arity), EI_CXX_ENCODE, "tuple_header = ", arity);
}

//------------------------------------------------------------------------------------------------------------------------//
void OBinary::encode_list_header(int size)
{
   char* buff = get_buff(LIST_SIZE);
   CHECK(ei_encode_list_header(buff, &m_index, size), EI_CXX_ENCODE, "list_header = ", size);
}

//------------------------------------------------------------------------------------------------------------------------//
void OBinary::encode_empty_list()
{
   char* buff = get_buff(NIL_SIZE);
   CHECK(ei_encode_empty_list(buff, &m_index), EI_CXX_ENCODE, "nil", "");
}

//------------------------------------------------------------------------------------------------------------------------//
char* OBinary::get_buff(size_t sz)
{
   if (m_index + sz <= sizeof(m_heapBuff))
   {
      return m_heapBuff;
   }
   else if (!m_erlBin)
   {
      m_erlBin = driver_alloc_binary(BIN_SIZE);
      memcpy(m_erlBin->orig_bytes, m_heapBuff, m_index);
   }
   else if (m_index + sz > m_erlBin->orig_size)
   {
      m_erlBin = driver_realloc_binary(m_erlBin, m_erlBin->orig_size + BIN_SIZE);
   }
   return m_erlBin->orig_bytes;
}

//------------------------------------------------------------------------------------------------------------------------//
char const* OBinary::get_buff() const
{
   if (m_erlBin)
   {
      return m_erlBin->orig_bytes;
   }
   return m_heapBuff;
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (bool val)
{
   check();
   char* buff = get_buff(INT_SIZE);
   CHECK(ei_encode_boolean(buff, &m_index, val == true ? 1 : 0), EI_CXX_ENCODE, "bool = ", val);
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (long val)
{
   check();
   int index = 0;
   char tmp[32];
   CHECK(ei_encode_long(tmp, &index, val), EI_CXX_ENCODE, "long = ", val);
   char* buff = get_buff(index);
   memcpy(buff + m_index, tmp, index);
   m_index += index;
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (unsigned long val)
{
   check();
   int index = 0;
   char tmp[32];
   CHECK(ei_encode_ulong(tmp, &index, val), EI_CXX_ENCODE, "ulong = ", val);
   char* buff = get_buff(index);
   memcpy(buff + m_index, tmp, index);
   m_index += index;
   return *this;
}


#if defined(EI_CXX_ENABLE_INT64) && defined(__WIN32__)
//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (boost::int64_t val)
{
   check();
   int index = 0;
   char tmp[32];
   CHECK(ei_encode_longlong(tmp, &index, val), EI_CXX_ENCODE, "int64 = ", val);
   char* buff = get_buff(index);
   memcpy(buff + m_index, tmp, index);
   m_index += index;
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (boost::uint64_t val)
{
   check();
   int index = 0;
   char tmp[32];
   CHECK(ei_encode_ulonglong(tmp, &index, val), EI_CXX_ENCODE, "uint64 = ", val);
   char* buff = get_buff(index);
   memcpy(buff + m_index, tmp, index);
   m_index += index;
   return *this;
}

#endif // EI_CXX_ENABLE_INT64

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (double val)
{
   check();
   int index = 0;
   char tmp[32];
   CHECK(ei_encode_double(tmp, &index, val), EI_CXX_ENCODE, "doube = ", val);
   char* buff = get_buff(index);
   memcpy(buff + m_index, tmp, index);
   m_index += index;
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (char val)
{
   check();
   char* buff = get_buff(CHAR_SIZE);
   CHECK(ei_encode_char(buff, &m_index, val), EI_CXX_ENCODE, "char = ", val);
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (std::string const& val)
{
   check();
   if (val.length() > 65535)
   {
      THROW(std::runtime_error, "Too long string to encode.");
   }
   char* buff = get_buff(STRING_SIZE + val.length());
   CHECK(ei_encode_string(buff, &m_index, val.c_str()), EI_CXX_ENCODE, "string = ", val);
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (Atom const& val)
{
   check();
   char* buff = get_buff(ATOM_SIZE + val.get().length());
   CHECK(
      ei_encode_atom(buff, &m_index, val.get().c_str()),
      EI_CXX_ENCODE,
      "atom = ", val.get());
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (OBinary const& val)
{
   check();
   if (val.m_index == 0)
   {
      THROW(std::runtime_error, "Unable to encode empty tuple");
   }
   int startFrom = 0;
   int ver = 0;
   char const* tbuff = val.get_buff();
   ei_decode_version(tbuff, &startFrom, &ver); // skip version

   char* buff = get_buff(val.m_index - startFrom);
   memcpy(buff + m_index, tbuff + startFrom, val.m_index - startFrom);
   m_index += val.m_index - startFrom;
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (bytes const& val)
{
   check();
   char* buff = get_buff(BINARY_SIZE + val.size());
   CHECK(ei_encode_binary(buff, &m_index, &val[0], val.size()), EI_CXX_ENCODE, "binary size = ", val.size());
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (bpt::ptime const& val)
{
   namespace bg = boost::gregorian;
   check();
   static bpt::ptime epoc(bg::date(1970,1,1));
   bpt::time_duration td;
   if (epoc < val)
   {
      td = val - epoc;
   }
   else
   {
      td = val.time_of_day();
   }
   OTuple etime(3); // now: {MSec, Sec, Microsec}
   etime << (long)(td.total_seconds() / 1000000)
         << (long)(td.total_seconds() % 1000000)
         << (long)td.fractional_seconds();
   *this << etime;
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (OTuple const& val)
{
   operator << (dynamic_cast<OBinary const&>(val));
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
OBinary& OBinary::operator << (OList const& val)
{
   operator << (dynamic_cast<OBinary const&>(val));
   encode_empty_list();
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
void OBinary::send(Port& p)
{
   if (m_erlBin)
   {
      p.send(m_erlBin, m_index);
   }
   else
   {
      p.send(m_heapBuff, m_index);
   }
}

} // namespace ei_cxx

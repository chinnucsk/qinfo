/// @file   tuple.cc
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 03/26/2011 11:57:00 AM

#include "ei_cxx/tuple.h"

namespace ei_cxx
{

//------------------------------------------------------------------------------------------------------------------------//
ITuple::ITuple()
{
}

//------------------------------------------------------------------------------------------------------------------------//
ITuple::ITuple(ErlDrvBinary* erlBin) : IBinary(erlBin)
{
   m_arity = IBinary::decode_tuple_header();
}

//------------------------------------------------------------------------------------------------------------------------//
ITuple::ITuple(char* buff, size_t len) : IBinary(buff, len)
{
   m_arity = IBinary::decode_tuple_header();
}

//------------------------------------------------------------------------------------------------------------------------//
void ITuple::set_buff(ErlDrvBinary* erlBin, char* buff, size_t index, size_t len)
{
   IBinary::set_buff(erlBin, buff, index, len, false);
   m_arity = IBinary::decode_tuple_header();
}

//------------------------------------------------------------------------------------------------------------------------//
OTuple::OTuple(int arity) : m_arity(arity), m_encoded(0)
{
   OBinary::encode_tuple_header(m_arity);
}

//------------------------------------------------------------------------------------------------------------------------//
void OTuple::check()
{
   if (m_encoded == m_arity)
   {
      THROW(std::runtime_error, "Unable to encode more.");
   }
   ++m_encoded;
}

} // namespace EiCxx

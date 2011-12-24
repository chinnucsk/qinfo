/// @file   list.cc
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 03/26/2011 11:57:00 AM

#include "../include/ei_cxx/list.h"

namespace ei_cxx
{

//------------------------------------------------------------------------------------------------------------------------//
IList::IList() : m_decoded(0)
{
}

//------------------------------------------------------------------------------------------------------------------------//
IList::IList(ErlDrvBinary* erlBin) : IBinary(erlBin), m_decoded(0)
{
   if (!IBinary::decode_list_header(m_size))
   {
      IBinary::operator >> (m_buff);
   }
}

//------------------------------------------------------------------------------------------------------------------------//
IList::IList(char* buff, size_t len) : IBinary(buff, len), m_decoded(0)
{
   if (!IBinary::decode_list_header(m_size))
   {
      IBinary::operator >> (m_buff);
   }
}

//------------------------------------------------------------------------------------------------------------------------//
void IList::set_buff(ErlDrvBinary* erlBin, char* buff, size_t index, size_t len)
{
   IBinary::set_buff(erlBin, buff, index, len, false);
   if (!IBinary::decode_list_header(m_size))
   {
      IBinary::operator >> (m_buff);
   }
}

//------------------------------------------------------------------------------------------------------------------------//
IList& IList::operator >> (long& val)
{
   if (m_buff.empty())
   {
      IBinary::operator >> (val);
   }
   else
   {
      if (m_decoded == m_size)
      {
         THROW(std::runtime_error, "Nothing to decode in IList.");
      }
      val = m_buff[m_decoded++];
   }
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
IList& IList::operator >> (unsigned long& val)
{
   if (m_buff.empty())
   {
      IBinary::operator >> (val);
   }
   else
   {
      if (m_decoded == m_size)
      {
         THROW(std::runtime_error, "Nothing to decode in IList.");
      }
      val = m_buff[m_decoded++];
   }
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
IList& IList::operator >> (unsigned char& val)
{
   if (m_buff.empty())
   {
      IBinary::operator >> (val);
   }
   else
   {
      if (m_decoded == m_size)
      {
         THROW(std::runtime_error, "Nothing to decode in IList.");
      }
      val = m_buff[m_decoded++];
   }
   return *this;
}

//------------------------------------------------------------------------------------------------------------------------//
OList::OList(int size) : m_size(size), m_encoded(0)
{
   OBinary::encode_list_header(m_size);
}

//------------------------------------------------------------------------------------------------------------------------//
void OList::check()
{
   if (m_encoded == m_size)
   {
      THROW(std::runtime_error, "Unable to encode more.");
   }
   ++m_encoded;
}

//------------------------------------------------------------------------------------------------------------------------//
void OList::send(Port& p)
{
   if (m_encoded < m_size)
   {
      THROW(std::runtime_error, FMT("Not all list elements were encoded. Encoded %1%, but list size %2%.", m_encoded % m_size));
   }
   encode_empty_list();
   OBinary::send(p);
}

} // namespace EiCxx

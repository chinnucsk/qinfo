/// @file   binary.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 04/11/2011 09:50:49 PM

#ifndef EI_CXX_OBINARY_H
#define EI_CXX_OBINARY_H

#include "common.h"

#include <ei.h>
#include <erl_driver.h>

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/cstdint.hpp>

#include <string>
#include <list>

#define EI_CXX_ENCODE "encode"

namespace ei_cxx
{

class OTuple;
class Atom;
class Port;
class OList;

namespace bpt = boost::posix_time;

class OBinary
{
public:
   OBinary();
   ~OBinary();
   OBinary(OBinary const& rhs);
   OBinary& operator = (OBinary const& rhs);
public:
   virtual OBinary& operator << (bool val);
   virtual OBinary& operator << (long val);
   virtual OBinary& operator << (unsigned long val);
#ifdef EI_CXX_ENABLE_INT64
   virtual OBinary& operator << (boost::int64_t val);
   virtual OBinary& operator << (boost::uint64_t val);
#endif // EI_CXX_ENABLE_INT64
   virtual OBinary& operator << (double val);
   virtual OBinary& operator << (char val);
   virtual OBinary& operator << (std::string const& val);
   virtual OBinary& operator << (Atom const& val);
   virtual OBinary& operator << (OBinary const& val);
   virtual OBinary& operator << (bytes const& val);
   virtual OBinary& operator << (OTuple const& val);
   virtual OBinary& operator << (bpt::ptime const& newVal);
   virtual OBinary& operator << (OList const& lst);
   virtual void send(Port& p);
protected:
   void copy(OBinary const& rhs);
   virtual void check() {}
   void encode_tuple_header(int arity);
   void encode_list_header(int size);
   void encode_empty_list();
   char* get_buff(size_t sz = 0);
   char const* get_buff() const;
private:
   ErlDrvBinary*  m_erlBin;
   char           m_heapBuff[64];
   int            m_index;
};

} // namespace ei_cxx

#endif // EI_CXX_OBINARY_H

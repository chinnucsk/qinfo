/// @file   binary.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 04/11/2011 09:50:49 PM

#ifndef EI_CXX_IBINARY_H
#define EI_CXX_IBINARY_H

#include "common.h"

#include <ei.h>
#include <erl_driver.h>

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/cstdint.hpp>

#include <string>
#include <list>
#include <vector>

namespace ei_cxx
{

class ITuple;
class Atom;
class IList;

namespace bpt = boost::posix_time;

class IBinary
{
public:
   IBinary();
   ~IBinary();
   explicit IBinary(ErlDrvBinary* bin);
   IBinary(char* buff, size_t len);
public:
   int get_type() const { return m_type; }
   virtual IBinary& operator >> (bool& val);
   virtual IBinary& operator >> (long& val);
   virtual IBinary& operator >> (unsigned long& val);
#if defined(EI_CXX_ENABLE_INT64) && defined(__WIN32__)
   virtual IBinary& operator >> (boost::int64_t& val);
   virtual IBinary& operator >> (boost::uint64_t& val);
#endif // EI_CXX_ENABLE_INT64
   virtual IBinary& operator >> (double& val);
   virtual IBinary& operator >> (unsigned char& val);
   virtual IBinary& operator >> (std::string& val);
   virtual IBinary& operator >> (Atom& val);
   virtual IBinary& operator >> (bytes& val);
   virtual IBinary& operator >> (ITuple& val);
   virtual IBinary& operator >> (bpt::ptime& newVal);
   virtual IBinary& operator >> (IList& val);
public:
  static int const ErlSmallInt     = 97;
  static int const ErlInteger      = 98;
  static int const ErlFloat        = 99;
  static int const ErlAtom         = 100;
  static int const ErlSmallTuple   = 104;
  static int const ErlLargeTuple   = 105;
  static int const ErlNil          = 106;
  static int const ErlString       = 107;
  static int const ErlList         = 108;
  static int const ErlBinary       = 109;
  static int const ErlBitBinary    = 77;
protected:
   char* get_buff() const;
   virtual void set_buff(ErlDrvBinary* erlBin, char* buff, size_t index, size_t len, bool decode_ver = true);
   int decode_tuple_header();
   bool decode_list_header(int& size);
   void set_type(int type) { m_type = type; }
private:
   ErlDrvBinary*  m_erlBin;
   char*          m_buff;
   size_t         m_len;
   int            m_index;
   int            m_type;
};

} // namespace ei_cxx

#endif // EI_CXX_IBINARY_H

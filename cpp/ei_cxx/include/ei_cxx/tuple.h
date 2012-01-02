// @file   tuple.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 03/26/2011 11:51:34 AM

#ifndef EI_CXX_TUPLE_H
#define EI_CXX_TUPLE_H

#include "ibinary.h"
#include "obinary.h"

#include <erl_driver.h>

namespace ei_cxx
{

class ITuple : public IBinary
{
   friend class IBinary;
public:
   ITuple();
   explicit ITuple(ErlDrvBinary* erlBin);
   ITuple(char* buff, size_t len);
public:
   int getArity() const { return m_arity; }
protected:
   virtual void set_buff(ErlDrvBinary* erlBin, char* buff, size_t index, size_t len);
private:
   int m_arity;
};

class OTuple : public OBinary
{
public:
   explicit OTuple(int arity);
protected:
   virtual void check();
private:
   int const   m_arity;
   int         m_encoded;
};

} // namespace ei_cxx

#endif // EI_CXX_TUPLE_H

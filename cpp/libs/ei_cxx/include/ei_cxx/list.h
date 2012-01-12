/// @file   list.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 03/26/2011 11:51:34 AM

#ifndef EI_CXX_LIST_H
#define EI_CXX_LIST_H

#include "ibinary.h"
#include "obinary.h"

#include <erl_driver.h>

#include <string>
#include <list>

namespace ei_cxx
{

class IList : public IBinary
{
   friend class IBinary;
public:
   IList();
   explicit IList(ErlDrvBinary* erlBin);
   IList(char* buff, size_t len);
public:
   int size() const { return m_size; }
   using IBinary::operator >>;
   virtual IList& operator >> (long& val);
   virtual IList& operator >> (unsigned long& val);
   virtual IList& operator >> (unsigned char& val);
   template<typename T>
   std::list<T> to_std_list();
protected:
   virtual void set_buff(ErlDrvBinary* erlBin, char* buff, size_t index, size_t len);
private:
   int m_size;
   int m_decoded;
   std::string m_buff;
};

template<typename T>
std::list<T> IList::to_std_list()
{
   std::list<T> reply;
   for(int i = 0; i < size(); ++i)
   {
      T item;
      operator >> (item);
      reply.push_back(item);
   }
   return reply;
}

class OList : public OBinary
{
public:
   explicit OList(int size);
   virtual void send(Port& p);
   template<typename T>
   static OList from_std_list(std::list<T> const& lst);
protected:
   virtual void check();
private:
   int const   m_size;
   int         m_encoded;
};

template<typename T>
OList OList::from_std_list(std::list<T> const& lst)
{
   OList reply(lst.size());
   for(typename std::list<T>::const_iterator it = lst.begin(); it != lst.end(); ++it)
   {
      reply << *it;
   }
   return reply;
}

} // namespace ei_cxx

#endif // EI_CXX_LIST_H

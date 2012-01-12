/// @file   Atom.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 04/11/2011 10:00:52 PM

#ifndef EI_CXX_ATOM_H
#define EI_CXX_ATOM_H

#include <string>

namespace ei_cxx
{

/// @class Atom
/// @brief Atom wrapper
class Atom
{
public:
   Atom() {}
   explicit Atom(std::string const& value) : m_value(value) {}
   std::string const& get() const { return m_value; }
   void set(std::string const& newVal) { m_value = newVal; }
private:
   std::string m_value;
};

} // namespace

#endif // EI_CXX_ATOM_H

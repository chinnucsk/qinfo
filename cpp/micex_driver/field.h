/// @file   field.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/05/2012 08:48:44 AM

#ifndef MICEX_DRIVER_FIELD_H
#define MICEX_DRIVER_FIELD_H

namespace micex
{

class Field
{
public:
   static void skip(char const*& buff);
};

class InField : public Field
{
public:
   static void skip(char const*& buff);
};

class OutField : public Field
{
public:
   static void skip(char const*& buff);
};

} // namespace micex

#endif // MICEX_DRIVER_FIELD_H

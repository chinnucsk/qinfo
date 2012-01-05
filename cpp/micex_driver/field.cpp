/// @file   field.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/05/2012 08:51:45 AM

#include "field.h"

namespace micex
{

//---------------------------------------------------------------------------------------------------------------------//
void Field::skip(char const*& buff)
{
   get_string(buff); // name
   get_string(buff); // description
   get_int32(buff);  // size
   get_int32(buff);  // type
   get_int32(buff);  // attributes
   get_string(buff); // enum
}

//---------------------------------------------------------------------------------------------------------------------//
void InField::skip(char const*& buff)
{
   Field::skip(buff);
   get_string(buff); // default value
}

//---------------------------------------------------------------------------------------------------------------------//
void OutField::skip(char const*& buff)
{
   Field::skip(buff);
}

} // namespace micex

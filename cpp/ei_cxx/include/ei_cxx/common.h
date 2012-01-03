/// @file   common.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 04/17/2011 09:10:03 PM

#ifndef QINFO_COMMON_H
#define QINFO_COMMON_H

#include <common/exception.h>

namespace ei_cxx
{

#define CHECK(expr, action, that, val) \
{ \
   int const err = expr; \
   if (err < 0) \
   { \
      THROW(std::runtime_error, FMT("Unable to %1% %2%. Reason = <%3%>.", action % that % val)); \
   } \
}

size_t const INT_SIZE = 1 + 4;
size_t const CHAR_SIZE = 1 + 1;
size_t const STRING_SIZE = 1 + 2;
size_t const ATOM_SIZE = 1 + 2;
size_t const LIST_SIZE = 1 + 4;
size_t const NIL_SIZE = 1;
size_t const SMALL_TUPLE_EXT_SIZE = 1 + 1;
size_t const LARGE_TUPLE_EXT_SIZE = 1 + 4;
size_t const VER_SIZE = 1;
size_t const BINARY_SIZE = 1 + 4;
size_t const BIT_BINARY_SIZE = 1 + 4 + 1;

typedef std::vector<unsigned char> bytes;

} // namespace ei_cxx

#endif // EI_CXX_COMMON_H

/// @file   common_utils.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/24/2012 05:09:02 PM

#ifndef QINFO_COMMON_UTILS_H
#define QINFO_COMMON_UTILS_H

#include <erl_nif.h>

#ifdef __WIN32__
#   ifdef QINFO_COMMON_UTILS_EXPORTS
#      define QINFO_COMMON_UTILS_API __declspec(dllexport)
#   else
#      define QINFO_COMMON_UTILS_API __declspec(dllimport)
#   endif
#else
#   define QINFO_COMMON_UTILS_API static
#endif

#ifdef __WIN32__
extern "C"
{

extern QINFO_COMMON_UTILS_API ErlNifFunc nif_funcs[];

}
#endif

#endif // QINFO_COMMON_UTILS_H

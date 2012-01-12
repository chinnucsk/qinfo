/// @file   micex_driver_dll.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/04/2012 08:49:47 AM

#include <erl_driver.h>

#ifdef MICEX_DRIVER_DLL_EXPORTS
#define MICEX_DRIVER_DLL_API __declspec(dllexport)
#else
#define MICEX_DRIVER_DLL_API __declspec(dllimport)
#endif

extern "C"
{

extern MICEX_DRIVER_DLL_API ErlDrvEntry micex_driver_entry;

MICEX_DRIVER_DLL_API DRIVER_INIT(micex_driver) /* must match name in driver_entry */
{
    return &micex_driver_entry;
}

}

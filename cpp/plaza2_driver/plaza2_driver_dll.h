/// @file   plaza2_driver_dll.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/11/2012 09:31:15 PM

#include <erl_driver.h>

#ifdef PLAZA2_DRIVER_DLL_EXPORTS
#define PLAZA2_DRIVER_DLL_API __declspec(dllexport)
#else
#define PLAZA2_DRIVER_DLL_API __declspec(dllimport)
#endif

extern "C"
{

extern PLAZA2_DRIVER_DLL_API ErlDrvEntry plaza2_driver_entry;

PLAZA2_DRIVER_DLL_API DRIVER_INIT(plaza2_driver) /* must match name in driver_entry */
{
    return &plaza2_driver_entry;
}

}

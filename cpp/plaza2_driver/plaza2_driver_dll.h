// The following ifdef block is the standard way of creating macros which make exporting
// from a DLL simpler. All files within this DLL are compiled with the CDP_WRAPPER_DLL_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see
// CDP_WRAPPER_DLL_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.

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

/// @file   mtesrl_driver_dll.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/04/2012 08:49:47 AM

#include <erl_driver.h>

#ifdef MTESRL_DRIVER_DLL_EXPORTS
#define MTESRL_DRIVER_DLL_API __declspec(dllexport)
#else
#define MTESRL_DRIVER_DLL_API __declspec(dllimport)
#endif

extern "C"
{

extern MTESRL_DRIVER_DLL_API ErlDrvEntry mtesrl_driver_entry;

MTESRL_DRIVER_DLL_API DRIVER_INIT(mtesrl_driver) /* must match name in driver_entry */
{
    return &mtesrl_driver_entry;
}

}

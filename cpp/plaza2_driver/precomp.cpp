#include "precomp.h"
#include <ei_cxx/log_wrapper.h>

#ifdef _ATL_STATIC_REGISTRY
#include <statreg.h>
#endif


ATL::CComModule g_module;

LogLevel::type_t log_level = LogLevel::info;

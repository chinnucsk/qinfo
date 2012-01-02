#include "precomp.h"
#include "common.h"

#ifdef _ATL_STATIC_REGISTRY
#include <statreg.h>
#endif


ATL::CComModule g_module;

LogLevel::type_t log_level = LogLevel::info;

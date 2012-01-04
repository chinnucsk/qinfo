/// @file   precomp.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/04/2012 04:25:04 PM

#ifndef MICEX_DRIVER_PRECOMP_H
#define MICEX_DRIVER_PRECOMP_H

#define _WIN32_WINNT 0x0501

#include <ei_cxx/log_wrapper.h>

#define NOMINMAX

#include <Windows.h>

namespace
{

int const MTE_ERRMSG_SIZE = 256 + 1;
int const MTE_CONNPARAMS_SIZE = 2048 + 1;

} // namespace


#endif // MICEX_DRIVER_PRECOMP_H

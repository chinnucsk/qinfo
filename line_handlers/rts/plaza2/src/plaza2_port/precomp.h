#pragma once

#include <sstream>

#define _WIN32_WINNT 0x0501

#include <atlbase.h>
#include <atlcom.h>
#include <atldbcli.h>

#include <exception>
#include <sstream>
#include <string>

#define WIN32_LEAN_AND_MEAN
#define VC_EXTRALEAN
#define NOMINMAXm_bInfoAvailable

#include <Windows.h>

#import "tlb/P2ClientGateMTA.tlb" named_guids

#define _ATL_FREE_THREADED

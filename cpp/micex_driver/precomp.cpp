/// @file   precomp.cpp
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/04/2012 04:27:05 PM

#include "precomp.h"

LogLevel::type_t log_level;

void setLogLevel(LogLevel::type_t llevel)
{
   log_level = llevel;
}

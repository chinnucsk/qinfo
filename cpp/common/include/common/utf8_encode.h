/// @file   utf8_encode.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 06/24/2011 06:28:04 PM
/// @version $Id: Exp $

#ifndef COMMON_UTF8_ENCODE_H
#define COMMON_UTF8_ENCODE_H

#include <string>
#include <list>

std::list<long> cp1251_to_utf8(std::string const& str);

#endif // COMMON_UTF8_ENCODE_H

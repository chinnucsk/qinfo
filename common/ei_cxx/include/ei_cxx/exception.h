/// @file   exception.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 03/26/2011 01:46:31 PM

#ifndef EI_CXX_EXCEPTION_H
#define EI_CXX_EXCEPTION_H

#include <sstream>
#include <boost/format.hpp>

#define FMT(fmt, args) boost::str(boost::format(fmt) % args).c_str()
#define THROW(ex, txt) \
{ \
   std::ostringstream qf_port_ost; \
   qf_port_ost << __FILE__ << "(" << __LINE__ << "). " << txt ;\
   throw ex(qf_port_ost.str()); \
}

#endif // EI_CXX_EXCEPTION_H

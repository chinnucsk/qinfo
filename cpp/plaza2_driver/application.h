/// @file   application.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/11/2012 09:30:05 PM


#ifndef PLAZA2_DRIVER_APPLICATION_H
#define PLAZA2_DRIVER_APPLICATION_H

#include <boost/noncopyable.hpp>

#include <string>

namespace Plaza2
{

using namespace P2ClientGateMTA;

class Application : private boost::noncopyable
{
public:
   explicit Application(std::string const& iniFile);
   operator bool () const;
   ~Application();
private:
   IP2ApplicationPtr m_app;
};

} // namespace Plaza2


#endif // PLAZA2_DRIVER_APPLICATION_H

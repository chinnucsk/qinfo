#ifndef PLAZA2_DRIVER_APPLICATION_H
#define PLAZA2_DRIVER_APPLICATION_H

#include <boost/noncopyable.hpp>

#include <string>

namespace Plaza2
{

using namespace P2ClientGateMTA;

class Application : private boost::noncopyable
{
private:
	Application();
public:
   static Application* instance();
   void init(std::string const& iniFile);
   operator bool () const;
	~Application();
private:
	IP2ApplicationPtr m_app;
};

} // namespace Plaza2


#endif // PLAZA2_DRIVER_APPLICATION_H

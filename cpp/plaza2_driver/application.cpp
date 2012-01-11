/// @file   application.cpp
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/11/2012 09:28:43 PM

#include "precomp.h"
#include "application.h"

namespace Plaza2
{

//------------------------------------------------------------------------------------------------------------------------//
Application::Application() : m_app(NULL)
{
}

//------------------------------------------------------------------------------------------------------------------------//
Application* Application::instance()
{
   static Application* app = new Application();
   return app;
}

//------------------------------------------------------------------------------------------------------------------------//
void Application::init(std::string const& iniFile)
{
   CoInitializeEx(NULL, COINIT_MULTITHREADED);
   m_app.CreateInstance(CLSID_CP2Application);
   m_app->StartUp(iniFile.c_str());
}

//------------------------------------------------------------------------------------------------------------------------//
Application::operator bool () const
{
   return m_app != NULL;
}

//------------------------------------------------------------------------------------------------------------------------//
Application::~Application()
{
   m_app->CleanUp();
   //CoUninitialize();
   m_app = NULL;
}

} // namespace Plaza2

#include "precomp.h"
#include "connection.h"

#include <ei_cxx/tuple.h>
#include <ei_cxx/atom.h>

#include <boost/bind.hpp>

extern ei_cxx::Port g_port;

namespace Plaza2
{

unsigned long DEF_WAIT_TIMEOUT = 10;

//------------------------------------------------------------------------------------------------------------------------//
Connection::Connection() : m_conn(NULL), m_stop(false), m_connected(false)
{
}

//------------------------------------------------------------------------------------------------------------------------//
void Connection::connect(
   std::string const& host,
   unsigned long port,
   std::string const& appName,
   std::string const& passwd)
{
   if (m_conn)
   {
      ERL_LOG_ERROR(g_port, "Already connected. Do disconnect first.");
      return;
   }
   m_conn.CreateInstance(CLSID_CP2Connection);
   m_conn->AppName = appName.c_str();
   m_conn->Host = host.c_str();
   m_conn->Port = port;
   m_conn->Password = passwd.c_str();
   DispEventAdvise(m_conn);
   m_worker.reset(new boost::thread(boost::bind(&Connection::run, this)));
}

//------------------------------------------------------------------------------------------------------------------------//
Connection* Connection::instance()
{
   static Connection* conn = new Connection();
   return conn;
}

//------------------------------------------------------------------------------------------------------------------------//
void Connection::disconnect()
{
   ERL_LOG_INFO(g_port, "Disconnecting...");
   m_stop = true;
   m_worker->join();
   m_worker.reset(NULL);
   if (m_conn != NULL)
   {
      closeStreams();
      m_conn->Disconnect();
      DispEventUnadvise(m_conn);
      m_conn = NULL;
   }
   ERL_LOG_INFO(g_port, "Disconnected.")
}

//------------------------------------------------------------------------------------------------------------------------//
Connection::~Connection()
{
   try
   {
      disconnect();
   }
   catch(...)
   {
   }
}

//------------------------------------------------------------------------------------------------------------------------//
void Connection::ConnectionStatusChanged(IDispatch* conn, TConnectionStatus status)
{
   if (status & ConnectionStatus::DISCONNECTED)
   {
      m_connected = false;
      m_conn->Disconnect();
      ERL_LOG_INFO(g_port, "Connection status DISCONNECTED.");
   }
   if (status & ConnectionStatus::CONNECTED)
   {
      ERL_LOG_INFO(g_port, "Connection status CONNECTED.");
   }
   if (status & ConnectionStatus::INVALID)
   {
      ERL_LOG_INFO(g_port, "Connection status INVALID.");
      m_conn->Disconnect();
      m_connected = false;
   }
   if (status & ConnectionStatus::BUSY)
   {
      ERL_LOG_INFO(g_port, "Connection status BUSY.");
   }
   if (status & ConnectionStatus::ROUTER_DISCONNECTED)
   {
      ERL_LOG_INFO(g_port, "Connection status ROUTER_DISCONNECTED.");
   }
   if (status & ConnectionStatus::ROUTER_RECONNECTING)
   {
      ERL_LOG_INFO(g_port, "Connection status ROUTER_RECONNECTING.");
   }
   if (status & ConnectionStatus::ROUTER_CONNECTED)
   {
      ERL_LOG_INFO(g_port, "Connection status ROUTER_CONNECTED.");
   }
   if (status & ConnectionStatus::ROUTER_LOGINFAILED)
   {
      ERL_LOG_INFO(g_port, "Connection status ROUTER_LOGINFAILED.");
   }
   if (status & ConnectionStatus::ROUTER_NOCONNECT)
   {
      ERL_LOG_INFO(g_port, "Connection status ROUTER_NOCONNECT.");
   }}

//------------------------------------------------------------------------------------------------------------------------//
void Connection::run()
{
   ERL_LOG_INFO(g_port, "Connecting... appName='" << m_conn->AppName << "', host='" << m_conn->Host << "', port=" << m_conn->Port);
   while(!m_stop)
   {
      try
      {
         if (!m_connected)
         {
            ULONG errClass = 0;
            HRESULT err = m_conn->raw_Connect(&errClass);
            if (err != ErrorCode::P2ERR_OK)
            {
               if (errClass == 0)
               {
                  THROW(TryAgain, "Unable to connect. Error = 0x" << std::hex << err << '.');
               }
               else if (errClass ==  1)
               {
                  THROW(std::runtime_error, "Connection parameters are wrong.");
               }
            }
            m_connected = true;
         }

         reopenStreams();
         unsigned long cookie = 0;
         m_conn->ProcessMessage(&cookie, DEF_WAIT_TIMEOUT);
      }
      catch(TryAgain const& err)
      {
         ERL_LOG_ERROR(g_port, "Unable to connect. Error = " << err.what());
         Sleep(1000);
      }
      catch(std::exception const& err)
      {
         ERL_LOG_ERROR(g_port, "Connection::run error. Error=" << err.what());
      }
      catch(_com_error const& err)
      {
         ERL_LOG_ERROR(g_port, "Connection::run COM error. Error=" << err.Error() << ", Message=" << err.ErrorMessage());
      }
   }
}

//------------------------------------------------------------------------------------------------------------------------//
void Connection::addStream(std::string const& streamName, std::string const& iniFile, StreamType::type_t st)
{
   StreamPtr stream(new Stream(streamName, iniFile, st));
   m_streams.push_back(stream);
}


//------------------------------------------------------------------------------------------------------------------------//
Connection::operator bool () const
{
   return m_conn != NULL;
}

//------------------------------------------------------------------------------------------------------------------------//
void Connection::reopenStreams()
{
   ScopedLock lk(m_lock);
   for(Streams::iterator it = m_streams.begin(); it != m_streams.end(); ++it)
   {
      (*it)->open(m_conn);
   }
}

//------------------------------------------------------------------------------------------------------------------------//
void Connection::closeStreams()
{
   ScopedLock lk(m_lock);
   for(Streams::iterator it = m_streams.begin(); it != m_streams.end(); ++it)
   {
      (*it)->close();
   }
}

} // namespace Plaza2

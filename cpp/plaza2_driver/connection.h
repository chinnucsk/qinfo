/// @file   connection.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/04/2012 04:29:18 PM

#ifndef PLAZA2_DRIVER_CONNECTION_H
#define PLAZA2_DRIVER_CONNECTION_H

#include "stream.h"
#include <ei_cxx/port.h>

#include <boost/noncopyable.hpp>
#include <boost/thread.hpp>

namespace Plaza2
{

using namespace P2ClientGateMTA;

class Connection : private boost::noncopyable,
                   public IDispEventImpl<0, Connection, &IID_IP2ConnectionEvent, &LIBID_P2ClientGateMTA, 1>
{
private:
   Connection();
public:
   static Connection* instance();
   void connect(
      std::string const& host,
      unsigned long port,
      std::string const& appName,
      std::string const& passwd);
   void disconnect();
   void addStream(std::string const& streamName, std::string const& iniFile, StreamType::type_t st);
   ~Connection();
   BEGIN_SINK_MAP(Connection)
      SINK_ENTRY_EX(0, IID_IP2ConnectionEvent, 1, ConnectionStatusChanged)
   END_SINK_MAP()
   operator bool () const;
private:
   void __stdcall ConnectionStatusChanged(IDispatch* conn, TConnectionStatus status);
   void run();
   void reopenStreams();
   void closeStreams();
private:
   typedef boost::mutex::scoped_lock ScopedLock;
   typedef std::list<StreamPtr>  Streams;
   ei_cxx::Port*                 m_port;
   IP2ConnectionPtr              m_conn;
   bool                          m_stop;
   std::auto_ptr<boost::thread>  m_worker;
   Streams                       m_streams;
   boost::mutex                  m_lock;
   bool                          m_connected;

};

} // namespace Plaza2

#endif // PLAZA2_DRIVER_CONNECTION_H

/// @file   stream.h
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/11/2012 09:32:18 PM


#ifndef PLAZA2_DRIVER_STREAM_H
#define PLAZA2_DRIVER_STREAM_H

#include <ei_cxx/port.h>
#include <common/smart_enum.h>
#include <boost/shared_ptr.hpp>

#include <string>

namespace Plaza2
{

using namespace P2ClientGateMTA;

class Stream :
   public IDispEventImpl<0, Stream, &IID_IP2DataStreamEvents, &LIBID_P2ClientGateMTA, 1>
{
public:
   explicit Stream(std::string const& streamName, std::string const& iniFile, StreamType::type_t streamType);
   void open(IP2ConnectionPtr conn);
   void close();
   StreamState getState() const;
   BEGIN_SINK_MAP(Stream)
      SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 1, StreamStateChanged)
      SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 2, StreamDataInserted)
      SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 3, StreamDataUpdated)
      SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 4, StreamDataDeleted)
      SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 5, StreamDatumDeleted)
      SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 6, StreamDBWillBeDeleted)
      SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 7, StreamLifeNumChanged)
      SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 8, StreamDataBegin)
      SINK_ENTRY_EX(0, IID_IP2DataStreamEvents, 9, StreamDataEnd)
   END_SINK_MAP()
private:
   void __stdcall StreamStateChanged(IDispatch* stream, TDataStreamState newState);
   void __stdcall StreamDataInserted(IDispatch* stream, BSTR tableName, IDispatch* rec);
   void __stdcall StreamDataUpdated(IDispatch* stream, BSTR tableName, __int64 Id, IDispatch* rec );
   void __stdcall StreamDataDeleted(IDispatch* stream, BSTR tableName, __int64 Id, IDispatch* rec );
   void __stdcall StreamDatumDeleted(IDispatch* stream, BSTR tableName, __int64 rev);
   void __stdcall StreamDBWillBeDeleted(IDispatch* stream);
   void __stdcall StreamLifeNumChanged(IDispatch* stream, long LifeNum);
   void __stdcall StreamDataBegin(IDispatch* stream);
   void __stdcall StreamDataEnd(IDispatch* stream);
private:
   void setRevisions(IP2TableSetPtr ts);
   void processStream(std::string const& eventName, IP2DataStreamPtr stream, IP2RecordPtr rec, _bstr_t tableName);
private:
   IP2DataStreamPtr     m_dataStream;
};

typedef boost::shared_ptr<Stream> StreamPtr;

} // namespace Plaza2

#endif // PLAZA2_DRIVER_STREAM_H

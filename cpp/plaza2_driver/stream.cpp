/// @file   stream.cpp
/// @author Dmitry S. Melnikov, dmitryme@gmail.com
/// @date   Created on: 01/11/2012 09:31:59 PM


#include "precomp.h"
#include "stream.h"

#include <ei_cxx/list.h>
#include <ei_cxx/tuple.h>
#include <ei_cxx/atom.h>

#include <iostream>

namespace Plaza2
{

//------------------------------------------------------------------------------------------------------------------------//
Stream::Stream(
   ei_cxx::Port& port, std::string const& streamName, std::string const& iniFile, StreamType::type_t streamType)
   : m_port(port)
{
   IP2TableSetPtr tableSet;
   tableSet.CreateInstance(CLSID_CP2TableSet);
   tableSet->InitFromIni(iniFile.c_str(), _bstr_t());
   setRevisions(tableSet);

   m_dataStream.CreateInstance(CLSID_CP2DataStream);
   m_dataStream->type = static_cast<TRequestType>(streamType);
   m_dataStream->StreamName = streamName.c_str();
   m_dataStream->TableSet = tableSet;
}

//------------------------------------------------------------------------------------------------------------------------//
void Stream::open(IP2ConnectionPtr conn)
{
   if (m_dataStream->State == StreamState::Error)
   {
      close();
   }
   if (m_dataStream->State == StreamState::Close /* || m_dataStream->State == StreamState::CloseComplete */)
   {
      m_dataStream->Open(conn);
      DispEventAdvise(m_dataStream);
   }
}

//------------------------------------------------------------------------------------------------------------------------//
void Stream::setRevisions(IP2TableSetPtr ts)
{
   IEnumVARIANTPtr pEnum(ts->_NewEnum());
   _variant_t elem;
   unsigned long fe = 0;
   for(int i = 0; i < ts->GetCount(); ++i)
   {
      HRESULT res = pEnum->Next(1, &elem, &fe);
      if (FAILED(res))
      {
         THROW(std::runtime_error, "Unable to enum tableset.");
      }
      _bstr_t tableName = elem;
      ts->rev[tableName] = 0;
   }
}

//------------------------------------------------------------------------------------------------------------------------//
void Stream::close()
{
   if (m_dataStream)
   {
      m_dataStream->Close();
      DispEventUnadvise(m_dataStream);
   }
}

//------------------------------------------------------------------------------------------------------------------------//
StreamState Stream::getState() const
{
   return m_dataStream->State;
}

//------------------------------------------------------------------------------------------------------------------------//
void Stream::StreamStateChanged(IDispatch *s, TDataStreamState newState)
{
   IP2DataStreamPtr stream(s);
   ERL_LOG_INFO(m_port,
      "Stream <" << (char*)stream->StreamName << "> state changed to " << StreamState::toString(newState));
}

//------------------------------------------------------------------------------------------------------------------------//
void Stream::processStream(std::string const& eventName, IP2DataStreamPtr stream, IP2RecordPtr rec, _bstr_t tableName)
{
   try
   {
      using namespace ei_cxx;

      std::ostringstream ost;
      if (tableName.length() > 0)
      {
         ost << (char*)tableName << '@';
      }
      ost << (char*)stream->StreamName;

      OTuple t(rec->Count + 2);
      t << Atom(ost.str())
        << Atom(eventName);

      for(size_t i = 0; i < rec->Count; ++i)
      {
         _variant_t val = rec->GetValAsVariantByIndex(i);
         if (val.vt == VT_UI1 || val.vt == VT_UI2 || val.vt == VT_UI4)
         {
            t << static_cast<unsigned long>(val);
         }
         else if (val.vt == VT_UI8)
         {
            unsigned __int64 const tmp = val;
            t << static_cast<boost::uint64_t>(tmp);
         }
         else if (val.vt == VT_I1 || val.vt == VT_I2 || val.vt == VT_I4)
         {
            t << static_cast<long>(val);
         }
         else if (val.vt == VT_I8)
         {
            __int64 const tmp = val;
            t << static_cast<boost::int64_t>(tmp);
         }
         else if (val.vt == VT_BSTR)
         {
            std::string const str = (_bstr_t)val;
            t << str;
         }
         else if (val.vt == VT_DECIMAL)
         {
            t << (double)val;
         }
         else if (val.vt == VT_R8)
         {
            t << static_cast<float>(val);
         }
      }
      t.send(m_port);
      if (tableName.length() == 0)
      {
         ERL_LOG_DEBUG(m_port, eventName << ": " << (char*)stream->StreamName);
      }
      else
      {
         ERL_LOG_DEBUG(m_port, eventName << ": " <<  (char*)tableName << '@' << (char*)stream->StreamName);
      }
   }
   catch(_com_error const& err)
   {
      ERL_LOG_ERROR(m_port,
         eventName << " COM error. Error=" << err.Error() << ", Message=" << err.ErrorMessage());
   }
   catch(std::exception const& err)
   {
      ERL_LOG_ERROR(m_port, eventName << ". Error=" << err.what());
   }
}

//------------------------------------------------------------------------------------------------------------------------//
void Stream::StreamDataInserted(IDispatch* s, BSTR tname, IDispatch* r)
{
   processStream("StreamDataInserted", s, r, tname);
}

//------------------------------------------------------------------------------------------------------------------------//
void Stream::StreamDataUpdated(IDispatch* s, BSTR tname, __int64, IDispatch* r)
{
   processStream("StreamDataUpdated", s, r, tname);
}

//------------------------------------------------------------------------------------------------------------------------//
void Stream::StreamDataDeleted(IDispatch* s, BSTR tname, __int64, IDispatch* r)
{
   processStream("StreamDataDeleted", s, r, tname);
}

//------------------------------------------------------------------------------------------------------------------------//
void Stream::StreamDatumDeleted(IDispatch*, BSTR, __int64)
{
}

//------------------------------------------------------------------------------------------------------------------------//
void Stream::StreamDBWillBeDeleted(IDispatch* s)
{
   using namespace ei_cxx;
   OTuple t(2);
   IP2DataStreamPtr stream(s);

   t << Atom("stream_deleted") << std::string(stream->StreamName);
   t.send(m_port);
}

//------------------------------------------------------------------------------------------------------------------------//
void Stream::StreamLifeNumChanged(IDispatch* s, long LifeNum)
{
   using namespace ei_cxx;
   m_dataStream->TableSet->LifeNum = LifeNum;

   IP2DataStreamPtr stream(s);

   OTuple t(3);
   t << Atom("life_num") << std::string(stream->StreamName) << LifeNum;
   t.send(m_port);
}

//------------------------------------------------------------------------------------------------------------------------//
void Stream::StreamDataBegin(IDispatch* s)
{
   using namespace ei_cxx;
   IP2DataStreamPtr stream(s);
   OTuple t(2);
   t << Atom("StreamDataBegin") << std::string(stream->StreamName);
   t.send(m_port);
}

//------------------------------------------------------------------------------------------------------------------------//
void Stream::StreamDataEnd(IDispatch* s)
{
   using namespace ei_cxx;
   IP2DataStreamPtr stream(s);
   OTuple t(2);
   t << Atom("StreamDataEnd") << std::string(stream->StreamName);
   t.send(m_port);
}

} // namespace Plaza2

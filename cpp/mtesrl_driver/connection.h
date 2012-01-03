/// @file   connection.h
/// @author Dmitry S. Melnikov, dmitryme@cqg.com
/// @date   Created on: 01/03/2012 05:11:01 PM

#ifndef MTESRL_CONNECTION_H
#define MTESRL_CONNECTION_H

namespace mtesrl
{

class Connection
{
public:
   Connection();
   ~Connection();
   void open();
   void close();
private:
   long m_connDescr;
};

};

#endif // MTESRL_CONNECTION_H

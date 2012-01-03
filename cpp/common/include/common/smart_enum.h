/**
 * @file smart_enum.hpp
 * @author Dmitry Melnikov (dmitryme@cqg.com)
 * @version $Id: smart_enum.h,v 1.10 2009/12/03 16:44:54 dmitryme Exp $
 */

#ifndef SMART_ENUM_H
#define SMART_ENUM_H

#include <string>
#include <boost/format.hpp>
#include <exception>
#include <boost/preprocessor/seq/seq.hpp>
#include <boost/preprocessor/seq/for_each.hpp>
#include <boost/preprocessor/seq/cat.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/stringize.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>
#include <boost/preprocessor/punctuation/comma_if.hpp>
#include <boost/preprocessor/comparison/equal.hpp>
#include <boost/preprocessor/seq/filter.hpp>

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)

#  define SMART_ENUM_MACRO1(r, data, elem) static const type_t BOOST_PP_SEQ_HEAD(elem) = BOOST_PP_SEQ_HEAD(BOOST_PP_SEQ_TAIL(elem));
#  define SMART_ENUM_MACRO4(r, data, elem)  BOOST_PP_EMPTY()
#  define CLASS_DECLARATION(name, type) \
   class name { \
   public: \
      typedef type type_t;

#  define CNAME(name) name
#  define ENUM_TYPEDEF(name, type)

#elif defined(linux) || defined(__linux) || defined(__linux__)

#  define SMART_ENUM_MACRO1(r, data, elem) static const type_t BOOST_PP_SEQ_HEAD(elem);

#  define SMART_ENUM_MACRO4(r, data, elem)  \
   template<typename T> \
   const typename data<T>::type_t data<T>::BOOST_PP_SEQ_HEAD(elem) = BOOST_PP_SEQ_HEAD(BOOST_PP_SEQ_TAIL(elem));

#  define CLASS_DECLARATION(name, type) \
   template<typename T> \
   class _##name { \
   public: \
      typedef T type_t;

#  define CNAME(name) _##name
#  define ENUM_TYPEDEF(name, type) typedef CNAME(name)<type> name;

#else
#  error "Unknown platform"
#endif

#define SMART_ENUM_MACRO2(r, data, elem) if (val == BOOST_PP_SEQ_HEAD(elem)) return BOOST_PP_SEQ_TAIL(BOOST_PP_SEQ_TAIL(elem)) ;

#define SMART_ENUM_MACRO3(r, data, elem) if (val.val_ == BOOST_PP_SEQ_HEAD(elem)) return BOOST_PP_STRINGIZE(BOOST_PP_SEQ_HEAD(elem)) ;

#define SMART_ENUM_MACRO5(r, data, elem) if (str == BOOST_PP_STRINGIZE(BOOST_PP_SEQ_HEAD(elem))) return BOOST_PP_SEQ_HEAD(elem) ;

#define SMART_ENUM_MACRO6(r, data, elem) if (str == BOOST_PP_SEQ_TAIL(BOOST_PP_SEQ_TAIL(elem))) return BOOST_PP_SEQ_HEAD(elem) ;

#define SMART_ENUM_MACRO_CHECK(r, data, elem) if (val == BOOST_PP_SEQ_HEAD(elem)) return;

#define ENTRY(name, val) ((name)(val))
#define ENTRY2(name, val, txt) ((name)(val)(txt))

class bad_enum_cast : public std::exception
{
public:
   bad_enum_cast(std::string const& str)
      :  m_What(str)
   {
   }
   virtual const char* what() const throw()
   {
      return m_What.c_str();
   }
   virtual ~bad_enum_cast() throw()
   {
   }
private:
   std::string m_What;
};

//------------------------------------------------------------------------------------------------------------------------//
//Usage:
//DECLARE_ENUM
//(
//    MyEnum, int,
//    ENTRY(First,  1)
//    ENTRY(Second, 2)
//    ENTRY(Third,  3)
//)
//
//------------------------------------------------------------------------------------------------------------------------//
#define DECLARE_ENUM(name, type, en) \
CLASS_DECLARATION(name, type) \
public: \
   CNAME(name)() \
      : val_(type_t()) \
   { \
   } \
   CNAME(name)(type_t const& val) \
      :  val_(val) \
   { \
      check(val); \
   } \
   operator type_t () const \
   { \
      return val_; \
   } \
   CNAME(name)& operator = (type_t const& val) \
   { \
      check(val); \
      val_ = val; \
      return *this; \
   } \
   BOOST_PP_SEQ_FOR_EACH(SMART_ENUM_MACRO1, type, en) \
   static std::string toString(CNAME(name) const& val) { \
      BOOST_PP_SEQ_FOR_EACH(SMART_ENUM_MACRO3, BOOST_PP_EMPTY, en) \
      throw bad_enum_cast(boost::str(boost::format("Unable to convert from %1% to string %2%") % val.val_ % #name)); \
   } \
   static CNAME(name) fromString(std::string const& str) \
   { \
      BOOST_PP_SEQ_FOR_EACH(SMART_ENUM_MACRO5, BOOST_PP_EMPTY, en) \
      throw bad_enum_cast(boost::str(boost::format("Unable to convert from \"%1%\" to %2%") % str % #name)); \
   } \
private: \
   void check(type_t const& val) \
   { \
      BOOST_PP_SEQ_FOR_EACH(SMART_ENUM_MACRO_CHECK, BOOST_PP_EMPTY, en) \
      throw bad_enum_cast(boost::str(boost::format("Unable to convert from %1% to %2%") % val % #name)); \
   } \
private: \
   type_t val_; \
}; \
ENUM_TYPEDEF(name, type) \
BOOST_PP_SEQ_FOR_EACH(SMART_ENUM_MACRO4, CNAME(name), en)

//------------------------------------------------------------------------------------------------------------------------//
//Usage:
//DECLARE_ENUM2
//(
//    MyEnum, int,
//    ENTRY2(First,  1, "It is First")
//    ENTRY2(Second, 2, "it is Second")
//    ENTRY2(Third,  3, "It is Third")
//)
//
//------------------------------------------------------------------------------------------------------------------------//
#define DECLARE_ENUM2(name, type, en) \
CLASS_DECLARATION(name, type) \
public: \
   CNAME(name)() \
      : val_(type_t()) \
   { \
   } \
   CNAME(name)(type_t const& val) \
      :  val_(val) \
   { \
      check(val); \
   } \
   operator type_t () const \
   { \
      return val_; \
   } \
   CNAME(name)& operator = (type_t const& val) \
   { \
      check(val); \
      val_ = val; \
      return *this; \
   } \
   BOOST_PP_SEQ_FOR_EACH(SMART_ENUM_MACRO1, type, en) \
   static std::string toString(CNAME(name) const& val) { \
      BOOST_PP_SEQ_FOR_EACH(SMART_ENUM_MACRO2, BOOST_PP_EMPTY, en) \
      throw bad_enum_cast(boost::str(boost::format("Unable to convert from %1% to %2%") % val.val_ % #name)); \
   } \
   static CNAME(name) fromString(std::string const& str) \
   { \
      BOOST_PP_SEQ_FOR_EACH(SMART_ENUM_MACRO6, BOOST_PP_EMPTY, en) \
      throw bad_enum_cast(boost::str(boost::format("Unable to convert from \"%1%\" to %2%") % str % #name)); \
   } \
private: \
   void check(type_t const& val) \
   { \
      BOOST_PP_SEQ_FOR_EACH(SMART_ENUM_MACRO_CHECK, BOOST_PP_EMPTY, en) \
      throw bad_enum_cast(boost::str(boost::format("Unable to convert from %1% to %2%") % val % #name)); \
   } \
private: \
   type_t val_; \
}; \
ENUM_TYPEDEF(name, type) \
BOOST_PP_SEQ_FOR_EACH(SMART_ENUM_MACRO4, CNAME(name), en)

#endif // SMART_ENUM_H

cmake_minimum_required(VERSION 2.6)

set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR})
set(LIBRARY_OUTPUT_PATH  ${CMAKE_BINARY_DIR})

set(BOOST_HOME    /tmp)
set(BOOST_INCLUDE ${BOOST_HOME})
set(BOOST_LIB     ${BOOST_HOME}stage/lib)

set(ERL_HOME      /opt/R15B01/lib/erlang/)
set(ERL_INCLUDE   ${ERL_HOME}usr/include)
set(ERL_LIB       ${ERL_HOME}usr/lib)
set(EI_INCLUDE    ${ERL_HOME}lib/erl_interface-3.7.7/include)
set(EI_LIB        ${ERL_HOME}lib/erl_interface-3.7.7/lib)

include_directories(${BOOST_INCLUDE} ${ERL_INCLUDE} ${EI_INCLUDE})
link_directories(${BOOST_LIB} ${EI_LIB} ${ERL_LIB})

if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
   add_definitions(-D__WIN32__ -DEI_CXX_ENABLE_INT64)
else (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
   add_definitions(-D__LINUX__ -DEI_CXX_ENABLE_INT64)
endif(${CMAKE_SYSTEM_NAME} MATCHES "Windows")

add_subdirectory(libs/qinfo_common)
add_subdirectory(libs/ei_cxx)
if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
   add_subdirectory(libs/mtesrl)
   add_subdirectory(micex_driver)
   add_subdirectory(plaza2_driver)
endif(${CMAKE_SYSTEM_NAME} MATCHES "Windows")

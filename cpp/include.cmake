cmake_minimum_required(VERSION 2.6)

set(BOOST_HOME    C:/dev/qinfo/cpp/3rd_party/boost_1_48_0/)
set(BOOST_LIB     ${BOOST_HOME}stage/lib)

set(ERL_HOME      C:/Program\ Files/erl5.9/)
set(ERL_INCLUDE   ${ERL_HOME}usr/include)
set(ERL_LIB       ${ERL_HOME}usr/lib)
set(EI_INCLUDE    ${ERL_HOME}lib/erl_interface-3.7.6/include)
set(EI_LIB        ${ERL_HOME}lib/erl_interface-3.7.6/lib)

include_directories(${BOOST_HOME} ${ERL_INCLUDE} ${EI_INCLUDE})

link_directories(${BOOST_LIB} ${EI_LIB} ${ERL_LIB})
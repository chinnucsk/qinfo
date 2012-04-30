cmake_minimum_required(VERSION 2.6)

set(BOOST_HOME    /home/dmitryme/tmp/boost_1_49_0/)
set(BOOST_LIB     ${BOOST_HOME}stage/lib)

set(ERL_HOME      /opt/R15B01/lib/erlang/)
set(ERL_INCLUDE   ${ERL_HOME}usr/include)
set(ERL_LIB       ${ERL_HOME}usr/lib)
set(EI_INCLUDE    ${ERL_HOME}lib/erl_interface-3.7.7/include)
set(EI_LIB        ${ERL_HOME}lib/erl_interface-3.7.7/lib)

include_directories(${BOOST_HOME} ${ERL_INCLUDE} ${EI_INCLUDE})

link_directories(/usr/local/lib64 ${BOOST_LIB} ${EI_LIB} ${ERL_LIB})

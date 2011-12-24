cmake_minimum_required(VERSION 2.6)

include(include.cmake NO_POLICY_SCOPE)

add_definitions(-D__WIN32__ -DEI_CXX_ENABLE_INT64 -DPLAZA2_PORT_DLL_EXPORTS)
include_directories(common/ei_cxx/include)

aux_source_directory(common/ei_cxx/src EI_CXX_SRC)
aux_source_directory(line_handlers/rts/plaza2/src/plaza2_port PLAZA2_PORT)

add_library(ei_cxx STATIC ${EI_CXX_SRC})
add_library(ei_cxx_test SHARED common/ei_cxx/test/ei_cxx_test.cc ${EI_CXX_SRC})
add_library(plaza2_port SHARED ${PLAZA2_PORT})

target_link_Libraries(ei_cxx_test ei)
target_link_libraries(plaza2_port ei_cxx ei)
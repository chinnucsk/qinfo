cmake_minimum_required(VERSION 2.6)

include(include.cmake NO_POLICY_SCOPE)

add_definitions(-D__WIN32__ -DEI_CXX_ENABLE_INT64 -DPLAZA2_DRIVER_DLL_EXPORTS)
include_directories(ei_cxx/include)
include_directories(common/include)

aux_source_directory(common/src COMMON_SRC)
aux_source_directory(ei_cxx/src EI_CXX_SRC)
aux_source_directory(plaza2_driver PLAZA2_DRIVER_SRC)

add_library(common STATIC ${COMMON_SRC})
add_library(ei_cxx STATIC ${EI_CXX_SRC})
add_library(ei_cxx_test SHARED ei_cxx/test/ei_cxx_test.cc ${EI_CXX_SRC})
add_library(plaza2_driver SHARED ${PLAZA2_DRIVER_SRC})

target_link_Libraries(ei_cxx_test common ei)
target_link_libraries(plaza2_driver common ei_cxx ei)
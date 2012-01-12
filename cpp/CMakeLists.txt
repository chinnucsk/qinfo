cmake_minimum_required(VERSION 2.6)

include(include.cmake NO_POLICY_SCOPE)

add_definitions(-D__WIN32__ -DEI_CXX_ENABLE_INT64)

include_directories(libs/ei_cxx/include)
include_directories(libs/common/include)
include_directories(libs/mtesrl/include)

aux_source_directory(libs/mtesrl/src MTESRL_SRC)
aux_source_directory(libs/mtesrl/example MTESRL_EXAMPLE_SRC)
aux_source_directory(libs/common/src COMMON_SRC)
aux_source_directory(libs/ei_cxx/src EI_CXX_SRC)
aux_source_directory(plaza2_driver PLAZA2_DRIVER_SRC)
aux_source_directory(micex_driver MICEX_DRIVER_SRC)

add_library(mtesrl STATIC ${MTESRL_SRC})
add_library(common STATIC ${COMMON_SRC})
add_library(ei_cxx STATIC ${EI_CXX_SRC})
add_library(ei_cxx_test SHARED libs/ei_cxx/test/ei_cxx_test.cc ${EI_CXX_SRC})
add_library(plaza2_driver SHARED ${PLAZA2_DRIVER_SRC})
add_library(micex_driver SHARED ${MICEX_DRIVER_SRC})
add_executable(mtesrl_example ${MTESRL_EXAMPLE_SRC})

set_property(TARGET plaza2_driver PROPERTY COMPILE_DEFINITIONS PLAZA2_DRIVER_DLL_EXPORTS)
set_property(TARGET micex_driver PROPERTY COMPILE_DEFINITIONS MICEX_DRIVER_DLL_EXPORTS)

target_link_Libraries(ei_cxx_test common ei)
target_link_libraries(plaza2_driver common ei_cxx ei)
target_link_libraries(micex_driver common ei_cxx ei mtesrl)
target_link_libraries(mtesrl_example common ei_cxx ei mtesrl)

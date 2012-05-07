cmake_minimum_required(VERSION 2.6)

include(include.cmake NO_POLICY_SCOPE)

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

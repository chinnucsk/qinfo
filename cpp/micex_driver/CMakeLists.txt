cmake_minimum_required(VERSION 2.6)

project(micex_driver)

include_directories(${qinfo_common_SOURCE_DIR}/include)
include_directories(${mtesrl_SOURCE_DIR}/include)
include_directories(${ei_cxx_SOURCE_DIR}/include)

aux_source_directory(. SOURCES)

add_definitions(-DMICEX_DRIVER_DLL_EXPORTS)

add_library(${PROJECT_NAME} SHARED ${SOURCES})

target_link_libraries(${PROJECT_NAME} qinfo_common ei_cxx mtesrl ei)

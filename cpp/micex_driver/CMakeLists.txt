cmake_minimum_required(VERSION 2.6)

project(micex_driver)

aux_source_directory(src SOURCES)

add_definitions(-DMICEX_DRIVER_DLL_EXPORTS)

add_library(${PROJECT_NAME} SHARED ${SOURCES})

target_link_libraries(${PROJECT_NAME} qinfo_common ei_cxx mtesrl)

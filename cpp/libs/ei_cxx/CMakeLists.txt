cmake_minimum_required(VERSION 2.6)

project(ei_cxx)

include_directories(include)
include_directories(${qinfo_common_SOURCE_DIR}/include)

add_definitions(-fPIC)

aux_source_directory(src SOURCES)

add_library(${PROJECT_NAME} STATIC ${SOURCES})

add_subdirectory(test)

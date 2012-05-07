cmake_minimum_required(VERSION 2.6)

project(qinfo_common)

include_directories(include)

aux_source_directory(src SOURCES)

add_definitions(-DQINFO_COMMON_UTILS_EXPORTS)

add_library(${PROJECT_NAME} SHARED ${SOURCES})

set_target_properties(${PROJECT_NAME} PROPERTIES PREFIX "")

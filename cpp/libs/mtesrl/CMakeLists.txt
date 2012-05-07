cmake_minimum_required(VERSION 2.6)

project(mtesrl)

include_directories(include)
include_directories(${qinfo_common_SOURCE_DIR}/include)

aux_source_directory(src SOURCES)

add_library(${PROJECT_NAME} STATIC ${SOURCES})

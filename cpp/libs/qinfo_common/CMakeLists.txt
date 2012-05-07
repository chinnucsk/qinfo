cmake_minimum_required(VERSION 2.6)

project(qinfo_common)

include_directories(include)

aux_source_directory(src SOURCES)

add_library(${PROJECT_NAME} SHARED ${SOURCES})

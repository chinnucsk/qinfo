cmake_minimum_required(VERSION 2.6)

project(mtesrl_example)

include_directories(../include)
include_directories(${qinfo_common_SOURCE_DIR}/include)

aux_source_directory(src SOURCES)

add_executable(${PROJECT_NAME} ${SOURCES})

target_link_libraries(${PROJECT_NAME} mtesrl)

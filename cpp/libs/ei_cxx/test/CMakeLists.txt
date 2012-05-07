cmake_minimum_required(VERSION 2.6)

project(ei_cxx_test)

aux_source_directory(. TEST_SOURCES)

add_library(${PROJECT_NAME} SHARED ${TEST_SOURCES})

target_link_libraries(${PROJECT_NAME} ei_cxx qinfo_common ei)

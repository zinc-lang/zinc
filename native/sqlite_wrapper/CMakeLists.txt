cmake_minimum_required (VERSION 3.10)

project ("sqlite_wrapper")

# 设置 C++20 标准
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
file(GLOB_RECURSE ALL_SOURCE_FILES "${SOURCE_DIR}/*.cpp")

set(CMAKE_BUILD_TYPE Debug)

add_library(sqlite_wrapper ${ALL_SOURCE_FILES}  "${CMAKE_CURRENT_SOURCE_DIR}/../sqlite/sqlite3.c")
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../sqlite)

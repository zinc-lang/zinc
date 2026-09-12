cmake_minimum_required (VERSION 3.10)

project ("llvm_wrapper")


# 设置 C++20 标准
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(LLVM_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../out/llvm/lib/cmake/llvm")
message(STATUS "set LLVM_DIR ${LLVM_DIR}")

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

set(ZINC_CORE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../library/zinc_core")
include_directories(${ZINC_CORE_DIR})

set(SOURCE_DIR ".")
file(GLOB_RECURSE ALL_SOURCE_FILES "${SOURCE_DIR}/*.cpp")

set(CMAKE_BUILD_TYPE Debug)

add_library(llvm_wrapper STATIC ${ALL_SOURCE_FILES})

# Find the libraries that correspond to the LLVM components that we wish to use
llvm_map_components_to_libnames(llvm_libs support core irreader irprinter target native)

foreach(lib IN LISTS llvm_libs)
  find_library(LIB_PATH_${lib}
    NAMES ${lib}
    PATHS ${LLVM_LIBRARY_DIRS}
    NO_DEFAULT_PATH
  )
  message(STATUS "${lib} => ${LIB_PATH_${lib}}")
endforeach()

# Link against LLVM libraries and sqlite
target_link_libraries(llvm_wrapper PRIVATE ${llvm_libs})

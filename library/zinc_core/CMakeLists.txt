##
## zinc_core 模块是可以改用 zinc 重新实现的, 毕竟 zinc 的裸指针可以完全模拟 C 语言。
## 但是前提是编译器支持 no_std 模式。zinc_core 不能反过来依赖 zinc_std。
##

cmake_minimum_required (VERSION 3.10)

project ("zinc_core")

# 设置 C++20 标准
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(SOURCE_DIR ".")
file(GLOB_RECURSE ALL_SOURCE_FILES "${SOURCE_DIR}/*.c")

## todo: 后续可以考虑通过编译选项 把 mimalloc 编译进来
## 在性能优先场景，用 mimalloc；在code size 优先场景，用默认的 malloc

set(CMAKE_BUILD_TYPE Debug)

add_library(zinc_core ${ALL_SOURCE_FILES})

install(TARGETS zinc_core
        RUNTIME DESTINATION libs
       )
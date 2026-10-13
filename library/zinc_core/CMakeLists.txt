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

## vendored libbacktrace: 用 _Unwind_Backtrace 采帧 + 自己解析 ELF/DWARF 得到
## file:line + 函数名, 不依赖 LLVM, 也不起 llvm-symbolizer 子进程。
## 只编 Linux/ELF + mmap 分配器需要的文件, 详见 third_party/libbacktrace/README.zinc.md。
set(LIBBACKTRACE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../third_party/libbacktrace")
include_directories(${LIBBACKTRACE_DIR})
## 没有放 print.c: 它只提供 backtrace_print, 我们用不到 (只要 pcinfo/syminfo)。
set(LIBBACKTRACE_SOURCES
    "${LIBBACKTRACE_DIR}/atomic.c"
    "${LIBBACKTRACE_DIR}/backtrace.c"
    "${LIBBACKTRACE_DIR}/dwarf.c"
    "${LIBBACKTRACE_DIR}/elf.c"
    "${LIBBACKTRACE_DIR}/fileline.c"
    "${LIBBACKTRACE_DIR}/mmap.c"
    "${LIBBACKTRACE_DIR}/mmapio.c"
    "${LIBBACKTRACE_DIR}/posix.c"
    "${LIBBACKTRACE_DIR}/simple.c"
    "${LIBBACKTRACE_DIR}/sort.c"
    "${LIBBACKTRACE_DIR}/state.c"
)

## libbacktrace 自身的调试信息对使用者没有意义, 去掉 -g 后每个程序大约省 300KB
## (实测这些对象 -g 时 461KB, -g0 时 132KB)。
set_source_files_properties(${LIBBACKTRACE_SOURCES} PROPERTIES COMPILE_OPTIONS "-g0")

## todo: 后续可以考虑通过编译选项 把 mimalloc 编译进来
## 在性能优先场景，用 mimalloc；在code size 优先场景，用默认的 malloc

set(CMAKE_BUILD_TYPE Debug)

## zinc_core_map.c 是整个运行时里最热的 TU: 实测中 workload 的 callgrind Ir 里
## map 的读写路径占 ~60%, 而 Debug(-g, 无优化) 下每次查表都要多走好几层函数调用。
## 只把这一个文件用 -O2 编译 (其余保持 -g, 便于 backtrace/调试), 实测大 workload
## 的 user 时间再降 ~33%。语义/产物都不变: 见 docs/performance.md 第 12 节。
set_source_files_properties("${CMAKE_CURRENT_SOURCE_DIR}/zinc_core_map.c"
                            PROPERTIES COMPILE_OPTIONS "-O2")

add_library(zinc_core ${ALL_SOURCE_FILES} ${LIBBACKTRACE_SOURCES})

install(TARGETS zinc_core
        RUNTIME DESTINATION libs
       )
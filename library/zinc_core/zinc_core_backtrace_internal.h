#pragma once

#ifdef __cplusplus // 如果正在被C++编译器编译
extern "C" {       // 告诉C++编译器，括号内的函数按C语言的规则编译和链接
#endif

#include "zinc_core_backtrace.h"

// 单个栈帧符号化之后的结果。只在 zinc_core 内部使用 (符号化是内部实现细节,
// 不需要暴露到公开头文件里)。
#define ZINC_CORE_SYMBOL_NAME_MAX 512

typedef struct ZincCoreSymbol {
    int valid;      // 是否成功符号化
    unsigned long line;                          // 0 表示 DWARF 里没有这一条的行号
    char file[ZINC_CORE_SYMBOL_NAME_MAX];
    char function[ZINC_CORE_SYMBOL_NAME_MAX];
} ZincCoreSymbol;

// 由 zinc_core_backtrace.c 提供。
// 在普通上下文里预热一次: 建好 libbacktrace 的 state (这样信号处理函数里不用
// pthread_once), 并让 libgcc 建好 FDE 缓存。它**不**做第一次符号化 —— 那会
// dl_iterate_phdr 并解析各模块的调试信息, 实测 88ms, 详见实现里的注释。
// 只能在 main 入口这类普通上下文里调用, 不能在信号处理函数里调用。
void zinc_core_backtrace_warm_up(void);

// 由 zinc_core_backtrace_libbacktrace.c 提供。
// 用 vendored 的 libbacktrace 把一个运行期地址符号化成 file:line + 函数名,
// 成功返回 1。这是优先级最高的符号化路径 (编译进了 zinc_core, 所有程序都可用)。
int zinc_core_libbacktrace_symbolize(unsigned long addr, ZincCoreSymbol * out);

// 提前在普通上下文 (main 入口) 里建好 libbacktrace 的 state。
// 信号处理函数里不能调用它: pthread_once 和打开/解析 ELF 都不是 async-signal-safe 的,
// 信号一旦打断正在初始化 state 的线程就会死锁。
void zinc_core_libbacktrace_init(void);

// 信号处理函数里用的符号化: 只读已经发布好的 state, 不做初始化、不加锁。
// state 还没建好时返回 0 (交给上层的兜底路径)。
int zinc_core_libbacktrace_symbolize_signal_safe(unsigned long addr, ZincCoreSymbol * out);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif
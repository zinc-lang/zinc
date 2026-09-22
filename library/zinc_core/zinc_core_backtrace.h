#pragma once

#ifdef __cplusplus // 如果正在被C++编译器编译
extern "C" {       // 告诉C++编译器，括号内的函数按C语言的规则编译和链接
#endif

#define ZINC_CORE_MAX_FRAMES 64

// 采集/打印 backtrace 的这些函数不能被内联: 打印时按固定帧数跳过
// capture / print_stacktrace / panic / print_signal_backtrace 自身的帧,
// 一旦被内联帧数就会错位。
#if defined(__GNUC__) || defined(__clang__)
#define ZINC_CORE_NOINLINE __attribute__((noinline))
#else
#define ZINC_CORE_NOINLINE
#endif

// 搜集到的调用栈。由 zinc_core_capture_backtrace 填写。
// skip 表示打印时跳过最前面的若干帧 (跳过搜集/打印函数自身)。
// 注意: zinc_std 的 BackTrace (library/zinc_std/primitives/backtrace.zn) 是按
// 这个布局在 Zinc 侧直接构造的, 改字段要同步改那边 (C 侧有 static assert 兜底)。
typedef struct ZincCoreBacktrace {
    void * frames[ZINC_CORE_MAX_FRAMES];
    int frame_count;
    int skip;
} ZincCoreBacktrace;

ZINC_CORE_NOINLINE void zinc_core_panic(const char * file, unsigned long line, const char * msg, unsigned long len);

// 搜集当前调用栈
ZINC_CORE_NOINLINE void zinc_core_capture_backtrace(ZincCoreBacktrace * bt);
// 打印已经搜集好的调用栈
void zinc_core_print_backtrace(const ZincCoreBacktrace * bt);

// 把已经采集好的调用栈渲染成文本 (和 panic 时打印的帧格式一致, 每帧 1~2 行,
// 只是不带最末尾那个空行)。返回文本的总字节数; out != NULL 且 cap > 0 时写入
// 前 min(总长, cap) 字节 (不写结尾 0)。可以先用 out=NULL, cap=0 问出长度, 再按
// 长度分配缓冲区调第二次。会做符号化 (第一次可能解析 DWARF), 不能在信号处理
// 函数里调用。zinc_std 的 BackTrace::to_string 用的就是这个。
unsigned long zinc_core_backtrace_text(const ZincCoreBacktrace * bt, char * out, unsigned long cap);
// 便捷函数: 搜集并打印当前调用栈。只能在普通上下文里调用:
// 内部会加锁, 并且第一次符号化时要做懒初始化, 这两件事都不能在信号处理函数里做。
ZINC_CORE_NOINLINE void zinc_core_print_stacktrace(void);

// 在信号处理函数里打印调用栈 (SIGSEGV / SIGABRT 等)。
// fault_pc 是 ucontext 里保存的出错指令地址 (见 zinc_core_process.c), 传 0 表示拿不到。
// 它作为第一帧精确符号化, 不依赖 unwinder 能否跨过信号帧; 后面接出错现场的调用者。
//
// 与 print_stacktrace 的区别 (信号处理函数里必须遵守的限制):
//   1. 用 pthread_mutex_trylock, 拿不到锁就直接打印, 不会死锁;
//   2. 不触发 libbacktrace state 的懒初始化 (state 已经在 zinc_core_protect_stack
//      里建好), 也不会调 dladdr; 但第一次符号化仍可能触发 libbacktrace 的
//      fileline 初始化 (dl_iterate_phdr) —— 那要拿一次动态库的锁, 见
//      zinc_core_backtrace_warm_up 的注释;
//   3. 只用 write(2) 输出, 不碰 stdio 的 FILE 锁;
//   4. 不做任何 malloc。
// 仍然会用 backtrace()/_Unwind_Backtrace (libgcc 内部有 object_mutex) 和
// libbacktrace 的 mmap 分配器, 这是能力边界 (和 Rust 的 backtrace 一样只能力所能及)。
ZINC_CORE_NOINLINE void zinc_core_print_signal_backtrace(unsigned long fault_pc);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif

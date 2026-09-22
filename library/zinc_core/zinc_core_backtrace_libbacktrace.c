#include "zinc_core_backtrace.h"
#include "zinc_core_backtrace_internal.h"

// 用 vendored 的 libbacktrace 符号化单个运行期地址。
//
// libbacktrace 自己用 _Unwind_Backtrace 采帧, 自己解析 ELF + DWARF 得到
// file:line:col 和函数名: 不依赖 LLVM, 也不起 llvm-symbolizer 子进程。
// GCC 的 sanitizer / addr2line 用的就是这套。
//
// 和 zinc_core 一起静态链接, 所以所有 Zinc 程序 (不只是编译器) 都能拿到
// file:line。代价是 libbacktrace 本身的体积 (静态库约 100KB 量级)。

#include <string.h>
#include <pthread.h>

#include "backtrace.h"

static pthread_once_t zinc_core_bt_once = PTHREAD_ONCE_INIT;
// volatile: 信号处理函数里随时可能读这个指针, 不能被编译器缓存到寄存器里。
// state 由 zinc_core_libbacktrace_init (普通上下文) 发布, 发布之前读到 NULL 也没关系。
static struct backtrace_state * volatile zinc_core_bt_state = NULL;

static void zinc_core_bt_error(void * data, const char * msg, int errnum) {
    // libbacktrace 找不到 debug info 时会调这里 (errnum == -1); 静默即可,
    // 因为外层还有别的符号化路径可以兜底。
    (void) data;
    (void) msg;
    (void) errnum;
}

static void zinc_core_bt_init_state(void) {
    // /proc/self/exe 就是当前可执行文件, DWARF 在里面。
    // 第二个参数 threaded=1: 程序可能是多线程的。
    zinc_core_bt_state = backtrace_create_state("/proc/self/exe", 1, zinc_core_bt_error, NULL);
}

// pthread_once 保证多线程同时首次 panic 时只创建一个 state
static struct backtrace_state * zinc_core_bt_get_state(void) {
    pthread_once(&zinc_core_bt_once, zinc_core_bt_init_state);
    return zinc_core_bt_state;
}

// 在 main 入口 (信号处理函数安装之前) 就建好 state, 这样信号处理函数里
// 只需要读指针, 不用走 pthread_once —— 后者和正在初始化 state 的线程撞上会死锁。
void zinc_core_libbacktrace_init(void) {
    pthread_once(&zinc_core_bt_once, zinc_core_bt_init_state);
}

struct zinc_core_bt_lookup {
    ZincCoreSymbol * out;
    int got_location;
    int got_function;
};

// 有界拷贝。这里刻意不用 snprintf: 这些回调可能在信号处理函数里被调用,
// 而 snprintf 不在 POSIX 的 async-signal-safe 列表里 (strlen/memcpy 在)。
static void zinc_core_bt_copy_str(char * dst, size_t dst_size, const char * src) {
    size_t i = 0;
    if (dst_size == 0) {
        return;
    }
    while (i + 1 < dst_size && src[i] != '\0') {
        dst[i] = src[i];
        ++i;
    }
    dst[i] = '\0';
}

// backtrace_pcinfo 的回调。命中源码位置时调用; 如果是内联调用, 可能调用多次,
// 我们取第一次 (最内层), 所以返回非 0 让 libbacktrace 停下。
// 注意 filename/function 可能是非 NULL 的空串 (有调试信息但没名字), 要当成没有。
static int zinc_core_bt_pcinfo_cb(void * data, uintptr_t pc, const char * filename, int lineno, const char * function) {
    (void) pc;
    struct zinc_core_bt_lookup * lookup = (struct zinc_core_bt_lookup *) data;

    if (filename != NULL && filename[0] != '\0') {
        zinc_core_bt_copy_str(lookup->out->file, ZINC_CORE_SYMBOL_NAME_MAX, filename);
        lookup->out->line = lineno > 0 ? (unsigned long) lineno : 0;
        lookup->got_location = 1;
    }
    if (function != NULL && function[0] != '\0') {
        zinc_core_bt_copy_str(lookup->out->function, ZINC_CORE_SYMBOL_NAME_MAX, function);
        lookup->got_function = 1;
    }
    return 1;
}

// backtrace_syminfo 的回调: pcinfo 没给出函数名时用符号表补上。
static void zinc_core_bt_syminfo_cb(void * data, uintptr_t pc, const char * symname, uintptr_t symval, uintptr_t symsize) {
    (void) pc;
    (void) symval;
    (void) symsize;
    struct zinc_core_bt_lookup * lookup = (struct zinc_core_bt_lookup *) data;
    if (symname != NULL && symname[0] != '\0') {
        zinc_core_bt_copy_str(lookup->out->function, ZINC_CORE_SYMBOL_NAME_MAX, symname);
        lookup->got_function = 1;
    }
}

// 公共实现: 用一个已经建好的 state 符号化。state 为 NULL 时直接失败。
static int zinc_core_bt_symbolize(struct backtrace_state * state, unsigned long addr, ZincCoreSymbol * out) {
    if (out == NULL) {
        return 0;
    }
    memset(out, 0, sizeof(*out));

    if (state == NULL) {
        return 0;
    }

    struct zinc_core_bt_lookup lookup;
    lookup.out = out;
    lookup.got_location = 0;
    lookup.got_function = 0;

    // 先要 file:line (需要 DWARF)
    backtrace_pcinfo(state, (uintptr_t) addr, zinc_core_bt_pcinfo_cb, zinc_core_bt_error, &lookup);

    // pcinfo 没给出函数名时 (没有 DWARF, 或者只拿到文件信息) 用符号表补上。
    // 只在缺函数名时调用, 所以不会覆盖 pcinfo 已经拿到的结果。
    if (!lookup.got_function) {
        backtrace_syminfo(state, (uintptr_t) addr, zinc_core_bt_syminfo_cb, zinc_core_bt_error, &lookup);
    }

    if (!lookup.got_location && !lookup.got_function) {
        return 0;
    }

    out->valid = 1;
    return 1;
}

int zinc_core_libbacktrace_symbolize(unsigned long addr, ZincCoreSymbol * out) {
    return zinc_core_bt_symbolize(zinc_core_bt_get_state(), addr, out);
}

int zinc_core_libbacktrace_symbolize_signal_safe(unsigned long addr, ZincCoreSymbol * out) {
    // 只读指针: state 还没发布 (信号早于 zinc_core_protect_stack) 就放弃,
    // 由调用方退回 "只打印地址" 的路径。绝不在这里初始化。
    struct backtrace_state * state = zinc_core_bt_state;
    return zinc_core_bt_symbolize(state, addr, out);
}

#define _GNU_SOURCE // Dl_info / dladdr 需要

#include "zinc_core_backtrace.h"
#include "zinc_core_backtrace_internal.h"

// 注:
// 1. 搜集栈帧用 libc 的 backtrace。
// 2. 符号化 (file:line + 函数名) 分两级:
//    a. libbacktrace: vendored 进 zinc_core, 所有程序都可用。它自己解析 ELF + DWARF,
//       不依赖 LLVM, 也不起 llvm-symbolizer 子进程 (对应标准库 BackTrace::capture 的思路)。
//    b. libbacktrace 拿不到信息时 (没有 DWARF、被 strip 过) 退化为 dladdr 得到的
//       函数名+偏移/地址。信号处理函数里不走这一级 (见下)。
// 3. 输出统一走 write(2), 不用 stdio: stderr 的 FILE 锁和 printf 的 malloc 都不是
//    async-signal-safe 的, 而打印调用栈很可能发生在信号处理函数里。
// 4. capture/print_stacktrace/panic/print_signal_backtrace 在头文件里标了 noinline:
//    打印时按固定帧数跳过它们自身的帧, 内联会让 skip 错位。
// 5. 打印分两个入口, 信号处理函数里必须用后者:
//    a. zinc_core_print_stacktrace: 普通上下文。加锁 + 允许懒初始化。
//    b. zinc_core_print_signal_backtrace: 信号上下文。trylock + 只读已建好的
//       libbacktrace state + 不调用 dladdr, 见头文件里的说明。

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <execinfo.h>
#include <dlfcn.h>
#include <pthread.h>

// 多线程同时 panic / 打印时, 保证一份 backtrace 的输出不会被另一份打断。
// 信号处理函数里用 trylock: 拿不到锁就直接打印 (输出可能交错, 但不会死锁)。
static pthread_mutex_t zinc_core_bt_output_lock = PTHREAD_MUTEX_INITIALIZER;

// zinc_std 的 BackTrace (library/zinc_std/primitives/backtrace.zn) 是按这个布局
// 在 Zinc 侧直接构造 ZincCoreBacktrace 的, 加字段/改顺序会静默读错。
_Static_assert(offsetof(ZincCoreBacktrace, frame_count) == ZINC_CORE_MAX_FRAMES * sizeof(void *),
               "ZincCoreBacktrace layout changed: update zinc_std BackTrace");
_Static_assert(offsetof(ZincCoreBacktrace, skip) == ZINC_CORE_MAX_FRAMES * sizeof(void *) + sizeof(int),
               "ZincCoreBacktrace layout changed: update zinc_std BackTrace");

// 输出一行需要的最大长度: 前缀 "├ #NN    " (9) + 最长的 file/函数名
// (ZINC_CORE_SYMBOL_NAME_MAX 含结尾 0) + ":行号" 之类。放不下就截断。
#define ZINC_CORE_BT_LINE_MAX (ZINC_CORE_SYMBOL_NAME_MAX + 256)

// ---------------------------------------------------------------------------
// async-signal-safe 的输出: 只写 stderr, 不分配内存, 不碰 stdio 的锁
// ---------------------------------------------------------------------------

struct zinc_core_bt_out {
    char buf[ZINC_CORE_BT_LINE_MAX];
    size_t len;
    // 输出目标:
    //   to_memory == 0 -> 写到 stderr (崩溃 / panic 用)
    //   to_memory == 1 -> 追加到 sink (容量 sink_cap), 用于把调用栈渲染成字符串。
    //                     超过容量的部分只计数不写入, sink_len 始终是"逻辑总长度",
    //                     这样调用者可以先传 cap=0 问出长度, 再按长度分配缓冲区。
    int to_memory;
    char * sink;
    size_t sink_cap;
    size_t sink_len;
};

static void zinc_core_bt_out_init(struct zinc_core_bt_out * out) {
    out->len = 0;
    out->to_memory = 0;
    out->sink = NULL;
    out->sink_cap = 0;
    out->sink_len = 0;
}

static void zinc_core_bt_out_init_sink(struct zinc_core_bt_out * out, char * sink, size_t cap) {
    zinc_core_bt_out_init(out);
    out->to_memory = 1;
    out->sink = sink;
    out->sink_cap = cap;
}

// 追加一段字节, 放不下就截断 (宁可少打几个字符, 也不能写越界)
static void zinc_core_bt_out_bytes(struct zinc_core_bt_out * out, const char * data, size_t len) {
    size_t room = sizeof(out->buf) - out->len;
    if (len > room) {
        len = room;
    }
    memcpy(out->buf + out->len, data, len);
    out->len += len;
}

static void zinc_core_bt_out_str(struct zinc_core_bt_out * out, const char * s) {
    if (s != NULL) {
        zinc_core_bt_out_bytes(out, s, strlen(s));
    }
}

static void zinc_core_bt_out_char(struct zinc_core_bt_out * out, char c) {
    zinc_core_bt_out_bytes(out, &c, 1);
}

static void zinc_core_bt_out_u64(struct zinc_core_bt_out * out, unsigned long value) {
    char tmp[24]; // 20 位十进制 + 余量
    size_t n = 0;
    do {
        tmp[n++] = (char) ('0' + (value % 10));
        value /= 10;
    } while (value != 0);
    while (n > 0) {
        zinc_core_bt_out_char(out, tmp[--n]);
    }
}

static void zinc_core_bt_out_hex(struct zinc_core_bt_out * out, unsigned long value) {
    static const char digits[] = "0123456789abcdef";
    char tmp[2 * sizeof(unsigned long)];
    size_t n = 0;
    do {
        tmp[n++] = digits[value & 0xf];
        value >>= 4;
    } while (value != 0);
    while (n > 0) {
        zinc_core_bt_out_char(out, tmp[--n]);
    }
}

static void zinc_core_bt_out_ptr(struct zinc_core_bt_out * out, const void * p) {
    zinc_core_bt_out_str(out, "0x");
    zinc_core_bt_out_hex(out, (unsigned long) p);
}

// 下标按 printf 的 %-2d 格式: 1 -> "1 ", 10 -> "10"
static void zinc_core_bt_out_index(struct zinc_core_bt_out * out, int index) {
    if (index < 10) {
        zinc_core_bt_out_char(out, (char) ('0' + index));
        zinc_core_bt_out_char(out, ' ');
        return;
    }
    zinc_core_bt_out_u64(out, (unsigned long) index);
}

static void zinc_core_bt_out_flush(struct zinc_core_bt_out * out) {
    if (out->to_memory) {
        size_t room = out->sink_len < out->sink_cap ? out->sink_cap - out->sink_len : 0;
        size_t n = out->len < room ? out->len : room;
        if (n > 0 && out->sink != NULL) {
            memcpy(out->sink + out->sink_len, out->buf, n);
        }
        out->sink_len += out->len;
        out->len = 0;
        return;
    }

    size_t written = 0;
    while (written < out->len) {
        ssize_t n = write(STDERR_FILENO, out->buf + written, out->len - written);
        if (n <= 0) {
            break; // 写不出去 (例如 stderr 被关了) 就放弃, 不能递归
        }
        written += (size_t) n;
    }
    out->len = 0;
}

// 收尾: 打印到 stderr 时补一个空行, 把调用栈和后面的输出隔开。
// 渲染成字符串时不要调它 (字符串就是帧文本本身)。
static void zinc_core_bt_out_finish(struct zinc_core_bt_out * out) {
    zinc_core_bt_out_char(out, '\n');
    zinc_core_bt_out_flush(out);
}

// ---------------------------------------------------------------------------
// 单帧的打印
// ---------------------------------------------------------------------------

// 帧头 "├ #%-2d    "
static void zinc_core_bt_out_frame_prefix(struct zinc_core_bt_out * out, int index) {
    zinc_core_bt_out_str(out, "├ #");
    zinc_core_bt_out_index(out, index);
    zinc_core_bt_out_str(out, "    ");
}

// 源码位置那一行的前缀 "├         " (和上面的函数名对齐)
static void zinc_core_bt_out_location_prefix(struct zinc_core_bt_out * out) {
    zinc_core_bt_out_str(out, "├         ");
}

// 打印一个已经符号化好的帧: 函数名在前, file:line 在后
static void zinc_core_bt_print_symbol_frame(struct zinc_core_bt_out * out, int index, const ZincCoreSymbol * symbol) {
    if (symbol->function[0] != '\0') {
        zinc_core_bt_out_frame_prefix(out, index);
        zinc_core_bt_out_str(out, symbol->function);
        zinc_core_bt_out_char(out, '\n');
        if (symbol->file[0] != '\0') {
            zinc_core_bt_out_location_prefix(out);
            zinc_core_bt_out_str(out, symbol->file);
            zinc_core_bt_out_char(out, ':');
            // line == 0 表示 DWARF 里没有这一行的行号 (例如内联展开之后),
            // 打 "?": 打 0 会被误读成 "第 0 行"。
            if (symbol->line == 0) {
                zinc_core_bt_out_char(out, '?');
            } else {
                zinc_core_bt_out_u64(out, symbol->line);
            }
            zinc_core_bt_out_char(out, '\n');
        }
        zinc_core_bt_out_flush(out);
        return;
    }

    if (symbol->file[0] != '\0') {
        zinc_core_bt_out_frame_prefix(out, index);
        zinc_core_bt_out_str(out, symbol->file);
        zinc_core_bt_out_char(out, ':');
        if (symbol->line == 0) {
            zinc_core_bt_out_char(out, '?');
        } else {
            zinc_core_bt_out_u64(out, symbol->line);
        }
        zinc_core_bt_out_char(out, '\n');
    }
    zinc_core_bt_out_flush(out);
}

// 兜底: dladdr 得到的 函数名+偏移 [地址]。
// lookup 用来查符号, addr 是原始地址 (只用于显示)。
// signal_safe=1 时不调用 dladdr (它会拿动态库的锁), 只打印地址。
static void zinc_core_bt_print_raw_frame(struct zinc_core_bt_out * out, int index, void * lookup, void * addr, int signal_safe) {
    Dl_info info;
    if (!signal_safe && dladdr(lookup, &info) != 0 && info.dli_sname != NULL) {
        unsigned long offset = (unsigned long) ((const char *) lookup - (const char *) info.dli_saddr);
        zinc_core_bt_out_frame_prefix(out, index);
        zinc_core_bt_out_str(out, info.dli_sname);
        zinc_core_bt_out_str(out, "+0x");
        zinc_core_bt_out_hex(out, offset);
        zinc_core_bt_out_str(out, " [");
        zinc_core_bt_out_ptr(out, addr);
        zinc_core_bt_out_str(out, "]\n");
        zinc_core_bt_out_flush(out);
        return;
    }
    if (!signal_safe && dladdr(lookup, &info) != 0 && info.dli_fname != NULL) {
        unsigned long offset = (unsigned long) ((const char *) lookup - (const char *) info.dli_fbase);
        zinc_core_bt_out_frame_prefix(out, index);
        zinc_core_bt_out_str(out, info.dli_fname);
        zinc_core_bt_out_str(out, "+0x");
        zinc_core_bt_out_hex(out, offset);
        zinc_core_bt_out_str(out, " [");
        zinc_core_bt_out_ptr(out, addr);
        zinc_core_bt_out_str(out, "]\n");
        zinc_core_bt_out_flush(out);
        return;
    }

    zinc_core_bt_out_frame_prefix(out, index);
    zinc_core_bt_out_ptr(out, addr);
    zinc_core_bt_out_char(out, '\n');
    zinc_core_bt_out_flush(out);
}

// libbacktrace 符号化 (最优先的一级)
static bool zinc_core_bt_print_frame_by_libbacktrace(struct zinc_core_bt_out * out, int index, void * lookup, int signal_safe) {
    ZincCoreSymbol symbol;
    int ok = signal_safe
        ? zinc_core_libbacktrace_symbolize_signal_safe((unsigned long) lookup, &symbol)
        : zinc_core_libbacktrace_symbolize((unsigned long) lookup, &symbol);
    if (!ok || !symbol.valid) {
        return false;
    }
    zinc_core_bt_print_symbol_frame(out, index, &symbol);
    return true;
}

// 打印一批帧, 编号从 1 开始。
// frame0_exact: frames[0] 是精确 PC (信号现场的出错指令), 查符号时不做 -1;
//              其余帧都是返回地址, 一律用 addr-1 落到 call 指令上再查。
// signal_safe:  在信号处理函数里, 只走不阻塞、不分配的符号化路径。
static void zinc_core_bt_print_frames(struct zinc_core_bt_out * out, void * const * frames, int count, int frame0_exact, int signal_safe) {
    if (count > ZINC_CORE_MAX_FRAMES) {
        count = ZINC_CORE_MAX_FRAMES;
    }
    if (count < 0) {
        count = 0;
    }

    for (int i = 0; i < count; ++i) {
        void * addr = frames[i];

        // libc backtrace() 返回的是返回地址 (call 的下一条指令)。直接拿它查符号,
        // 可能正好落在函数结尾之外 (例如 panic 这种不返回的调用之后紧跟函数结尾),
        // 于是查不到任何符号。统一用 addr-1 落到 call 指令上再查; 显示仍用原始地址。
        // frame0_exact 时 frames[0] 是精确 PC (信号现场的出错指令), 不能 -1。
        // addr == NULL 时不做指针运算 (外部填进来的 ZincCoreBacktrace 可能带空帧)。
        void * lookup = addr;
        if (addr != NULL && !(i == 0 && frame0_exact)) {
            lookup = (void *) ((const char *) addr - 1);
        }

        if (zinc_core_bt_print_frame_by_libbacktrace(out, i + 1, lookup, signal_safe)) {
            continue;
        }
        zinc_core_bt_print_raw_frame(out, i + 1, lookup, addr, signal_safe);
    }
}

// ---------------------------------------------------------------------------
// 对外的采集 / 打印入口
// ---------------------------------------------------------------------------

void zinc_core_capture_backtrace(ZincCoreBacktrace * bt) {
    if (bt == NULL) {
        return;
    }
    int captured = backtrace(bt->frames, ZINC_CORE_MAX_FRAMES);
    bt->frame_count = captured > 0 ? captured : 0;
    bt->skip = 0;
}

// 在普通上下文里预热一次 (由 zinc_core_protect_stack 调用):
// 1. 建好 libbacktrace 的 state, 这样信号处理函数里不需要 pthread_once。
// 2. libgcc 的 unwind 也是懒初始化的, 第一次 backtrace() 会 malloc + 注册 FDE。
//    这个很便宜 (实测 0.1ms), 顺手做掉。
//
// 这里刻意**不**跑第一次 backtrace_pcinfo/syminfo: 那条路会做 fileline_initialize,
// 里面 dl_iterate_phdr 并且解析各模块 (含 libc) 的调试信息 —— 实测 88ms, 放在每个
// 程序的启动路径上不可接受。代价是: 如果"第一次符号化"正好发生在信号处理函数里,
// 会拿一次动态库的锁 (glibc 里是递归锁: 同一个线程再拿没问题, 只有在别的线程正卡在
// dlopen/dlclose 时才会等一会儿)。这属于和 libgcc object_mutex 同一类的残余风险。
void zinc_core_backtrace_warm_up(void) {
    zinc_core_libbacktrace_init();

    void * frames[2];
    (void) backtrace(frames, 2);
}

// 把 skip / frame_count 夹到合法范围。
// ZincCoreBacktrace 是公开结构 (而且 Zinc 侧会直接按同样布局构造它), 防止外部
// 填了超过数组长度的 frame_count; backtrace() 也允许返回 -1, 所以下界也要夹住。
static void zinc_core_bt_clamp(const ZincCoreBacktrace * bt, int * start, int * count) {
    int s = bt->skip;
    if (s < 0) {
        s = 0;
    }
    int c = bt->frame_count;
    if (c > ZINC_CORE_MAX_FRAMES) {
        c = ZINC_CORE_MAX_FRAMES;
    }
    if (c < 0) {
        c = 0;
    }
    if (s > c) {
        s = c;
    }
    *start = s;
    *count = c;
}

void zinc_core_print_backtrace(const ZincCoreBacktrace * bt) {
    if (bt == NULL) {
        return;
    }

    int start;
    int count;
    zinc_core_bt_clamp(bt, &start, &count);

    pthread_mutex_lock(&zinc_core_bt_output_lock);
    struct zinc_core_bt_out out;
    zinc_core_bt_out_init(&out);
    zinc_core_bt_print_frames(&out, bt->frames + start, count - start, 0, 0);
    zinc_core_bt_out_finish(&out);
    pthread_mutex_unlock(&zinc_core_bt_output_lock);
}

// 把已经采集好的调用栈渲染成文本 (和 panic 时打印的帧格式完全一致, 只是不带
// 最末尾那个空行)。返回文本的总字节数; out/cap 有效时写入前 min(总长, cap) 字节。
// 调用者可以先传 out=NULL, cap=0 问出长度, 再按长度分配缓冲区调第二次。
// 注意: 会做符号化 (第一次可能解析 DWARF), 不要在信号处理函数里调用。
unsigned long zinc_core_backtrace_text(const ZincCoreBacktrace * bt, char * out, unsigned long cap) {
    if (bt == NULL) {
        return 0;
    }

    int start;
    int count;
    zinc_core_bt_clamp(bt, &start, &count);

    struct zinc_core_bt_out sink;
    zinc_core_bt_out_init_sink(&sink, out, (size_t) cap);
    zinc_core_bt_print_frames(&sink, bt->frames + start, count - start, 0, 0);
    return (unsigned long) sink.sink_len;
}

void zinc_core_print_stacktrace(void) {
    ZincCoreBacktrace bt;
    zinc_core_capture_backtrace(&bt);
    // 跳过 capture 与 print_stacktrace 自身, 从调用者开始打印
    bt.skip = 2;
    zinc_core_print_backtrace(&bt);
}

void zinc_core_print_signal_backtrace(unsigned long fault_pc) {
    void * frames[ZINC_CORE_MAX_FRAMES];
    int count = 0;

    // 出错指令放在第一帧: 即使 unwinder 跨不过信号帧, 也能看到出错的位置。
    if (fault_pc != 0) {
        frames[count++] = (void *) fault_pc;
    }
    // backtrace() 失败时会返回 -1, 夹到 0
    int captured = backtrace(frames + count, ZINC_CORE_MAX_FRAMES - count);
    if (captured < 0) {
        captured = 0;
    }
    int total = count + captured;

    // backtrace() 采到的帧形如 (从本函数开始):
    //   [0] zinc_core_print_signal_backtrace   [1] 信号处理函数
    //   [2] __restore_rt (信号跳板)             [3] 出错指令 (如果 unwinder 能跨过信号帧)
    // 这些都是信号处理路径, 不该出现在调用栈里。找到与出错指令相同的那一帧,
    // 把它和它之前的帧全部丢掉; 它之后的才是出错现场的调用者。
    int start;
    if (fault_pc != 0) {
        int dup = -1;
        for (int i = count; i < total; ++i) {
            if ((unsigned long) frames[i] == fault_pc) {
                dup = i;
                break;
            }
        }
        // 跨不过信号帧时 (dup < 0) 就只剩出错指令本身
        start = (dup >= 0) ? dup + 1 : total;
    } else {
        // 未知架构拿不到出错指令: 退化成普通采集, 跳过本函数和信号处理函数
        start = 2;
    }

    int w = count;
    for (int i = start; i < total && w < ZINC_CORE_MAX_FRAMES; ++i) {
        frames[w++] = frames[i]; // start >= count, 读在下标前面, 不会互相覆盖
    }
    total = w;

    // 信号处理里不能阻塞在锁上: 拿不到锁就直接打印 (输出可能和其他线程交错, 但不会死锁)
    int locked = (pthread_mutex_trylock(&zinc_core_bt_output_lock) == 0);
    struct zinc_core_bt_out out;
    zinc_core_bt_out_init(&out);
    zinc_core_bt_print_frames(&out, frames, total, fault_pc != 0, 1);
    zinc_core_bt_out_finish(&out);
    if (locked) {
        pthread_mutex_unlock(&zinc_core_bt_output_lock);
    }
}

void zinc_core_panic(const char * file, unsigned long line, const char * msg, unsigned long len) {
    // panic 抬头和调用栈要一起输出, 不能被别的线程插进来
    pthread_mutex_lock(&zinc_core_bt_output_lock);

    struct zinc_core_bt_out out;
    zinc_core_bt_out_init(&out);
    zinc_core_bt_out_str(&out, "⚠️ Panic: ");
    zinc_core_bt_out_flush(&out);

    // msg 不一定以 0 结尾, 长度可能超过一行缓冲, 直接 write
    if (msg != NULL) {
        size_t written = 0;
        while (written < len) {
            ssize_t n = write(STDERR_FILENO, msg + written, len - written);
            if (n <= 0) {
                break;
            }
            written += (size_t) n;
        }
    }

    zinc_core_bt_out_init(&out);
    zinc_core_bt_out_str(&out, "\n├ #0 ");
    zinc_core_bt_out_str(&out, file);
    zinc_core_bt_out_char(&out, ':');
    zinc_core_bt_out_u64(&out, line);
    zinc_core_bt_out_char(&out, '\n');
    zinc_core_bt_out_flush(&out);

    // 打印调用栈: 跳过 capture 与 panic 自身
    ZincCoreBacktrace bt;
    zinc_core_capture_backtrace(&bt);
    bt.skip = 2;

    int start;
    int count;
    zinc_core_bt_clamp(&bt, &start, &count);
    zinc_core_bt_print_frames(&out, bt.frames + start, count - start, 0, 0);
    zinc_core_bt_out_finish(&out);

    pthread_mutex_unlock(&zinc_core_bt_output_lock);

    exit(1);
}

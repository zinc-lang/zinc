// _GNU_SOURCE 必须在任何系统头文件之前定义 (pthread_getattr_np / ucontext / dlsym 等需要)
#define _GNU_SOURCE

#include "zinc_core_process.h"
#include "zinc_core_backtrace.h"
#include "zinc_core_backtrace_internal.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <pthread.h>
#include <stdint.h>
#include <ucontext.h>
#include <dlfcn.h>
#include <errno.h>
#include <sys/mman.h>

// 备用信号栈的大小。SIGSTKSZ 只有 8KB (本机), 而 handler 里要跑 backtrace() +
// libbacktrace 的 DWARF 解析, 8KB 太紧, 这里给 64KB。
// sigaltstack 是每线程的, 所以每个线程一份 (线程结束后由 trampoline 释放)。
#define ZINC_CORE_ALTSTACK_SIZE (64 * 1024)

void zinc_core_exit(int code) {
    exit(code);
}

static __thread uintptr_t stack_low;
static __thread uintptr_t stack_high;
static __thread long page_size;
// 本线程备用信号栈的内存, 线程正常结束时释放
static __thread void * altstack_mem;

int cache_thread_stack_info() {
    pthread_attr_t attr;
    void *base;
    size_t size;
    if (pthread_getattr_np(pthread_self(), &attr)) return -1;
    if (pthread_attr_getstack(&attr, &base, &size)) {
        pthread_attr_destroy(&attr);
        return -1;
    }
    pthread_attr_destroy(&attr);
    stack_low = (uintptr_t)base;
    stack_high = (uintptr_t)base + size;
    page_size = sysconf(_SC_PAGESIZE);
    return 0;
}

// 从信号现场的 ucontext 里取 "出错的那条指令" 的地址。
// 拿它当调用栈的第一帧, 就不依赖 unwinder 能不能跨过信号帧 (__restore_rt) 了。
static uintptr_t fault_instruction_pointer(const ucontext_t *uc) {
    if (uc == NULL) {
        return 0;
    }
    #ifdef __x86_64__
    return (uintptr_t)uc->uc_mcontext.gregs[REG_RIP];
    #elif defined(__aarch64__)
    return (uintptr_t)uc->uc_mcontext.pc;
    #else
    return 0;
    #endif
}

// 把信号恢复成默认处理并重新投递: 用于 "不是栈溢出" 的情况, 让内核按原样
// 终止进程 (该 core dump 就 core dump)。
// 注: raise 不在 POSIX 的 async-signal-safe 列表里, 但 glibc 的实现只是 tgkill,
// 没有锁; 用 kill() 反而可能投递到别的线程, 所以这里仍然用 raise。
static void restore_default_and_reraise(int sig) {
    struct sigaction dfl;
    memset(&dfl, 0, sizeof(dfl));
    dfl.sa_handler = SIG_DFL;
    sigemptyset(&dfl.sa_mask);
    sigaction(sig, &dfl, NULL);

    // 信号当前被屏蔽 (handler 默认 mask), raise 会把它挂起;
    // 从 handler 返回后内核按默认动作重新投递, 于是能正常 core dump。
    raise(sig);
}

static void overflow_handler(int sig, siginfo_t *info, void *ctx) {
    ucontext_t *uc = (ucontext_t *)ctx;
    uintptr_t fault = info != NULL ? (uintptr_t)info->si_addr : 0;

    uintptr_t sp = 0;
    if (uc != NULL) {
        #ifdef __x86_64__
        sp = (uintptr_t)uc->uc_mcontext.gregs[REG_RSP];
        #elif defined(__aarch64__)
        sp = (uintptr_t)uc->uc_mcontext.sp;
        #endif
    }

    // 本线程的栈边界是未知的 (例如线程不是通过 pthread_create 创建的, 或者
    // pthread_getattr_np 失败): 不做栈溢出的判断, 直接交还内核处理。
    int known_stack = (stack_low != 0 && stack_high > stack_low);
    int overflow = 0;

    if (known_stack) {
        // 1. 保护页命中检测 (最精准)
        if (fault >= stack_low - (uintptr_t)page_size && fault < stack_low) {
            overflow = 1;
        }
        // 2. 栈指针严重越界检测
        if (sp < stack_low) {
            overflow = 1;
        }
        // 3. 误报过滤：防止用户态故意访问栈底附近的无效地址被误杀
        // 检查当前 SP 是否离栈底太远，且 fault 不在栈空间内
        if (!overflow) {
            // 如果 fault 落在栈有效区间内，说明可能只是正常的缺页（如写时复制），忽略
            if (fault >= stack_low && fault < stack_high) {
                // 放过它，让内核处理正常的页错误
                restore_default_and_reraise(sig);
                return;
            }
        }
    }

    if (overflow) {
        const char msg[] = "Error: stackoverflow detected\n";
        write(STDERR_FILENO, msg, sizeof(msg) - 1);

        // 信号处理函数里只能用 async-signal-safe 的打印路径 (见头文件说明):
        // 用 ucontext 里的出错指令当第一帧, 而不是从 handler 自己开始。
        zinc_core_print_signal_backtrace(fault_instruction_pointer(uc));

        _Exit(2);
    }

    // 非栈溢出，交还内核默认处理
    restore_default_and_reraise(sig);
}

// 进程级的一次性初始化: 预热 backtrace、装信号处理函数。
static pthread_once_t zinc_core_process_once = PTHREAD_ONCE_INIT;

static void zinc_core_process_init(void) {
    // 在普通上下文里把 libbacktrace / libgcc 的懒初始化跑掉,
    // 信号处理函数里就不会再做这些 (它们会拿动态库的锁、会 malloc)。
    zinc_core_backtrace_warm_up();

    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_flags = SA_ONSTACK | SA_SIGINFO;
    sigemptyset(&sa.sa_mask);
    sa.sa_sigaction = overflow_handler;
    sigaction(SIGSEGV, &sa, NULL);
    // 部分平台/场景下栈溢出报的是 SIGBUS
    sigaction(SIGBUS, &sa, NULL);
}

// 每个线程都要调用一次: 记下本线程的栈边界, 并给本线程装一个备用信号栈。
// sigaltstack 是每线程的: 只给主线程装的话, 其它线程爆栈时 handler 会跑在
// 已经爆掉的栈上, 直接二次 fault, 什么都打不出来 (实测就是这个现象)。
// 可重复调用: 已经有备用栈就直接返回。
void zinc_core_protect_stack(void) {
    cache_thread_stack_info();
    pthread_once(&zinc_core_process_once, zinc_core_process_init);

    stack_t current;
    if (sigaltstack(NULL, &current) == 0 && (current.ss_flags & SS_DISABLE) == 0) {
        return;
    }

    void * mem = malloc(ZINC_CORE_ALTSTACK_SIZE);
    if (mem == NULL) {
        return;
    }
    stack_t ss;
    ss.ss_sp = mem;
    ss.ss_size = ZINC_CORE_ALTSTACK_SIZE;
    ss.ss_flags = 0;
    if (sigaltstack(&ss, NULL) != 0) {
        free(mem);
        return;
    }
    altstack_mem = mem;
}

static void zinc_core_unprotect_thread_stack(void) {
    if (altstack_mem == NULL) {
        return;
    }
    // 先摘掉备用栈再释放
    stack_t ss;
    ss.ss_sp = NULL;
    ss.ss_size = 0;
    ss.ss_flags = SS_DISABLE;
    sigaltstack(&ss, NULL);
    free(altstack_mem);
    altstack_mem = NULL;
}

// ---------------------------------------------------------------------------
// 让 pthread_create 创建出来的线程也自动受保护。
//
// zinc_core 是静态库, 但最终会链进可执行文件, 所以这里定义的 pthread_create
// 会覆盖动态 libc 里的那个 (可执行文件里的定义优先)。真正的实现通过
// dlsym(RTLD_NEXT) 拿 —— 只支持动态链接 libc 的场景, 本项目的链接命令都是动态的。
//
// 线程入口被包一层: 新线程里先装好备用信号栈和栈边界, 再跑用户函数。
// ---------------------------------------------------------------------------

typedef int (*zinc_core_pthread_create_fn)(pthread_t *, const pthread_attr_t *, void * (*)(void *), void *);

static zinc_core_pthread_create_fn zinc_core_real_pthread_create;
static pthread_once_t zinc_core_pthread_create_once = PTHREAD_ONCE_INIT;

static void zinc_core_resolve_pthread_create(void) {
    zinc_core_real_pthread_create = (zinc_core_pthread_create_fn)dlsym(RTLD_NEXT, "pthread_create");
}

struct zinc_core_thread_start {
    void * (*fn)(void *);
    void * arg;
};

static void * zinc_core_thread_trampoline(void * raw) {
    struct zinc_core_thread_start start = *(struct zinc_core_thread_start *)raw;
    free(raw);

    // 先给这个线程装好保护, 再跑用户的入口函数
    zinc_core_protect_stack();

    void * ret = start.fn != NULL ? start.fn(start.arg) : NULL;

    // 线程正常结束时把备用栈还回去。如果用户函数里调了 pthread_exit
    // 就不会走到这里, 这块内存就漏掉了 (可以接受)。
    zinc_core_unprotect_thread_stack();
    return ret;
}

int pthread_create(pthread_t * thread, const pthread_attr_t * attr,
                   void * (*start_routine)(void *), void * arg) {
    pthread_once(&zinc_core_pthread_create_once, zinc_core_resolve_pthread_create);
    if (zinc_core_real_pthread_create == NULL) {
        // 拿不到真正的实现 (例如静态链接 libc): 不能假装创建成功
        return EAGAIN;
    }

    struct zinc_core_thread_start * start = malloc(sizeof(*start));
    if (start == NULL) {
        return EAGAIN;
    }
    start->fn = start_routine;
    start->arg = arg;

    int rc = zinc_core_real_pthread_create(thread, attr, zinc_core_thread_trampoline, start);
    if (rc != 0) {
        free(start);
    }
    return rc;
}

// todo: 确保 std::thread::spawn 函数在创建每个线程的时候都调用了
// zinc_core_protect_stack (pthread_create 的包装已经覆盖了 pthread 这一层)
__attribute__((constructor))
void init_stack_check() {
    cache_thread_stack_info();
}

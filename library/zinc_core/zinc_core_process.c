#include "zinc_core_process.h"
#include "zinc_core_backtrace.h"

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <pthread.h>
#include <stdint.h>
#include <ucontext.h>
#include <sys/mman.h>

void zinc_core_exit(int code) {
    exit(code);
}

static __thread uintptr_t stack_low;
static __thread uintptr_t stack_high;
static __thread long page_size;

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

// todo: 确保 std::thread::spawn 函数在创建每个线程的时候都调用了这个函数
__attribute__((constructor))
void init_stack_check() {
    cache_thread_stack_info();
}

static void overflow_handler(int sig, siginfo_t *info, void *ctx) {
    ucontext_t *uc = (ucontext_t *)ctx;
    uintptr_t fault = (uintptr_t)info->si_addr;

    #ifdef __x86_64__
    uintptr_t sp = uc->uc_mcontext.gregs[REG_RSP];
    #elif defined(__aarch64__)
    uintptr_t sp = uc->uc_mcontext.sp;
    #else
    uintptr_t sp = 0;
    #endif

    int overflow = 0;

    // 1. 保护页命中检测 (最精准)
    if (fault >= stack_low - page_size && fault < stack_low) {
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
            signal(SIGSEGV, SIG_DFL);
            raise(SIGSEGV);
            return;
        }
    }

    if (overflow) {
        const char msg[] = "Error: stackoverflow detected\n";
        write(STDERR_FILENO, msg, sizeof(msg) - 1);

        zinc_core_print_stacktrace();

        _Exit(2);
    }

    // 非栈溢出，交还内核默认处理
    signal(SIGSEGV, SIG_DFL);
    raise(SIGSEGV);
}

void zinc_core_protect_stack() {
    struct sigaction sa;
    stack_t ss;

    // 分配备用信号栈 (必须！)
    ss.ss_sp = malloc(SIGSTKSZ);
    if (!ss.ss_sp) return;
    ss.ss_size = SIGSTKSZ;
    ss.ss_flags = 0;
    sigaltstack(&ss, NULL);

    sa.sa_flags = SA_ONSTACK | SA_SIGINFO;
    sigemptyset(&sa.sa_mask);
    sa.sa_sigaction = overflow_handler;
    sigaction(SIGSEGV, &sa, NULL);
}

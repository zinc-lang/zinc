#include "zinc_core_io.h"
#include "zinc_core_alloc.h"
#include <string.h>
#include <stdio.h>
#include <errno.h>

FILE * zinc_core_stdin() {
    return stdin;
}
FILE * zinc_core_stdout() {
    return stdout;
}
FILE * zinc_core_stderr() {
    return stderr;
}

// 保存 argc/argv 是各个平台上最通用的做法。
// 在 linux 上也可以通过读取 /proc/self/cmdline 来实现
static struct Slice g_main_args;

void zinc_core_set_main_args(int argc, char* argv[]) {
    // 一次性分配, 永远不释放
    struct Slice * arr = zinc_core_alloc_meta(argc * sizeof(struct Slice));
    for(int i = 0; i < argc; i++) {
        arr[i].data = argv[i];
        arr[i].len = strlen(argv[i]);
    }
    g_main_args.data = arr;
    g_main_args.len = argc;
}

// 以下函数给标准库使用
struct Slice zinc_core_get_main_args() {
    return g_main_args;
}

void zinc_core_print_str(const char * s, size_t len) {
    fwrite(s, 1, len, stdout);
    // 和其他 print 函数保持一致: 崩溃时 (panic / 栈溢出) 没有机会 flush,
    // 不能把内容留在 stdout 的缓冲区里。
    fflush(stdout);
}

void zinc_core_println_int(long long x) {
    printf("zinc_core_println_int %llx\n", x);
}

int zinc_core_float_to_string(double num, char * buf, size_t size) {
    return snprintf(buf, size, "%f", num);
}

int zinc_core_errno() {
    return errno;
}
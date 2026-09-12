#include "zinc_core_backtrace.h"

// 注:
// 1. 需要提供搜集 BackTrace 保存到数据结构中的能力。对应标准库的 `BackTrace::capture` 函数
// 2. 不应该用创建子进程的方式得到 backtrace, 因为程序部署目标机器可能没有 llvm-symbolizer 程序。应该模仿 rust 基于标准 C 实现一个跨平台的 backtrace 搜集库

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <execinfo.h>
#include <string.h>
#include <unistd.h> // 用于 readlink
#include <limits.h> // 用于 PATH_MAX
#include <sys/stat.h>

#define MAX_FRAMES 100
#define PATH_MAX  4096

static bool is_ascii_blank(int c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v';
}

static bool is_blank_string(const char *str) {
    if (str == NULL) return true;
    while (*str) {
        if (!is_ascii_blank((unsigned char)*str)) return false;
        str++;
    }
    return true;
}

void zinc_core_print_stacktrace() {

    int status = system("which llvm-symbolizer > /dev/null 2>&1");
    if (status < 0) {
        perror("system");
        return;
    }
    if (WIFEXITED(status)) {
        int exit_code = WEXITSTATUS(status);
        if (exit_code != 0) {
            fprintf(stderr, "llvm-symbolizer is not found, failed to print stacktrace.\n");
            return;
        }
    } 

    void* addr_list[MAX_FRAMES] = {NULL};
    int frame_count = backtrace(addr_list, MAX_FRAMES);
    if (frame_count == 0) {
        return;
    }

    char** symbol_list = backtrace_symbols(addr_list, frame_count);

    char exe_path[256] = {0}; // PATH_MAX 定义了系统支持的最大路径长度
    ssize_t len = readlink("/proc/self/exe", exe_path, sizeof(exe_path) - 1); // 读取符号链接内容
    if (len < 0) {
        return;
    }
    exe_path[len] = '\0'; // 手动添加字符串结束符

    for (int i = 3; i < frame_count; ++i) {
        char * line = symbol_list[i];

        char *address = strchr(line, ')');
        if (address != NULL) {
            *address = '\0';
            address = strchr(line, '(');
            if (address == NULL) {
                break;
            }
            address++;

            char command[PATH_MAX] = {0};
            snprintf(command, PATH_MAX, "llvm-symbolizer --obj=%s  %s", exe_path, address);

            FILE *fp = popen(command, "r");
            if (fp != NULL) {
                char buffer[PATH_MAX] = {0};
                fprintf(stderr, "├ #%-2d", i-2);
                bool is_first = true;
                while (fgets(buffer, PATH_MAX, fp) != NULL) {
                    if (!is_blank_string(buffer)) {
                        if (is_first) {
                            fprintf(stderr, "    %s", buffer);
                            is_first = false;
                        } else {
                            fprintf(stderr, "├    %s", buffer);
                        }
                    }
                }
                pclose(fp);
            }
        }
    }
    fprintf(stderr, "\n");

    free(symbol_list);
}

void zinc_core_panic(const char * file, unsigned long line, const char * msg, unsigned long len) {
    fprintf(stderr, "⚠️ Panic: ");
    if (msg != NULL && len > 0) {
        fwrite(msg, 1, len, stderr);
    }
    fwrite("\n", 1, 1, stderr);
    fprintf(stderr, "├ #%-2d%s:%ld\n", 0, file, line);
    // 打印调用栈
    zinc_core_print_stacktrace();
    exit(1);
}

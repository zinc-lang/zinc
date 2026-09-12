#include "zinc_core_fs.h"
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>

bool zinc_core_path_exists(const char * path, unsigned long len) {
    if (path == NULL) {
        return false;
    }

    struct stat st;
    if (path[len-1] != '\0') {
        // 拷贝路径到一个新的缓冲区，并添加 null 终止符
        char * buffer = (char *)malloc(len + 1);
        if (buffer == NULL) {
            return false; // 内存分配失败
        }
        memcpy(buffer, path, len);
        buffer[len] = '\0'; // 添加 null 终止符

        bool r = stat(buffer, &st) == 0;
        free(buffer); // 释放缓冲区
        return r;
    } else {
        bool r = stat(path, &st) == 0;
        return r;
    }

}

bool zinc_core_set_current_dir(const char * path, unsigned long len) {
    if (path == NULL) {
        return false;
    }

    if (path[len-1] != '\0') {
        // 拷贝路径到一个新的缓冲区，并添加 null 终止符
        char * buffer = (char *)malloc(len + 1);
        if (buffer == NULL) {
            return false; // 内存分配失败
        }
        memcpy(buffer, path, len);
        buffer[len] = '\0'; // 添加 null 终止符
        bool r = chdir(buffer) == 0;
        free(buffer); // 释放缓冲区
        return r;
    } else {
        bool r = chdir(path) == 0;
        return r;
    }
}

size_t zinc_core_current_exe(char * buf, size_t len) {
    ssize_t r = readlink("/proc/self/exe", buf, len);
    if (r <= 0) return 0;

    buf[r] = '\0';
    return (size_t)r;
}

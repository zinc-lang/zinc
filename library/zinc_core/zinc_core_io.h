#pragma once

#include <stdio.h>

#ifdef __cplusplus // 如果正在被C++编译器编译
extern "C" {       // 告诉C++编译器，括号内的函数按C语言的规则编译和链接
#endif

FILE * zinc_core_stdin();
FILE * zinc_core_stdout();
FILE * zinc_core_stderr();

void zinc_core_set_main_args(int argc, char* argv[]);

void zinc_core_print_str(const char * s, size_t len);
void zinc_core_println_int(long long x);
void zinc_core_check_ptr(void * p);

struct Slice {
    void * data;
    size_t len;
};

struct Slice zinc_core_get_main_args();

int zinc_core_float_to_string(double num, char * buf, size_t size);
int zinc_core_errno();

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif


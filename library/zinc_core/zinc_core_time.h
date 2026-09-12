#pragma once

#include <inttypes.h>

#ifdef __cplusplus // 如果正在被C++编译器编译
extern "C" {       // 告诉C++编译器，括号内的函数按C语言的规则编译和链接
#endif

typedef struct Instance {
    uint64_t tv_sec;
    uint64_t tv_nsec;
} Instance;

void zinc_core_instance_now(Instance * s);

int64_t zinc_core_instance_elapsed(Instance * from);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif

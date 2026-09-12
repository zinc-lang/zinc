#pragma once
#include <stdint.h>

// 后续通过条件编译支持多平台
#include <pthread.h>

#ifdef __cplusplus // 如果正在被C++编译器编译
extern "C" {       // 告诉C++编译器，括号内的函数按C语言的规则编译和链接
#endif

typedef struct {
    pthread_mutex_t inner;
} OsMutex;

int32_t sys_mutex_init(OsMutex* mtx);

int32_t sys_mutex_lock(OsMutex* mtx);

int32_t sys_mutex_unlock(OsMutex* mtx);

int32_t sys_mutex_destroy(OsMutex* mtx);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif
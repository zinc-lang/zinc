#pragma once

// 这个 hashmap 是有特殊性的，可以不使用通用实现，利用这些特点针对性优化
// 1. key/value 都是非空指针
// 2. 每个 hashmap 的容量都很小
// 3. 只需要支持插入元素，不需要支持删除元素
// 4. 查询操作远多于插入操作
// 5. 需要线程安全

#include "zinc_core_alloc.h"
#include "zinc_core_mutex.h"

#ifdef __cplusplus // 如果正在被C++编译器编译
#include <atomic>
#define _Atomic(T) std::atomic<T>
extern "C" {       // 告诉C++编译器，括号内的函数按C语言的规则编译和链接
#endif

typedef struct {
    _Atomic(zinc_ushort)    capacity;
    _Atomic(zinc_ushort)    count;
    _Atomic(void*)           buckets;
    OsMutex                 mutex;
} HashMap;

typedef HashMap* HashMapPtr;

// 不提供删除功能
void       zinc_core_map_init(HashMapPtr map);
void*      zinc_core_map_get(HashMapPtr m, void* k);

typedef void* (*NewValueFn)(void*);
// fn 和 arg 两个参数可以模拟闭包
void*      zinc_core_map_get_or_insert(HashMapPtr m, void* k, NewValueFn fn, void* arg);

void       zinc_core_map_put(HashMapPtr m, void* k, void* v);

void       zinc_core_map_iter(HashMapPtr m, void (*callback)(void* k, void* v, void* extra), void* extra);

void*      zinc_core_map_remove(HashMapPtr m, void* k); // 返回旧 value

size_t     zinc_core_map_calc_count(HashMapPtr m);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif

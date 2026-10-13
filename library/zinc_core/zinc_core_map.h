#pragma once

// 这个 hashmap 是有特殊性的，可以不使用通用实现，利用这些特点针对性优化
// 1. key/value 都是非空指针 (是8的整数倍)
// 2. 每个 hashmap 的容量都很小
// 3. 只支持插入, 不支持删除
// 4. 查询操作远多于插入操作
// 5. 需要线程安全
//
// 实现方式: 读路径完全无锁 (只有 acquire load, 没有原子读改写),
// 写路径一把 per-map mutex, 旧表延迟回收。

#include "zinc_core_alloc.h"
#include "zinc_core_mutex.h"

#ifdef __cplusplus // 如果正在被C++编译器编译
#include <atomic>
#define _Atomic(T) std::atomic<T>
extern "C" {       // 告诉C++编译器，括号内的函数按C语言的规则编译和链接
#endif

typedef struct {
    _Atomic(void*)          table;   // 当前表快照 (Table*), 只被整体替换
    _Atomic(zinc_ushort)    count;   // 元素个数 (只在写路径/持锁时修改)
    OsMutex                 mutex;   // 只保护写路径; 读路径完全无锁
} HashMap;

typedef HashMap* HashMapPtr;

void       zinc_core_map_init(HashMapPtr map);
void*      zinc_core_map_get(HashMapPtr m, void* k);

typedef void* (*NewValueFn)(void*);
// fn 和 arg 两个参数可以模拟闭包
void*      zinc_core_map_get_or_insert(HashMapPtr m, void* k, NewValueFn fn, void* arg);

// 只在 key 不存在时插入 (insert-if-absent); key 已存在时保持原有 value 不变
void       zinc_core_map_put(HashMapPtr m, void* k, void* v);

void       zinc_core_map_iter(HashMapPtr m, void (*callback)(void* k, void* v, void* extra), void* extra);

size_t     zinc_core_map_calc_count(HashMapPtr m);

// 释放所有被延迟回收的旧表
// 只能在没有其它线程可能正在访问任何 map 的时刻调用: 例如程序还在单线程阶段,
// 或者已经 join 掉所有会碰到 map 的线程。
// 运行期一次都不调用也不会出错, 只是旧表的桶数组会一直保留到进程退出;
// 每张 map 因此多占的内存 < 它最终表大小的 2 倍 (表按 2x+1 增长, 几何级数)。
void       zinc_core_map_reclaim_all(void);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif

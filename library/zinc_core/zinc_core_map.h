#pragma once

// 这个 hashmap 是有特殊性的，可以不使用通用实现，利用这些特点针对性优化
// 1. key/value 都是非空指针 (是8的整数倍)
// 2. 每个 hashmap 的容量都很小
// 3. 只支持插入, 不支持删除
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
    _Atomic(void*)          table;   // 当前表快照 (Table*), 只被整体替换
    _Atomic(zinc_ushort)    count;   // 元素个数
    _Atomic(zinc_ushort)    readers; // 正在读本 map 的线程数 (回收协议用)
    OsMutex                 mutex;
} HashMap;

typedef HashMap* HashMapPtr;

void       zinc_core_map_init(HashMapPtr map);
void*      zinc_core_map_get(HashMapPtr m, void* k);

typedef void* (*NewValueFn)(void*);
// fn 和 arg 两个参数可以模拟闭包
// 注意: fn 会在持有该 map 锁的情况下被调用 (避免同一个 key 被并发计算两次,
// do_instantiate 这类回调有副作用, 不能重复执行)。因此 fn 不能(直接或间接)
// 对同一个 map 重入 get_or_insert/put, 否则会自死锁。
void*      zinc_core_map_get_or_insert(HashMapPtr m, void* k, NewValueFn fn, void* arg);

// 只在 key 不存在时插入 (insert-if-absent); key 已存在时保持原有 value 不变
void       zinc_core_map_put(HashMapPtr m, void* k, void* v);

// 弱一致遍历: 只走调用时刻的那张表。条目不会被搬移, 所以遍历开始时就存在的条目
// 一定会被访问到 (每个恰好一次); 遍历期间就地插入到同一张表的新条目可能也会被
// 访问到。回调里修改本 map 是安全的: 遍历期间本线程算作读者, 旧表不会被释放,
// 也不会死锁。
void       zinc_core_map_iter(HashMapPtr m, void (*callback)(void* k, void* v, void* extra), void* extra);

size_t     zinc_core_map_calc_count(HashMapPtr m);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif

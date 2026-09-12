#pragma once
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus // 如果正在被C++编译器编译
extern "C" {       // 告诉C++编译器，括号内的函数按C语言的规则编译和链接
#endif

#if defined(__SIZEOF_POINTER__) && __SIZEOF_POINTER__ == 8

typedef  uint64_t  zinc_uint;
typedef  uint32_t  zinc_ushort;

#elif defined(__SIZEOF_POINTER__) && __SIZEOF_POINTER__ == 4

typedef  uint32_t  zinc_uint;
typedef  uint16_t  zinc_ushort;

#elif
    #error "Unsupported pointer size"
#endif

void zinc_core_check_ptr(void * p);
void zinc_core_check_all_ptrs();

void* zinc_core_alloc(zinc_uint size);
void  zinc_core_free(void *p);

// 这两个个函数是给 zinc_core 中的 C 代码用的，主要场景是管理 type meta
// 参数和返回都是直接指向头部不带偏移。
void* zinc_core_alloc_meta(zinc_uint size);
void zinc_core_free_meta(void * p);

// 这几个函数是给 safe 代码用的，主要场景是 ARC 内存管理。
// 参数和返回都带有引用计数偏移。
void* zinc_core_alloc_boxed(zinc_uint size);

// 这几个函数用 C 实现是因为 zinc 暂时还没支持 atomic 类型，在 std 里面无法实现原子操作
// 我们应该区分 atomic 版本和 non-atomic 版本
zinc_ushort  zinc_core_inc_strong_atomic(void *p);
zinc_ushort  zinc_core_dec_strong_atomic(void *p, void* type_meta);
zinc_ushort  zinc_core_inc_weak_atomic(void *p);
zinc_ushort  zinc_core_dec_weak_atomic(void *p, void* type_meta);

zinc_ushort  zinc_core_inc_strong(void *p);
zinc_ushort  zinc_core_dec_strong(void *p, void* type_meta);
zinc_ushort  zinc_core_inc_weak(void *p);
zinc_ushort  zinc_core_dec_weak(void *p, void* type_meta);

zinc_ushort zinc_core_strong_count(void* arc);
zinc_ushort zinc_core_weak_count(void* arc);
bool  zinc_core_is_unique(void* arc);
zinc_uint zinc_core_both_count(void* arc);

zinc_ushort zinc_core_set_strong_count(void* arc, zinc_ushort val);

// 参数是 ARC 指针
void* zinc_core_downgrade_atomic(void *arc);
// 参数是 WEAK 指针
void* zinc_core_upgrade_atomic(void* weak);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif

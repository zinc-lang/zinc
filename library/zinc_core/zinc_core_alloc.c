#include "zinc_core_alloc.h"
#include "zinc_core_typemeta.h"
#include "zinc_core_backtrace.h"

#include <stdatomic.h>
#include <threads.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

// 注: 目前没有默认链接 mimalloc, 后续需要提供比较友好的使用 mimalloc 的方式

static void ref_count_overflow_panic() {
    fprintf(stderr, "⚠️ Panic: reference count of ARC pointer overflows.\n");
    zinc_core_print_stacktrace();
    exit(1);
}

// 如果使用 mimalloc 可以把 type meta 都分配到这个单独的 heap 里面

typedef void (*dtor_fn_ty)(void*, void*); // 第一个参数是 self, 第二个参数是 type_meta

// 它的大小等于 void* 的大小
typedef struct RefCounts {
    zinc_ushort strong;
    zinc_ushort weak;
} RefCounts;

typedef struct AtomicRefCounts {
    _Atomic(zinc_ushort) strong;
    _Atomic(zinc_ushort) weak;
} AtomicRefCounts;

typedef union RefCountValue {
    _Atomic zinc_uint both;
    AtomicRefCounts seperate;
} RefCountValue;

void* zinc_core_alloc(zinc_uint size) {
    void* p = malloc(size);
    memset(p, 0, size);
    return p;
}

void zinc_core_free(void *p) {
    free(p);
}

// 不带引用计数头的分配
void* zinc_core_alloc_meta(zinc_uint size) {
    void * p = malloc(size);
    memset(p, 0, size);
    return p;
}

void zinc_core_free_meta(void * p) {
    free(p);
}

// 在 default heap 中分配, 带引用计数头
void* zinc_core_alloc_boxed(zinc_uint size) {
    size = size + sizeof(void*);
    void* p = NULL;
    p = malloc(size);
    memset(p, 0, size);

    RefCounts * head = (RefCounts*) p;
    head->strong = 1; // 强引用计数值设置成 1
    void * res = (char*)p + sizeof(void*);
    return res;
}

zinc_ushort  zinc_core_inc_strong_atomic(void *p) {
    char* head = (char*)p - sizeof(void*);
    AtomicRefCounts * c = (AtomicRefCounts*)head;

    zinc_ushort old = atomic_fetch_add(&c->strong, 1);
    if (old == UINT32_MAX || old == 0) {
        ref_count_overflow_panic();
    }
    return old;
}

zinc_ushort zinc_core_dec_strong_atomic(void *p, void* type_meta) {
    char* head = (char*)p - sizeof(void*);
    AtomicRefCounts * c = (AtomicRefCounts*)head;
    
    zinc_ushort old = atomic_fetch_sub(&c->strong, 1);
    if (old == UINT32_MAX || old == 0) {
        ref_count_overflow_panic();
    }
    if (old != 1) {
        return old;
    }
    ZnTypeMeta * ty_meta = (ZnTypeMeta*)type_meta;
    if (ty_meta->dtor_fn) {
        dtor_fn_ty dtor = (dtor_fn_ty)ty_meta->dtor_fn;
        dtor(p, type_meta);
    }
    if (c->weak == 0) {
        zinc_core_free(head);
    }
    return old;
}

zinc_ushort  zinc_core_inc_weak_atomic(void *p) {
    if (p == NULL) {
        fprintf(stderr, "[error] Attempting to call zinc_core_inc_weak_atomic for NULL pointer.\n");
        return 0;
    }
    char* head =  (char*)p - sizeof(void*);
    AtomicRefCounts * c = (AtomicRefCounts*)head;
    
    zinc_ushort old = atomic_fetch_add(&c->weak, 1);
    if (old == UINT32_MAX) {
        ref_count_overflow_panic();
    }
    return old;
}

zinc_ushort  zinc_core_dec_weak_atomic(void *p, void* _type_meta) {
    if (p == NULL) {
        fprintf(stderr, "[error] Attempting to call zinc_core_dec_weak_atomic for NULL pointer.\n");
        return 0;
    }
    char* head = (char*)p - sizeof(void*);
    AtomicRefCounts * c = (AtomicRefCounts*)head;
    
    zinc_ushort old = atomic_fetch_sub(&c->weak, 1);
    if (old != 1) {
        return old;
    }
    if(c->strong == 0) {
        zinc_core_free(head);
    }
    return old;
}

//

zinc_ushort  zinc_core_inc_strong(void *p) {
    if (p == NULL) {
        fprintf(stderr, "[error] Attempting to call zinc_core_inc_strong for NULL pointer.\n");
        return 0;
    }
    char* head = (char*)p - sizeof(void*);
    RefCounts * c = (RefCounts*)head;

    zinc_ushort old = c->strong;
    if (old == UINT32_MAX || old == 0) {
        ref_count_overflow_panic();
    }
    c->strong += 1;
    return old;
}

zinc_ushort zinc_core_dec_strong(void *p, void* type_meta) {
    if (p == NULL) {
        fprintf(stderr, "[error] Attempting to call zinc_core_dec_strong for NULL pointer.\n");
        return 0;
    }
    char* head = (char*)p - sizeof(void*);
    RefCounts * c = (RefCounts*)head;
    
    zinc_ushort old = c->strong;
    if (old == UINT32_MAX || old == 0) {
        ref_count_overflow_panic();
    }
    c->strong -= 1;
    if (old != 1) {
        return old;
    }
    ZnTypeMeta * ty_meta = (ZnTypeMeta*)type_meta;
    if (ty_meta->dtor_fn) {
        dtor_fn_ty dtor = (dtor_fn_ty)ty_meta->dtor_fn;
        dtor(p, type_meta);
    }
    if (c->weak == 0) {
        zinc_core_free(head);
    }
    return old;
}

zinc_ushort  zinc_core_inc_weak(void *p) {
    char* head =  (char*)p - sizeof(void*);
    RefCounts * c = (RefCounts*)head;
    
    zinc_ushort old = c->weak;
    if (old == UINT32_MAX) {
        ref_count_overflow_panic();
    }
    c->weak += 1;
    return old;
}

zinc_ushort  zinc_core_dec_weak(void *p, void* _type_meta) {
    char* head = (char*)p - sizeof(void*);
    RefCounts * c = (RefCounts*)head;
    
    zinc_ushort old = c->weak;
    c->weak -= 1;
    if (old != 1) {
        return old;
    }
    if(c->strong == 0) {
        zinc_core_free(head);
    }
    return old;
}


void* zinc_core_downgrade_atomic(void *p) {
    char* head = (char*)p - sizeof(void*);
    AtomicRefCounts * c = (AtomicRefCounts*)head;

    while (true) {
        zinc_ushort old = atomic_load(&c->weak);
        bool r = atomic_compare_exchange_weak(&c->weak, &old, old+1);
        if (r) {
            return p;
        }
    }
    return p;
}

void* zinc_core_upgrade_atomic(void* p) {
    char* head = (char*)p - sizeof(void*);
    AtomicRefCounts * c = (AtomicRefCounts*)head;

    while (true) {
        zinc_ushort old = atomic_load(&c->strong);
        if (old == 0) {
            return NULL;
        }
        if (old == UINT32_MAX) {
            ref_count_overflow_panic();
        }
        bool r = atomic_compare_exchange_weak(&c->strong, &old, old+1);
        if (r) {
            return p;
        }
    }

    return NULL;
}


zinc_ushort zinc_core_strong_count(void* arc) {
    AtomicRefCounts * head = ((AtomicRefCounts *) arc) - 1;
    return atomic_load(&head->strong);
}
zinc_ushort zinc_core_weak_count(void* arc) {
    AtomicRefCounts * head = ((AtomicRefCounts *) arc) - 1;
    return atomic_load(&head->weak);
}
bool  zinc_core_is_unique(void* arc) {
    zinc_uint _Atomic * head = ((zinc_uint _Atomic *) arc) - 1;
    zinc_uint both = atomic_load(head);
    RefCountValue val = { .both = both };
    return val.seperate.strong == 1 && val.seperate.weak == 0;
}
zinc_uint zinc_core_both_count(void* arc) {
    zinc_uint _Atomic * head = ((zinc_uint _Atomic *) arc) - 1;
    return atomic_load(head);
}

zinc_ushort zinc_core_set_strong_count(void* arc, zinc_ushort val) {
    AtomicRefCounts * head = ((AtomicRefCounts *) arc) - 1;
    zinc_ushort old = atomic_exchange(&head->strong, val);
    if (old == UINT32_MAX && val == UINT32_MAX) {
        ref_count_overflow_panic();
    }
    return old;
}
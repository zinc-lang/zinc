#include "zinc_core_typemeta.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdatomic.h>

// 把 p align 到 a 的整数倍
// 假设 a 是 2 的整数次幂
static void *align_ptr(void *p, size_t a) {
    if (a == 0) { a = 1; }
    void * result = (void *)(((uintptr_t)p + a - 1) & ~(a - 1));
    assert(result >= p);
    assert(((uintptr_t)result % a) == 0);
    assert(((uintptr_t)result - (uintptr_t)p) < a);
    return result;
}

// 向上取整到alignment的倍数
static size_t align_up(size_t offset, size_t alignment) {
    if (alignment == 0) { alignment = 1; }
    size_t result = (offset + alignment - 1) & ~(alignment - 1);
    assert(result >= offset);
    assert((result % alignment) == 0);
    assert((result - offset) < alignment);
    return result;
}

struct InstantiateArg {
    ZnGenericTypeMeta* generic_def;
    uint8_t ty_argc;
    ZnTypeMeta** ty_argv;
};

typedef ZnTypeMeta* (*CalculateTypeMeta)(uint8_t ty_argc, ZnTypeMeta* ty_argv[]);

typedef uintptr_t (*CalculateIntFn)(ZnTypeMeta * self_ty);

static void * create_new_hashmap(void*) {
    HashMap * map = (HashMap*) zinc_core_alloc_meta(sizeof(HashMap));
    zinc_core_map_init(map);
    return map;
}

static const char * name_of_generic_type(struct InstantiateArg* arg) {
    const char * short_name = arg->generic_def->name;
    size_t total_length = strlen(short_name) + 3; // 加两个尖括号以及 \0 结尾

    for(int i = 0; i < arg->ty_argc; i++) {
        ZnTypeMeta* argi = arg->ty_argv[i];
        total_length += strlen(argi->name);
        total_length += 1; // 逗号
    }

    // 分配足够的空间, 内部所有元素已经全部是 0
    char* result = (char*)zinc_core_alloc_meta(total_length * sizeof(char));
    if (result == NULL) { return NULL; }

    strcat(result, short_name);
    strcat(result, "<");

    _Bool is_first = 1;
    for (int i = 0; i < arg->ty_argc; i++) {
        ZnTypeMeta* argi = arg->ty_argv[i];
        if (is_first) {
            strcat(result, argi->name);
            is_first = 0;
        } else {
            strcat(result, ",");
            strcat(result, argi->name);
        }
    }
    strcat(result, ">");
    return result;
}


static void* do_instantiate(void* a) {
    static int global_id = 0;
    // 泛型实参个数必须等于泛型形参个数
    struct InstantiateArg* arg = (struct InstantiateArg*)a;
    global_id++;
    int id = global_id;

    ZnTypeMeta * ty_meta = (ZnTypeMeta *)zinc_core_alloc_meta(sizeof(ZnTypeMeta));
    ty_meta->kind = arg->generic_def->kind;
    ty_meta->ty_param_count = arg->generic_def->type_param_count;
    ty_meta->ty_argc = arg->ty_argc;
    ty_meta->ty_argv = zinc_core_alloc_meta(arg->ty_argc * sizeof(void*));
    for(unsigned i = 0; i < arg->ty_argc; i++) {
        ty_meta->ty_argv[i] = arg->ty_argv[i];
    }
    ty_meta->copy_fn = arg->generic_def->copy_fn;
    ty_meta->dtor_fn = arg->generic_def->dtor_fn;
    ty_meta->instantiated_from = arg->generic_def;
    zinc_core_map_init(&ty_meta->impls);
    for(size_t i = 0; i < arg->generic_def->related_impl_cap; i++) {
        ZnTraitImpl * impl = arg->generic_def->related_impls[i];
        if (impl != NULL) {
            // 还要考虑 where 条件
            _Bool (*where_cond_fn)(uint8_t ty_argc, ZnTypeMeta* ty_argv[]) = arg->generic_def->where_cond_fn;
            if (where_cond_fn && !where_cond_fn(arg->ty_argc, arg->ty_argv)) {
                continue;
            } else {
                // trait_fn 用来算 "这个 impl 对应的 trait" 的 TypeMeta, 它同时是
                // impls 这张表的 key。新编译器会给每个 impl 生成 trait_fn;
                // 老编译器 (以及手写的 zno) 里 trait_fn 恒为 NULL, 这时
                // trait_def 本身就是 trait 的 ZnTypeMeta, 直接拿它当 key
                // (与 component_init 里非泛型 target 那条路径完全一致)。
                CalculateTypeMeta trait_fn = (CalculateTypeMeta)impl->trait_fn;
                ZnTypeMeta* trait_ty = NULL;
                if (trait_fn != NULL) {
                    trait_ty = trait_fn(arg->ty_argc, arg->ty_argv);
                } else {
                    // 只有非泛型 trait 才能这样回退: trait_def 是 ZnTypeMeta 而不是
                    // ZnGenericTypeMeta。泛型 trait (trait TR<A>) 前端目前还不允许声明,
                    // 等支持之后必须由编译器生成 trait_fn, 不能依赖这条回退。
                    trait_ty = (ZnTypeMeta*)impl->trait_def;
                }
                if (trait_ty != NULL) {
                    zinc_core_map_put(&ty_meta->impls, trait_ty, impl);
                }
            }
        }
    }
    ty_meta->name = name_of_generic_type(arg);

    CalculateIntFn align_fn = (CalculateIntFn)arg->generic_def->align_fn;
    if (align_fn) {
        ty_meta->align = align_fn(ty_meta);
    } else {
        ty_meta->align = 1;
    }
    CalculateIntFn size_fn = (CalculateIntFn)arg->generic_def->size_fn;
    if (size_fn) {
        ty_meta->size = size_fn(ty_meta);
    } else {
        ty_meta->size = 0;
    }
    return ty_meta;
}

// 空 Tuple / 空 Array 的语义是"进程内唯一的一个 ZnTypeMeta, 不进 map"。
// 原实现用的是裸 static 指针的惰性初始化 (并发下是 data race), 这里用一把
// 只在首次初始化时触碰的原子自旋锁串行化: 与单线程版语义逐字相同
// (同一个 def 上永远只会有唯一一个空类型), 代价只在 argc==0 的极罕见路径上。
// 不需要改 ZnGenericTypeMeta 的布局 (那会牵动 native/llvm_wrapper 的字段表)。
static _Atomic int zn_singleton_lock = 0;

// argc==0 的 Tuple/Array 是"进程内唯一、不进 map"的两个单例槽。
static _Atomic(ZnTypeMeta*) zn_empty_tuple_ty = NULL;
static _Atomic(ZnTypeMeta*) zn_empty_array_ty = NULL;

static void zn_singleton_lock_acquire(void) {
    int expected = 0;
    while (!atomic_compare_exchange_weak_explicit(&zn_singleton_lock, &expected, 1,
                                                  memory_order_acquire, memory_order_relaxed)) {
        expected = 0;
    }
}

static void zn_singleton_lock_release(void) {
    atomic_store_explicit(&zn_singleton_lock, 0, memory_order_release);
}

// def->kind 必须是 TM_Tuple 或 TM_Array, 且 ty_argc == 0。
static ZnTypeMeta* zn_empty_generic_meta(ZnGenericTypeMeta* def, struct InstantiateArg* arg,
                                         _Atomic(ZnTypeMeta*)* slot) {
    (void)def;
    ZnTypeMeta* cached = atomic_load_explicit(slot, memory_order_acquire);
    if (cached != NULL) {
        return cached;
    }
    zn_singleton_lock_acquire();
    cached = atomic_load_explicit(slot, memory_order_relaxed);
    if (cached == NULL) {
        cached = do_instantiate(arg); // 锁内只做一次, 分配失败会 panic
        atomic_store_explicit(slot, cached, memory_order_release);
    }
    zn_singleton_lock_release();
    return cached;
}

ZnTypeMeta * zinc_core_instantiated(ZnGenericTypeMeta* def, uint8_t ty_argc, ZnTypeMeta* ty_argv[]) {

    // 在 on_load 阶段要保证这个 hashmap 已经初始化
    if (def->kind != TM_Tuple && def->kind != TM_Array) {
        assert(ty_argc > 0);
    }

    struct InstantiateArg arg = {
        def,
        ty_argc,
        ty_argv
    };
    HashMap * map = &def->all_instantiated;

    // TM_Tuple/TM_Array 的 argc==0 特例: 进程内唯一、不进 map。原实现用裸
    // static 指针惰性初始化 (多线程下是 data race), 这里走带自旋锁的同一份
    // 语义 (见 zn_empty_generic_meta)。
    if (def->kind == TM_Tuple || def->kind == TM_Array) {
        if (ty_argc == 0) {
            _Atomic(ZnTypeMeta*)* slot = (def->kind == TM_Tuple) ? &zn_empty_tuple_ty : &zn_empty_array_ty;
            return zn_empty_generic_meta(def, &arg, slot);
        }
        // 第一层 hashmap 的 key 是 ty_argc
        map = (HashMap*) zinc_core_map_get_or_insert(map, (void*)(size_t)ty_argc, create_new_hashmap, NULL);
    }

    assert(ty_argc > 0);

    for(int i = 0; i < ty_argc - 1; i++) {
        ZnTypeMeta* argi = ty_argv[i];
        map = (HashMap*) zinc_core_map_get_or_insert(map, argi, create_new_hashmap, NULL);
    }

    void* v = zinc_core_map_get_or_insert(map, ty_argv[ty_argc - 1], do_instantiate, &arg);
    if (v == NULL) {
        fprintf(stderr, "Panic: zinc_core_instantiated returns null.\n");
        fprintf(stderr, "  Type: %s.\n", name_of_generic_type(&arg));
        abort();
    }
    return (ZnTypeMeta*)v;
}

ZnTypeMeta * zinc_core_instantiate_array(ZnGenericTypeMeta* def, uint8_t ty_argc, ZnTypeMeta* argv) {
    if (ty_argc == 0) {
        return zinc_core_instantiated(def, 0, NULL); // 避免零长度 VLA
    }
    ZnTypeMeta * ty_argv[ty_argc];
    for(uint8_t i = 0; i < ty_argc; i++) {
        ty_argv[i] = argv;
    }
    return zinc_core_instantiated(def, ty_argc, ty_argv);
}

static void * empty_array[] = {};
static ZnTraitImpl marker_impl = {
    .type_param_count = 0,
    .assoc_type_count = 0,

    .target_def = 0,
    .trait_def = 0,
    .trait_fn = 0,

    .associated_types_fn = 0,
    .func_table = empty_array,
    .where_cond = 0,
};

ZnTraitImpl * zinc_core_find_impl(ZnTypeMeta * ty, ZnTypeMeta * trait) {
    if (ty == NULL) {
        return NULL; // 
    }
    if (trait == NULL) {
        fprintf(stderr, "Panic: zinc_core_find_impl receives null trait.\n");
        abort();
    }
    if (strcmp(trait->name, "::std::primitives::Any") == 0 ||
        strcmp(trait->name, "::std::marker::Send") == 0 ||
        strcmp(trait->name, "::std::marker::Sync") == 0 ||
        strcmp(trait->name, "::std::marker::UnSized") == 0) {
        return &marker_impl;
    }

    ZnTraitImpl * res = zinc_core_map_get(&ty->impls, trait);
    return res;
}

struct MapIterExtra {
    ZnTraitImpl * impl;
    ZnGenericTypeMeta * target_def;
    uint8_t ty_argc;
    ZnTypeMeta** ty_argv;
};

static void map_iter_callback(void* k, void* v, void* _extra) {
    struct MapIterExtra* extra = (struct MapIterExtra*) _extra;

    if (extra->ty_argc == extra->target_def->type_param_count) {
        // 还要考虑 where 条件
        _Bool (*where_cond_fn)(uint8_t ty_argc, ZnTypeMeta* ty_argv[]) = extra->target_def->where_cond_fn;
        if (where_cond_fn && !where_cond_fn(extra->ty_argc, extra->ty_argv)) {
            return;
        }
        // 与 do_instantiate 里的回退保持一致 (trait_fn == NULL 时用 trait_def 当 key)
        CalculateTypeMeta trait_fn = (CalculateTypeMeta)extra->impl->trait_fn;
        ZnTypeMeta* trait_ty = NULL;
        if (trait_fn != NULL) {
            trait_ty = trait_fn(extra->ty_argc, extra->ty_argv);
        } else {
            trait_ty = (ZnTypeMeta*)extra->impl->trait_def;
        }
        if (trait_ty == NULL) {
            return;
        }
        ZnTypeMeta* target_ty = (ZnTypeMeta*)v;
        zinc_core_map_put(&target_ty->impls, trait_ty, extra->impl);
        return;
    }

    uint8_t new_ty_argc = extra->ty_argc + 1;
    ZnTypeMeta* new_ty_argv[new_ty_argc];
    for(uint8_t i = 0; i < extra->ty_argc; i++) {
        new_ty_argv[i] = extra->ty_argv[i];
    }
    // k 是这一层的 key, 也就是第 extra->ty_argc 个泛型实参 (写 new_ty_argc 会越界一格)
    new_ty_argv[extra->ty_argc] = (ZnTypeMeta*)k;

    HashMapPtr new_map = (HashMapPtr) v;
    struct MapIterExtra new_extra = {
        .impl = extra->impl,
        .target_def = extra->target_def,
        .ty_argc = new_ty_argc,
        .ty_argv = new_ty_argv,
    };
    zinc_core_map_iter(new_map, map_iter_callback, &new_extra);
}

void add_related_impls(ZnGenericTypeMeta * target_def, ZnTraitImpl * impl) {
    size_t count = 0; // 数组中的指针的实际数量没有记录，通过遍历到 NULL 算出来
    for(size_t i = 0; i < target_def->related_impl_cap; i++) {
        if (target_def->related_impls[i] == NULL) {
            break;
        } else {
            count++;
        }
    }

    if (count == target_def->related_impl_cap) {
        // 扩容
        size_t new_cap = count == 0 ? 8 : count * 2;
        ZnTraitImpl ** new_arr = (ZnTraitImpl **) zinc_core_alloc_meta(new_cap * sizeof(ZnTraitImpl *));
        for(size_t i = 0; i < count; i++) {
            new_arr[i] = target_def->related_impls[i];
        }
        zinc_core_free_meta(target_def->related_impls);
        target_def->related_impls = new_arr;
        target_def->related_impl_cap = new_cap;
    }
    target_def->related_impls[count] = impl;
}

void zinc_core_component_init(ZnComponent * comp) {
    // 避免重复初始化
    if (comp->has_init) {
        return;
    }

    // 先递归初始化 imported_components
    for(struct ZnComponent ** comps = comp->imported_components; *comps != NULL; comps++) {
        ZnComponent * c = *comps;
        zinc_core_component_init(c);
    }

    // 把相关的 ZnTraitImpl 加到 impls 这个 hashmap 中
    // 这一步需要在运行阶段完成而不是编译阶段完成，因为可以在别的 component 中给一个类型新增 trait 实现
    // 在编译阶段搜集到的一个类型实现的所有 trait 是不完整的。
    for(ZnTraitImpl ** iter = comp->trait_impls; *iter != NULL; iter++) {
        ZnTraitImpl * impl = *iter;
        ZnGenericTypeMeta * target_def = (ZnGenericTypeMeta *)(impl->target_def);
        if (target_def->type_param_count == 0) { // 即使实际指向的是 ZnTypeMeta 也没关系, 成员偏移是一样的
            ZnTypeMeta* target_ty = (ZnTypeMeta*)target_def;

            ZnTypeMeta* trait_ty = (ZnTypeMeta*)(impl->trait_def);
            if (trait_ty == NULL) {
                fprintf(stderr, "Panic: impl's trait_def is null.\n");
                fprintf(stderr, "  Target type: %s.\n", target_ty->name);
                abort();
            }
            zinc_core_map_put(&target_ty->impls, trait_ty, impl);
        } else {
            add_related_impls(target_def, impl);
            // 遍历 all_instantiated 对每个 ZnTypeMeta 新增 impls
            HashMap * map = &target_def->all_instantiated;
            struct MapIterExtra extra = {
                impl, target_def, 0, NULL
            };
            zinc_core_map_iter(map, map_iter_callback, &extra);
        }
    }

    // 编译侧 typemeta 缓存的初始化 (编译器生成的函数, 没有就跳过)。
    // 必须在上面 impls 注册之后: do_instantiate 会把 generic_def->related_impls
    // 复制进新实例化出来的 ZnTypeMeta。
    if (comp->cache_init_fn != NULL) {
        comp->cache_init_fn();
    }

    comp->has_init = 1;
}

void zinc_core_arc_copyfn(void* ptr_head, ZnTypeMeta* ty_meta) {
    if (ptr_head == NULL) {
        return;
    }
    ZnTypeMeta * ty_arg = ty_meta->ty_argv[0];
    void * obj_head = *(void**)ptr_head;
    if (obj_head) {
        zinc_core_inc_strong_atomic(obj_head);
    }
}

void zinc_core_weak_copyfn(void* ptr_head, ZnTypeMeta* ty_meta) {
    if (ptr_head == NULL) {
        return;
    }
    ZnTypeMeta * ty_arg = ty_meta->ty_argv[0];
    void * obj_head = *(void**)ptr_head;
    if (obj_head) {
        zinc_core_inc_weak_atomic(obj_head);
    }
}

typedef void (*CopyFn)(void* obj, ZnTypeMeta* ty_meta);

void zinc_core_tuple_copyfn(void* obj, ZnTypeMeta* ty_meta) {
    uint8_t * elem = obj;
    for(uint8_t i = 0; i < ty_meta->ty_argc; i++) {
        ZnTypeMeta* elem_ty = ty_meta->ty_argv[i];
        elem = align_ptr(elem, elem_ty->align);
        if (elem_ty->copy_fn) {
            CopyFn f = (CopyFn)(elem_ty->copy_fn);
            f(elem, elem_ty);
        }
        elem += elem_ty->size;
    }
}

void zinc_core_array_copyfn(void* obj, ZnTypeMeta* ty_meta) {
    uint8_t * elem = obj;
    for(uint8_t i = 0; i < ty_meta->ty_argc; i++) {
        ZnTypeMeta* elem_ty = ty_meta->ty_argv[i];
        if (elem_ty->copy_fn) {
            CopyFn f = (CopyFn)(elem_ty->copy_fn);
            f(elem, elem_ty);
        }
        elem += elem_ty->size;
    }
}

void zinc_core_option_copyfn(void* obj, ZnTypeMeta* ty_meta) {
    if (obj == NULL) {
        return;
    }
    if (ty_meta == NULL) {
        fprintf(stderr, "Panic: zinc_core_option_copyfn receives null type meta.\n");
        abort();
    }
    ZnTypeMeta * ty_arg = ty_meta->ty_argv[0];
    if (ty_arg == NULL) {
        fprintf(stderr, "Panic: zinc_core_option_copyfn receives null type argument.\n");
        abort();
    }
    // 与 nullptr_opt_niche 保持一致
    if (ty_arg->kind == TM_Arc) {
        zinc_core_arc_copyfn(obj, ty_arg);
    }
    else if (ty_arg->kind == TM_Weak) {
        zinc_core_weak_copyfn(obj, ty_arg);
    }
    else {
        int64_t * p = (int64_t*) obj; // todo 注意考虑 size 和 align
        p = p + 1;
        CopyFn f = (CopyFn)(ty_arg->copy_fn);
        f(p, ty_arg);
    }
}

void zinc_core_option_some_fn(void* ret, size_t ret_size, void* val, ZnTypeMeta* ty_meta) {
    assert(ty_meta != NULL && ty_meta->ty_argc == 1); // option 类型有一个泛型参数
    ZnTypeMeta * ty_arg = ty_meta->ty_argv[0];
    assert(ty_arg);

    if (ty_arg->kind == TM_Arc || ty_arg->kind == TM_Weak ||
        ty_arg->kind == TM_MutBorrow || ty_arg->kind == TM_ImmutBorrow) {
        assert(ty_arg->ty_argc == 1);
        // 没有 tag, 直接拷贝
        uint8_t * out = (uint8_t *)ret;
        uint8_t * in = (uint8_t *)val;
        for(size_t i = 0; i < ret_size; i++) {
            out[i] = in[i];
        }
    } else {
        assert(ret_size >= 8); // tag 固定为 8 bytes, 后续优化
        uint64_t * tag = (uint64_t *) ret;
        * tag = 1;
        uint8_t * payload = (uint8_t *)(tag + 1);
        uint8_t * in = (uint8_t *)val;
        for(size_t i = 0; i < ret_size - 8; i++) {
            payload[i] = in[i];
        }
    }
}

void zinc_core_option_none_fn(void* ret, size_t ret_size, ZnTypeMeta* ty_meta) {
    assert(ty_meta != NULL && ty_meta->ty_argc == 1); // option 类型有一个泛型参数
    uint8_t * out = (uint8_t *)ret;
    for(size_t i = 0; i < ret_size; i++) {
        out[i] = 0;
    }
}

void zinc_core_arc_dtorfn(void* ptr_head, ZnTypeMeta* ty_meta) {
    ZnTypeMeta * ty_arg = ty_meta->ty_argv[0];
    void * obj_head = *(void**)ptr_head;
    if (ty_arg->kind == TM_Trait) {
        ZnTypeMeta * actual_ty = *(ZnTypeMeta**)((uint8_t*)ptr_head + sizeof(void*)); // trait对象头后面紧跟着实际类型指针
        zinc_core_dec_strong_atomic(obj_head, actual_ty);
    } else {
        zinc_core_dec_strong_atomic(obj_head, ty_arg);
    }
}

void zinc_core_weak_dtorfn(void* ptr_head, ZnTypeMeta* ty_meta) {
    void * obj_head = *(void**)ptr_head;
    zinc_core_dec_weak_atomic(obj_head, ty_meta);
}

typedef void (*DtorFn)(void* obj, ZnTypeMeta* ty_meta);

void zinc_core_tuple_dtorfn(void* obj, ZnTypeMeta* ty_meta) {
    uint8_t * elem = obj;
    for(uint8_t i = 0; i < ty_meta->ty_argc; i++) {
        ZnTypeMeta* elem_ty = ty_meta->ty_argv[i];
        elem = align_ptr(elem, elem_ty->align);
        if (elem_ty->dtor_fn) {
            DtorFn f = (DtorFn)(elem_ty->dtor_fn);
            f(elem, elem_ty);
        }
        elem += elem_ty->size;
    }
}

void zinc_core_array_dtorfn(void* obj, ZnTypeMeta* ty_meta) {
    uint8_t * elem = obj;
    for(uint8_t i = 0; i < ty_meta->ty_argc; i++) {
        ZnTypeMeta* elem_ty = ty_meta->ty_argv[i];
        if (elem_ty->dtor_fn) {
            DtorFn f = (DtorFn)(elem_ty->dtor_fn);
            f(elem, elem_ty);
        }
        elem += elem_ty->size;
    }
}

void zinc_core_option_dtorfn(void* option_obj, ZnTypeMeta* ty_meta) {
    // option_obj 是 Option 的对象头
    // ty_meta 是 Option 的 TypeMeta，ty_argv[0] 是 Option 的 type argument
    if (option_obj == NULL) {
        return;
    }
    if (ty_meta == NULL) {
        fprintf(stderr, "Panic: zinc_core_option_dtorfn receives null type meta.\n");
        abort();
    }
    ZnTypeMeta * ty_arg = ty_meta->ty_argv[0];
    if (ty_arg == NULL) {
        fprintf(stderr, "Panic: zinc_core_option_dtorfn receives null type argument.\n");
        abort();
    }
    // 与 nullptr_opt_niche 保持一致
    if (ty_arg->kind == TM_Arc) {
        ZnTypeMeta * pointee_ty = ty_arg->ty_argv[0]; // 指针指向的类型
        void* obj = *(void**)option_obj;
        if (obj) {
            zinc_core_arc_dtorfn(option_obj, ty_arg);
        }
    }
    else if (ty_arg->kind == TM_Weak) {
        ZnTypeMeta * pointee_ty = ty_arg->ty_argv[0]; // 指针指向的类型
        void* obj = *(void**)option_obj;
        if (obj) {
            zinc_core_weak_dtorfn(option_obj, ty_arg);
        }
    }
    else {
        int64_t * p = (int64_t*) option_obj;
        int64_t tag = *p;
        if (tag == 0) {
            return;
        } else {
            p = p + 1;
            DtorFn f = (DtorFn)(ty_arg->dtor_fn);
            f(p, ty_arg);
        }
    }
}

size_t zinc_core_agg_size(int64_t c, ZnTypeMeta* tys[]) {
    if (c == 0) {
        return 0;
    }
    uint8_t max_align = 1;
    size_t offset = 0;
    for(int64_t i = 0; i < c; i++) {
        ZnTypeMeta* ty = tys[i];
        // printf(" [%s, %ld]", ty->name, ty->size);
        if (ty->align > max_align) {
            max_align = ty->align;
        }
        offset = align_up(offset, ty->align);
        offset += ty->size;
    }
    // 结构体总大小需对齐到最大对齐值的整数倍
    size_t res = align_up(offset, max_align);
    return res;
}

size_t zinc_core_agg_align(int64_t c, ZnTypeMeta* tys[]) {
    if (c == 0) { return 1; }

    uint8_t max_align = 1;
    for(int64_t i = 0; i < c; i++) {
        ZnTypeMeta* ty = tys[i];
        if (ty->align > max_align) {
            max_align = ty->align;
        }
    }

    return max_align;
}

size_t zinc_core_tuple_sizefn(ZnTypeMeta* ty_meta) {
    return zinc_core_agg_size(ty_meta->ty_argc, ty_meta->ty_argv);
}
size_t zinc_core_array_sizefn(ZnTypeMeta* ty_meta) {
    if (ty_meta->ty_argc == 0) {
        return 0;
    }
    return (ty_meta->ty_argc) * (ty_meta->ty_argv[0]->size);
}
size_t zinc_core_tuple_alignfn(ZnTypeMeta* ty_meta) {
    size_t max = 1;
    for(unsigned int i = 0; i < ty_meta->ty_argc; i++) {
        ZnTypeMeta * t = ty_meta->ty_argv[i];
        if (t->align > max) {
            max = t->align;
        }
    }
    return max;
}
size_t zinc_core_array_alignfn(ZnTypeMeta* ty_meta) {
    if (ty_meta->ty_argc == 0) {
        return 1;
    }
    return ty_meta->ty_argv[0]->align;
}


size_t zinc_core_ptr_sizefn(ZnTypeMeta* ty_meta) {
    assert(ty_meta->ty_argc == 1);
    ZnTypeMeta* ty_arg = ty_meta->ty_argv[0];

    if (ty_arg->kind == TM_Trait || ty_arg->kind == TM_FnUpper || ty_arg->kind == TM_FnMutUpper) {
        return 8 * 3; // fat pointer
    } else {
        return 8; // thin pointer
    }
}

size_t zinc_core_option_sizefn(ZnTypeMeta* ty_meta) {
    if (ty_meta == NULL) {
        fprintf(stderr, "Panic: zinc_core_option_sizefn receives null type meta.\n");
        abort();
    }
    assert(ty_meta->ty_argc == 1);
    ZnTypeMeta* ty_arg = ty_meta->ty_argv[0];

    if (ty_arg->kind == TM_Arc || ty_arg->kind == TM_Weak ||
        ty_arg->kind == TM_MutBorrow || ty_arg->kind == TM_ImmutBorrow) {
        assert(ty_arg->ty_argc == 1);
        ZnTypeMeta* pointee = ty_arg->ty_argv[0];

        if (pointee->kind == TM_Trait || pointee->kind == TM_FnUpper || pointee->kind == TM_FnMutUpper) {
            return 8 * 3; // fat pointer
        } else {
            return 8; // thin pointer
        }
    } else {
        return 8 + ty_arg->size;
    }
}

size_t zinc_core_ptr_alignfn(ZnTypeMeta* ty_meta) {
    return 8;
}

void zinc_core_fill_in_repeat(void* elem, size_t count, void * array, ZnTypeMeta* elem_tm) {
    void * dst = array;
    size_t elem_size = elem_tm->size;
    for(size_t i = 0; i < count; i++) {
        memcpy(dst, elem, elem_size);
        dst = ((uint8_t *) dst) + elem_size;
    }
}

void zinc_core_conditional_destruct(bool* should_drop, void* fn, void* obj, ZnTypeMeta* ty_meta) {
    if (should_drop != NULL && fn != NULL && *should_drop) {
        DtorFn dtor_fn = (DtorFn)fn;
        dtor_fn(obj, ty_meta);
        *should_drop = false;
    }
}

// ---------------------------------------------------------------------------
// (E2) 显式的 meta 驱动 copy / destruct
//
// 给 std 用的两个入口: 泛型代码里的"元素搬运 / 元素析构"不再需要在源码里写出
// 具体类型 (也就不会为它物化一个运行期 ZnTypeMeta), 只要手里有元素的 TypeMeta
// 就能完成与编译器自动生成代码**语义相同**的动作:
//   * copy:     先逐字节复制 (memcpy), 再按 meta->copy_fn 修正 —— ARC 句柄
//               retain / 嵌套结构递归 copy。与 codegen 对 "T 类型的值做复制"
//               发射的动作一致 (cg_value.zn 的 do_copy_xvalue, TypeParamSymbol 分支)。
//   * destruct: 按 meta->dtor_fn 就地析构 (dtor_fn 为 NULL -> no-op), 与 codegen
//               对 "T 类型的值做 drop" 发射的动作一致 (cg_value.zn 的 do_drop)。
//
// 只新增两个符号: 不改任何现有函数的 ABI / 行为, 不改 std 公开 API, 不动 Entry 载荷。
// dst 与 src 不允许指向同一地址 (那没有"复制"语义); 零尺寸类型只调 copy_fn (若有)。
// ---------------------------------------------------------------------------

void zinc_core_meta_copy(void* dst, void* src, ZnTypeMeta* meta) {
    if (dst == NULL || src == NULL || meta == NULL || dst == src) {
        return;
    }
    size_t size = meta->size;
    if (size != 0) {
        memcpy(dst, src, size);
    }
    if (meta->copy_fn != NULL) {
        CopyFn f = (CopyFn)(meta->copy_fn);
        f(dst, meta);
    }
}

void zinc_core_meta_destruct(void* obj, ZnTypeMeta* meta) {
    if (obj == NULL || meta == NULL) {
        return;
    }
    if (meta->dtor_fn != NULL) {
        DtorFn f = (DtorFn)(meta->dtor_fn);
        f(obj, meta);
    }
}

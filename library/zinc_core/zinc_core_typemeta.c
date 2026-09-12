#include "zinc_core_typemeta.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

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
                CalculateTypeMeta trait_fn = (CalculateTypeMeta)impl->trait_fn;
                if (trait_fn) {
                    ZnTypeMeta* trait_ty = trait_fn(arg->ty_argc, arg->ty_argv);
                    zinc_core_map_put(&ty_meta->impls, trait_ty, impl);
                } else {
                    // todo 这是 bug
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

    if (def->kind == TM_Tuple) {
        if (ty_argc == 0) {
            static ZnTypeMeta * empty_tuple_ty = NULL;
            if (empty_tuple_ty == NULL) {
                empty_tuple_ty = do_instantiate(&arg);
            }
            return empty_tuple_ty;  // 无参情况单独保存, 不放到 map 里面
        } else {
            map = (HashMap*) zinc_core_map_get_or_insert(map, (void*)(size_t)ty_argc, create_new_hashmap, NULL); // 第一层 hashmap 的 key 是 ty_argc
        }
    }
    if (def->kind == TM_Array) {
        if (ty_argc == 0) {
            static ZnTypeMeta * empty_array_ty = NULL;
            if (empty_array_ty == NULL) {
                empty_array_ty = do_instantiate(&arg);
            }
            return empty_array_ty;
        } else {
            map = (HashMap*) zinc_core_map_get_or_insert(map, (void*)(size_t)ty_argc, create_new_hashmap, NULL); // 第一层 hashmap 的 key 是 ty_argc
        }
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
        CalculateTypeMeta trait_fn = (CalculateTypeMeta)extra->impl->trait_fn;
        ZnTypeMeta* trait_ty = trait_fn(extra->ty_argc, extra->ty_argv);
        ZnTypeMeta* target_ty = (ZnTypeMeta*)v;
        zinc_core_map_put(&target_ty->impls, trait_ty, extra->impl);
        return;
    }

    uint8_t new_ty_argc = extra->ty_argc + 1;
    ZnTypeMeta* new_ty_argv[new_ty_argc];
    for(uint8_t i = 0; i < extra->ty_argc; i++) {
        new_ty_argv[i] = extra->ty_argv[i];
    }
    new_ty_argv[new_ty_argc] = (ZnTypeMeta*)k;

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

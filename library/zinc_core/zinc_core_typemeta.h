#pragma once

#include <stdint.h>
#include <stdbool.h>
#include "zinc_core_map.h"

#ifdef __cplusplus // 如果正在被C++编译器编译
extern "C" {       // 告诉C++编译器，括号内的函数按C语言的规则编译和链接
#endif

struct ZnGenericTypeMeta;

enum ZnTypeMetaKind {
    TM_Err = 0,
    TM_Bool = 1,
    TM_Char = 2,
    TM_Int8 = 3, 
    TM_Int16 = 4, 
    TM_Int32 = 5, 
    TM_Int64 = 6, 
    TM_UInt8 = 7, 
    TM_UInt16 = 8, 
    TM_UInt32 = 9, 
    TM_UInt64 = 10, 
    TM_Int = 11, 
    TM_UInt = 12, 
    TM_Short = 13, 
    TM_UShort = 14,
    TM_Float32 = 15, 
    TM_Float64 = 16,
    TM_Arc = 17, 
    TM_Weak = 18, 
    TM_ImmutBorrow = 19, 
    TM_MutBorrow = 20, 
    TM_ImmutRawPointer = 21, 
    TM_MutRawPointer = 22,
    TM_Tuple = 23, 
    TM_Array = 24, 
    TM_Struct = 25, 
    TM_Enum = 26, 
    TM_Union = 27,
    TM_Trait = 28, 
    TM_FnLower = 29, 
    TM_FnUpper = 30, 
    TM_FnMutUpper = 31,
    TM_Func = 32, 
    TM_EnumVariant = 33,
    TM_Extern = 34, 
    TM_TypeParam = 35,
    TM_Lambda = 36,
};

// 考虑: 要不要保存所有成员的名字和类型。
// 如果要保存，需要模拟一个继承体系更合理，比如 ZnStructTypeMeta, ZnEnumTypeMeta
typedef struct ZnTypeMeta {
  // 开头三个成员跟 ZnGenericTypeMeta 是一样的。
  const char *     name;           // 0.
  uint8_t          kind;           // 1. ZnTypeMetaKind
  uint8_t          ty_param_count; // 2. 类型定义时候声明的泛型形参个数, 这个数大于0的话, instantiated_from 必定不是空
  uint8_t          ty_argc;        // 3. 类型实例化之后的泛型实参个数
  uint8_t          align;          // 4.
  uintptr_t        size;   // 5.

  const void *     copy_fn; // 6. void copy(void* obj, ZnTypeMeta* ty_meta) { 负责引用计数更新，不负责memcpy }
  const void *     dtor_fn; // 7. void dtor(void* obj, ZnTypeMeta* ty_meta) { 负责引用计数减少，不负责内存释放 }

  struct ZnTypeMeta ** ty_argv; // 8.

  struct ZnGenericTypeMeta * instantiated_from; // 9.

  HashMap          impls; // 实现了哪些 trait 及其 ZnTraitImpl
} ZnTypeMeta;

typedef struct ZnTraitImpl {
  uint8_t       type_param_count;
  uint8_t       assoc_type_count; // 关联类型个数

  const void *  target_def; //
  const void *  trait_def;
  const void *  trait_fn; // ZnTypeMeta* trait_fn(uint8_t ty_argc, ZnTypeMeta* ty_argv[])

  const void ** associated_types_fn; // 多个关联类型 ZnTypeMeta* assoc_fn(ZnTypeMeta* self, uint8_t ty_argc, ZnTypeMeta* ty_argv[])

  const void *  func_table; // 虚函数表
  const void *  where_cond; // bool where_cond(uint8_t ty_argc, ZnTypeMeta* ty_argv[])
} ZnTraitImpl;

typedef struct ZnGenericTypeMeta {
  const char *    name;
  uint8_t         kind;
  uint8_t         type_param_count; // 类型定义时候声明的泛型形参个数

  const void *    size_fn; // size_t size_fn(ZnTypeMeta*);
  const void *    align_fn;

  const void *    copy_fn;
  const void *    dtor_fn;
  const void *    where_cond_fn;

  size_t          related_impl_cap;
  ZnTraitImpl **  related_impls; // 指针数组

  HashMap         all_instantiated; // 泛型实参作为 key, ZnTypeMeta 作为 value
} ZnGenericTypeMeta;

ZnTypeMeta * zinc_core_instantiated(ZnGenericTypeMeta* def, uint8_t ty_argc, ZnTypeMeta* ty_argv[]);
ZnTypeMeta * zinc_core_instantiate_array(ZnGenericTypeMeta* def, uint8_t ty_argc, ZnTypeMeta*);

ZnTraitImpl * zinc_core_find_impl(ZnTypeMeta * ty, ZnTypeMeta * trait);

typedef struct ZnComponent {
  const char *          name;
  ZnTypeMeta**          non_generic_type_meta; // NULL 结尾的指针数组
  ZnGenericTypeMeta **  generic_type_meta; // NULL 结尾的指针数组
  ZnTraitImpl **        trait_impls; // NULL 结尾的指针数组
  struct ZnComponent ** imported_components; // NULL 结尾的指针数组
  uint8_t               has_init; // 确保不会重复初始化
} ZnComponent;

// 每个 component 都会生成一个 ZnComponent 类型的全局变量
// 它的mangle名就是 zn.component.<name>
// 如果组件是可执行程序，那么在main函数之前会以当前组件作为参数，调用 zinc_core_component_init
void zinc_core_component_init(ZnComponent * comp);


/// 下面是常用类型的 meta 函数
void zinc_core_arc_copyfn(void* obj, ZnTypeMeta* ty_meta);
void zinc_core_weak_copyfn(void* obj, ZnTypeMeta* ty_meta);
void zinc_core_tuple_copyfn(void* obj, ZnTypeMeta* ty_meta);
void zinc_core_array_copyfn(void* obj, ZnTypeMeta* ty_meta);
void zinc_core_option_copyfn(void* obj, ZnTypeMeta* ty_meta);

void zinc_core_option_some_fn(void* ret, size_t ret_size, void* val, ZnTypeMeta* ty_meta);
void zinc_core_option_none_fn(void* ret, size_t ret_size, ZnTypeMeta* ty_meta);

void zinc_core_arc_dtorfn(void* obj, ZnTypeMeta* ty_meta);
void zinc_core_weak_dtorfn(void* obj, ZnTypeMeta* ty_meta);
void zinc_core_tuple_dtorfn(void* obj, ZnTypeMeta* ty_meta);
void zinc_core_array_dtorfn(void* obj, ZnTypeMeta* ty_meta);
void zinc_core_option_dtorfn(void* obj, ZnTypeMeta* ty_meta);

size_t zinc_core_agg_size(int64_t c, ZnTypeMeta* tys[]);
size_t zinc_core_agg_align(int64_t c, ZnTypeMeta* tys[]);
size_t zinc_core_tuple_sizefn(ZnTypeMeta* ty_meta);
size_t zinc_core_array_sizefn(ZnTypeMeta* ty_meta);
size_t zinc_core_tuple_alignfn(ZnTypeMeta* ty_meta);
size_t zinc_core_array_alignfn(ZnTypeMeta* ty_meta);

size_t zinc_core_ptr_sizefn(ZnTypeMeta* ty_meta);
size_t zinc_core_ptr_alignfn(ZnTypeMeta* ty_meta);

size_t zinc_core_option_sizefn(ZnTypeMeta* ty_meta);

void zinc_core_fill_in_repeat(void* elem, size_t s, void * array, ZnTypeMeta* elem_tm);

void zinc_core_conditional_destruct(bool* should_drop, void* fn, void* obj, ZnTypeMeta* ty_meta);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif

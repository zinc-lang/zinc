#pragma once

#include "llvm/IR/Value.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include <cstdint>
#include <string>

#ifdef __cplusplus
extern "C" {
#endif

struct LLVMDataStructures {
    llvm::LLVMContext* ctx;
    llvm::Module* module;
    llvm::IRBuilder<> * builder;
    llvm::DIBuilder * di_builder;
    llvm::DICompileUnit * di_compile_unit = nullptr;

    std::string entry_source_file;
    std::string out_dir;

    LLVMDataStructures(std::string name);
    ~LLVMDataStructures();

    // type meta 相关结构体
    llvm::StructType * zn_type_meta = nullptr;
    llvm::StructType * zn_generic_type_meta = nullptr;
    llvm::StructType * zn_trait_impl = nullptr;
    llvm::StructType * zn_component = nullptr;
    llvm::StructType * zn_fat_ptr = nullptr;

    void init_typemeta_structs();
};

LLVMDataStructures * llds_create(const char * name, unsigned long len);
void llds_setup_compile_unit(LLVMDataStructures *llds, const char * entry, size_t len);
void llds_destroy(LLVMDataStructures *);
void llds_dump_module_llir(LLVMDataStructures *);

llvm::Type * llds_get_ZnTypeMeta(LLVMDataStructures *ds);
llvm::Type * llds_get_ZnGenericTypeMeta(LLVMDataStructures *ds);
llvm::Type * llds_get_ZnTraitImpl(LLVMDataStructures *ds);
llvm::Type * llds_get_ZnComponent(LLVMDataStructures *ds);
llvm::Type * llds_get_fat_ptr_type(LLVMDataStructures *ds);

llvm::Type * llds_get_pointer_type(LLVMDataStructures *ds);
llvm::Type * llds_get_void_type(LLVMDataStructures *ds);
llvm::Type * llds_get_int1_type(LLVMDataStructures *ds);
llvm::Type * llds_get_int8_type(LLVMDataStructures *ds);
llvm::Type * llds_get_int16_type(LLVMDataStructures *ds);
llvm::Type * llds_get_int32_type(LLVMDataStructures *ds);
llvm::Type * llds_get_int64_type(LLVMDataStructures *ds);
llvm::Type * llds_get_f32_type(LLVMDataStructures *ds);
llvm::Type * llds_get_f64_type(LLVMDataStructures *ds);
llvm::FunctionType * llds_create_function_type(LLVMDataStructures *ds, llvm::Type * ret, llvm::Type ** tys, size_t len);

llvm::Type * llds_type_of_value(llvm::Value*);

llvm::Type * llds_get_struct_type(LLVMDataStructures *ds, llvm::Type ** tys, size_t len);
bool llds_is_struct_type(llvm::Type * ty);
bool llds_is_pointer_type(llvm::Type * ty);
llvm::Type * llds_get_array_type_of(LLVMDataStructures *ds, llvm::Type* ty, size_t size);
llvm::Type * llds_get_byte_array_type(LLVMDataStructures *ds, size_t size);
llvm::Type * llds_get_ptr_array_type(LLVMDataStructures *ds, size_t count);

llvm::Constant * llds_get_const_null(LLVMDataStructures *ds);
llvm::Constant * llds_get_null_value_of(LLVMDataStructures *ds, llvm::Type* t);
llvm::Constant * llds_get_const_int1(LLVMDataStructures *ds, bool v);
llvm::Constant * llds_get_const_int8(LLVMDataStructures *ds, int8_t v);
llvm::Constant * llds_get_const_int16(LLVMDataStructures *ds, int16_t v);
llvm::Constant * llds_get_const_int32(LLVMDataStructures *ds, int32_t v);
llvm::Constant * llds_get_const_int64(LLVMDataStructures *ds, int64_t v);
llvm::Constant * llds_get_const_f32(LLVMDataStructures *ds, float v);
llvm::Constant * llds_get_const_f64(LLVMDataStructures *ds, double v);
llvm::Constant * llds_get_const_struct(LLVMDataStructures *ds, llvm::Constant ** fields, size_t len, llvm::StructType* ty);
llvm::Constant * llds_get_const_array(LLVMDataStructures *ds, llvm::Constant ** elems, size_t len, llvm::StructType* ty);
llvm::Constant * llds_get_const_array_gv(LLVMDataStructures *ds, llvm::Constant ** elems, size_t len, llvm::StructType* ty);
llvm::Constant * llds_get_const_string(LLVMDataStructures *ds, char * s, size_t len);

llvm::Function * llds_get_ext_fn_in_core(LLVMDataStructures *ds, const char * name, size_t len);

size_t llds_storage_size_of(LLVMDataStructures *ds, llvm::Type * ty);
size_t llds_alignment_of(LLVMDataStructures *ds, llvm::Type * ty);

llvm::GlobalVariable * llds_get_or_insert_global(LLVMDataStructures *ds, char * name, size_t len, llvm::Type * ty);
bool llds_global_has_initializer(llvm::GlobalVariable * g);
void llds_global_set_initializer(llvm::GlobalVariable * g, llvm::Constant * c);
llvm::Function* llds_get_function(LLVMDataStructures *ds, char * name, size_t len);

bool llds_verify_function(llvm::Function* f);
void llds_dump_function(llvm::Function* f);
void * llds_save_ip(LLVMDataStructures *ds);
void llds_restore_ip(LLVMDataStructures *ds, void* ip);
void llds_set_ip_for_instantiate(LLVMDataStructures* ds, llvm::Function* f);
llvm::Function * llds_create_function(LLVMDataStructures *ds, const char * name, size_t len, llvm::FunctionType * ty);
void llds_create_do_nothing(LLVMDataStructures *ds);
bool llds_function_is_empty(llvm::Function* f);
llvm::BasicBlock * llds_function_entry_basicblock(llvm::Function* f);
llvm::BasicBlock * llds_create_basicblock(LLVMDataStructures *ds, char * name, size_t len, llvm::Function * fn);
llvm::Value * llds_get_arg(llvm::Function * f, size_t i);
void llds_set_arg_name(llvm::Function * f, size_t i, char* name, size_t len);
llvm::Value * llds_create_struct_gep(LLVMDataStructures *ds, llvm::Type * ty, llvm::Value * val, size_t idx);
llvm::Value * llds_create_gep(LLVMDataStructures *ds, llvm::Type * ty, llvm::Value * val, llvm::Value * idx);
llvm::Value * llds_extract_value(LLVMDataStructures *ds, llvm::Value * val, size_t idx);
void llds_set_insert_point(LLVMDataStructures *ds, llvm::BasicBlock* bb);
llvm::Value * llds_create_alloca_sized(LLVMDataStructures *ds, llvm::Type * ty);
llvm::Value * llds_create_alloca_unsized(LLVMDataStructures *ds, llvm::Value* size);
void llds_create_store(LLVMDataStructures *ds, llvm::Value* val, llvm::Value* dst);
llvm::Value * llds_create_load(LLVMDataStructures *ds, llvm::Type* ty, llvm::Value* ptr);
void llds_create_memcpy(LLVMDataStructures *ds, llvm::Value* dst, llvm::Value* src, llvm::Value* size);
void llds_create_memset(LLVMDataStructures *ds, llvm::Value* dst, llvm::Value* val, llvm::Value* count);
void llds_create_br(LLVMDataStructures *ds, llvm::BasicBlock * bb);
void llds_create_cond_br(LLVMDataStructures *ds, llvm::Value * cond, llvm::BasicBlock * bb1, llvm::BasicBlock * bb2);
void llds_create_ret(LLVMDataStructures *ds, llvm::Value * v);
void llds_create_retvoid(LLVMDataStructures *ds);
void llds_create_unreachable(LLVMDataStructures *ds);
llvm::Value * llds_create_call(LLVMDataStructures *ds, llvm::Function* f, llvm::Value** args, size_t len);
llvm::Value * llds_create_call_with_fnty(LLVMDataStructures *ds, llvm::FunctionType * fnty, llvm::Function* f, llvm::Value** args, size_t len);
llvm::Value * llds_create_fat_ptr(LLVMDataStructures *ds, llvm::Value* p1, llvm::Value* p2, llvm::Value* p3);

llvm::Value * llds_create_ptr_to_int(LLVMDataStructures *ds, llvm::Value* ptr);
llvm::Value * llds_create_int_to_ptr(LLVMDataStructures *ds, llvm::Value* i);
llvm::Value * llds_create_zext(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty);
llvm::Value * llds_create_sext_or_trunc(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty);
llvm::Value * llds_create_zext_or_trunc(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty);
llvm::Value * llds_create_si_to_fp(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty);
llvm::Value * llds_create_ui_to_fp(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty);
llvm::Value * llds_create_fp_to_si(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty);
llvm::Value * llds_create_fp_to_ui(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty);
llvm::Value * llds_create_fp_trunc(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty);
llvm::Value * llds_create_fp_ext(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty);

llvm::Value * llds_create_add(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_sub(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_mul(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_sdiv(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_udiv(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_srem(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_urem(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_fadd(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_fsub(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_fmul(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_fdiv(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_frem(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_neg(LLVMDataStructures *ds, llvm::Value* val);
llvm::Value * llds_create_fneg(LLVMDataStructures *ds, llvm::Value* val);

llvm::Value * llds_create_logic_and(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_logic_or(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);

llvm::Value * llds_create_bit_and(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_bit_or(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_bit_xor(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_bit_shl(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_bit_lshr(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_bit_ashr(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_bit_not(LLVMDataStructures *ds, llvm::Value* val);
llvm::Value * llds_create_bit_reverse(LLVMDataStructures *ds, llvm::Value* arg);
llvm::Value * llds_create_bit_rotate_left(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_bit_rotate_right(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);

llvm::Value * llds_create_icmp_eq(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_icmp_ne(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_icmp_uge(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_icmp_ugt(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_icmp_ule(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_icmp_ult(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_icmp_sge(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_icmp_sgt(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_icmp_sle(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_icmp_slt(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_fcmp_oeq(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_fcmp_one(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_fcmp_ogt(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_fcmp_oge(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_fcmp_olt(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_fcmp_ole(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs);

llvm::Value * llds_create_select(LLVMDataStructures *ds, llvm::Value * cond, llvm::Value* lhs, llvm::Value* rhs);
llvm::Value * llds_create_phi(LLVMDataStructures *ds, llvm::Type * ty, size_t num);
void llds_phi_add_incoming(llvm::PHINode * phi, llvm::Value* val, llvm::BasicBlock * bb);
llvm::Value* llds_create_insert_value(LLVMDataStructures *ds, llvm::Value* agg, llvm::Value * val, size_t idx);

llvm::DIFile * llds_create_di_file(LLVMDataStructures *ds, const char * f, size_t len);
llvm::DISubprogram * llds_create_di_subprogram(
        LLVMDataStructures *ds,
        llvm::Function * f,
        const char * short_name,
        llvm::DIFile * file,
        unsigned int line
    );
llvm::DILocation* llds_create_di_location(
        LLVMDataStructures *ds,
        llvm::Function* func,
        unsigned int line,
        unsigned int column
    );
void llds_set_di_loc(LLVMDataStructures *ds, llvm::DILocation* di_loc);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif

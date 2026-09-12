#include "llvm_codegen_meta.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/TargetParser/Host.h"

#include <string>
#include "zinc_core_typemeta.h"

static std::vector<llvm::Constant *> all_type_meta;
static std::vector<llvm::Constant *> all_generic_type_meta;
static std::vector<llvm::Constant *> all_trait_impl;

static ZnTypeMetaKind builtin_type_kind(std::string name) {

    if (name == "Bool") { return ZnTypeMetaKind::TM_Bool; }
    if (name == "Char") { return ZnTypeMetaKind::TM_Char; }
    if (name == "Int8") { return ZnTypeMetaKind::TM_Int8; }
    if (name == "Int16") { return ZnTypeMetaKind::TM_Int16; }
    if (name == "Int32") { return ZnTypeMetaKind::TM_Int32; }
    if (name == "Int64") { return ZnTypeMetaKind::TM_Int64; }
    if (name == "Int") { return ZnTypeMetaKind::TM_Int; }
    if (name == "Short") { return ZnTypeMetaKind::TM_Short; }
    if (name == "UInt8") { return ZnTypeMetaKind::TM_UInt8; }
    if (name == "UInt16") { return ZnTypeMetaKind::TM_UInt16; }
    if (name == "UInt32") { return ZnTypeMetaKind::TM_UInt32; }
    if (name == "UInt64") { return ZnTypeMetaKind::TM_UInt64; }
    if (name == "UInt") { return ZnTypeMetaKind::TM_UInt; }
    if (name == "UShort") { return ZnTypeMetaKind::TM_UShort; }
    if (name == "F32") { return ZnTypeMetaKind::TM_Float32; }
    if (name == "F64") { return ZnTypeMetaKind::TM_Float64; }

    if (name == "Arc") { return ZnTypeMetaKind::TM_Arc; }
    if (name == "Weak") { return ZnTypeMetaKind::TM_Weak; }
    if (name == "Borrow") { return ZnTypeMetaKind::TM_ImmutBorrow; }
    if (name == "BorrowMut") { return ZnTypeMetaKind::TM_MutBorrow; }
    if (name == "RawImmutPointer") { return ZnTypeMetaKind::TM_ImmutRawPointer; }
    if (name == "RawMutPointer") { return ZnTypeMetaKind::TM_MutRawPointer; }

    if (name == "Tuple") { return ZnTypeMetaKind::TM_Tuple; }
    if (name == "Array") { return ZnTypeMetaKind::TM_Array; }

    return ZnTypeMetaKind::TM_Err;
}

llvm::Function* empty_copy_fn(LLVMDataStructures* llds) {
    static llvm::Function * empty_copy_fn = nullptr;
    if (empty_copy_fn == nullptr) {
        llvm::FunctionType * copy_fn_ty = llvm::FunctionType::get(llds->builder->getVoidTy(), {
            llvm::PointerType::get(*llds->ctx, 0),
            llvm::PointerType::get(*llds->ctx, 0)
        }, false);
        empty_copy_fn = llvm::Function::Create(copy_fn_ty, llvm::Function::InternalLinkage, "empty_copyfn", llds->module);
        auto * llbb = llvm::BasicBlock::Create(*llds->ctx, "", empty_copy_fn);
        llvm::IRBuilder<> builder{*llds->ctx};
        builder.SetInsertPoint(llbb);
        builder.CreateRetVoid();
    }
    return empty_copy_fn;
}

llvm::Function* empty_dtor_fn(LLVMDataStructures* llds) {
    static llvm::Function * empty_dtor_fn = nullptr;
    if (empty_dtor_fn == nullptr) {
        llvm::FunctionType * dtor_fn_ty = llvm::FunctionType::get(llds->builder->getVoidTy(), {
            llvm::PointerType::get(*llds->ctx, 0),              // void* obj
            llvm::PointerType::get(*llds->ctx, 0)                             // ZnTypeMeta* ty_meta
        }, false);
        empty_dtor_fn = llvm::Function::Create(dtor_fn_ty, llvm::Function::InternalLinkage, "empty_dtorfn", llds->module);
        auto * llbb = llvm::BasicBlock::Create(*llds->ctx, "", empty_dtor_fn);
        llvm::IRBuilder<> builder{*llds->ctx};
        builder.SetInsertPoint(llbb);
        builder.CreateRetVoid();
    }
    return empty_dtor_fn;
}

llvm::Function * codegen_tuple_array_sizefn(LLVMDataStructures* llds) {
    static llvm::Function* tuple_size_fn = nullptr;
    if (tuple_size_fn != nullptr) {
        return tuple_size_fn;
    }

    llvm::FunctionType * size_fn_ty = llvm::FunctionType::get(
        llds->builder->getInt64Ty(), 
        {
            llds_get_pointer_type(llds)                             // ZnTypeMeta* ty_meta
        }, false
    );

    std::string agg_size = "zinc_core_agg_size";
    llvm::Function * zinc_core_agg_size_fn = llds_get_ext_fn_in_core(llds, agg_size.c_str(), agg_size.size());

    // 搜集所有成员的 typemeta, 调用 zinc_core_agg_size
    auto * size_fn = llvm::Function::Create(size_fn_ty, llvm::Function::InternalLinkage, "tuple_array_sizefn", llds->module);
    auto * llbb = llvm::BasicBlock::Create(*llds->ctx, "", size_fn);
    llvm::IRBuilder<> builder{*llds->ctx};
    builder.SetInsertPoint(llbb);

    llvm::Value * type_meta = size_fn->getArg(0);
    llvm::Value * argc = builder.CreateStructGEP(llds->zn_type_meta, type_meta, 3);
    argc = builder.CreateLoad(builder.getInt8Ty(), argc);
    argc = builder.CreateZExt(argc, builder.getInt64Ty());
    llvm::Value * argv = builder.CreateStructGEP(llds->zn_type_meta, type_meta, 8);
    argv = builder.CreateLoad(llds_get_pointer_type(llds), argv);
    auto * call = builder.CreateCall(zinc_core_agg_size_fn, {argc, argv});
    builder.CreateRet(call);
    tuple_size_fn = size_fn;
    return size_fn;
}

llvm::Function * codegen_tuple_array_alignfn(LLVMDataStructures* llds) {
    
    llvm::FunctionType * align_fn_ty = llvm::FunctionType::get(
        llds->builder->getInt64Ty(), 
        {
            llds_get_pointer_type(llds)                             // ZnTypeMeta* ty_meta
        }, false
    );

    static llvm::Function* tuple_align_fn = nullptr;
    if (tuple_align_fn != nullptr) {
        return tuple_align_fn;
    }

    std::string agg_align = "zinc_core_agg_align";
    llvm::Function * zinc_core_agg_align_fn = llds_get_ext_fn_in_core(llds, agg_align.c_str(), agg_align.size());

    // 搜集所有成员的 typemeta, 调用 zinc_core_agg_align
    // Array 其实可以简单点，无需遍历所有成员
    auto * align_fn = llvm::Function::Create(align_fn_ty, llvm::Function::InternalLinkage, "tuple_array_alignfn", llds->module);
    auto * llbb = llvm::BasicBlock::Create(*llds->ctx, "", align_fn);
    llvm::IRBuilder<> builder{*llds->ctx};
    builder.SetInsertPoint(llbb);

    llvm::Value * type_meta = align_fn->getArg(0);
    llvm::Value * argc = builder.CreateStructGEP(llds->zn_type_meta, type_meta, 3);
    argc = builder.CreateLoad(builder.getInt8Ty(), argc);
    argc = builder.CreateZExt(argc, builder.getInt64Ty());
    llvm::Value * argv = builder.CreateStructGEP(llds->zn_type_meta, type_meta, 8);
    argv = builder.CreateLoad(llds_get_pointer_type(llds), argv);
    auto * call = builder.CreateCall(zinc_core_agg_align_fn, {argc, argv});
    builder.CreateRet(call);
    tuple_align_fn = align_fn;
    return align_fn;
}

llvm::GlobalVariable* codegen_builtin_typemeta(LLVMDataStructures* llds, char *name, size_t len, size_t byte_size) {
    std::string type_name{name, len};
    std::string meta_name = std::string("zn.meta.") + type_name;

    if (auto * gv = llds->module->getGlobalVariable(meta_name); gv && gv->hasInitializer()) {
        return gv; // 已经生成了
    }

    llvm::Constant * array = llvm::ConstantDataArray::getString(*llds->ctx, type_name, true);
    auto *name_gv = new llvm::GlobalVariable(
            *llds->module, array->getType(), true, llvm::GlobalValue::PrivateLinkage, array);
    name_gv->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

    auto * zero = llds->builder->getInt32(0);
    auto *name_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(array->getType(), name_gv, zero);
    llvm::Type * hashmap = llvm::ArrayType::get(llds_get_int8_type(llds), sizeof(HashMap));

    std::vector<llvm::Constant*> ll_fields = {
        name_ptr,
        llds_get_const_int8(llds, builtin_type_kind(type_name)), // data.kind,
        llds_get_const_int8(llds, 0), // data.ty_param_count,
        llds_get_const_int8(llds, 0), // data.ty_argc,
        llds_get_const_int8(llds, byte_size), // data.align,
        llds_get_const_int64(llds, byte_size),// data.size_byte,
        empty_copy_fn(llds), // data.copy_fn,
        empty_dtor_fn(llds),// data.dtor_fn,
        llds_get_const_null(llds), // data.ty_argv,
        llds_get_const_null(llds), // instantiated_from
        llvm::Constant::getNullValue(hashmap), // impls
    };
    llvm::Constant * agg = llvm::ConstantStruct::get(llds->zn_type_meta, ll_fields);
    
    if (auto * gv = llds->module->getGlobalVariable(meta_name)) {
        gv->setInitializer(agg);
        all_type_meta.push_back(gv);
        return gv;
    } else {
        auto *meta_gv = new llvm::GlobalVariable(
                *llds->module, llds->zn_type_meta, false, llvm::GlobalValue::ExternalLinkage, agg, meta_name);
        all_type_meta.push_back(meta_gv);
        return meta_gv;
    }
}

llvm::GlobalVariable* codegen_tuple_generictypemeta(LLVMDataStructures* llds) {
    std::string meta_name = "zn.meta.Tuple";
    if (auto * gv = llds->module->getGlobalVariable(meta_name); gv && gv->hasInitializer()) {
        return gv; // 已经生成了
    }

    llvm::Type * hashmap = llvm::ArrayType::get(llds_get_int8_type(llds), sizeof(HashMap));

    // name
    llvm::Constant * array = llvm::ConstantDataArray::getString(*llds->ctx, "Tuple", true);
    auto *name_gv = new llvm::GlobalVariable(
            *llds->module, array->getType(), true, llvm::GlobalValue::PrivateLinkage, array);
    name_gv->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

    auto * zero = llds->builder->getInt32(0);
    auto *name_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(array->getType(), name_gv, zero);

    std::string copy_fn = "zinc_core_tuple_copyfn";
    std::string dtor_fn = "zinc_core_tuple_dtorfn";

    std::vector<llvm::Constant*> ll_fields = {
        name_ptr,
        llds_get_const_int8(llds, ZnTypeMetaKind::TM_Tuple),// data.kind,
        llds_get_const_int8(llds, 0),// data.ty_param_count,
        codegen_tuple_array_sizefn(llds), // data.size_fn,
        codegen_tuple_array_alignfn(llds), // data.align_fn,
        llds_get_ext_fn_in_core(llds, copy_fn.c_str(), copy_fn.size()),// data.copy_fn,
        llds_get_ext_fn_in_core(llds, dtor_fn.c_str(), dtor_fn.size()), // data.dtor_fn,
        llds_get_const_null(llds), // data.where_cond_fn,
        llds_get_const_int64(llds, 0), // related_impl_cap
        llds_get_const_null(llds), // related_impls
        llvm::Constant::getNullValue(hashmap) // all_instantiated
    };

    llvm::Constant * agg = llvm::ConstantStruct::get(llds->zn_generic_type_meta, ll_fields);

    if (auto * gv = llds->module->getGlobalVariable(meta_name)) {
        gv->setInitializer(agg);
        all_generic_type_meta.push_back(gv);
        return gv;
    } else {
        auto *meta_gv = new llvm::GlobalVariable(
                *llds->module, llds->zn_generic_type_meta, false, llvm::GlobalValue::ExternalLinkage, agg, meta_name);
        all_generic_type_meta.push_back(meta_gv);
        return meta_gv;
    }
}

llvm::GlobalVariable* codegen_array_generictypemeta(LLVMDataStructures* llds) {
    std::string meta_name = "zn.meta.Array";
    if (auto * gv = llds->module->getGlobalVariable(meta_name); gv && gv->hasInitializer()) {
        return gv; // 已经生成了
    }
    llvm::Type * hashmap = llvm::ArrayType::get(llds_get_int8_type(llds), sizeof(HashMap));

    // name
    llvm::Constant * array = llvm::ConstantDataArray::getString(*llds->ctx, "Array", true);
    auto *name_gv = new llvm::GlobalVariable(
            *llds->module, array->getType(), true, llvm::GlobalValue::PrivateLinkage, array);
    name_gv->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

    auto * zero = llds->builder->getInt32(0);
    auto *name_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(array->getType(), name_gv, zero);

    std::string copy_fn = "zinc_core_array_copyfn";
    std::string dtor_fn = "zinc_core_array_dtorfn";

    std::vector<llvm::Constant*> ll_fields = {
        name_ptr,
        llds_get_const_int8(llds, ZnTypeMetaKind::TM_Array),// data.kind,
        llds_get_const_int8(llds, 0),// data.ty_param_count,
        codegen_tuple_array_sizefn(llds), // data.size_fn,
        codegen_tuple_array_alignfn(llds), // data.align_fn,
        llds_get_ext_fn_in_core(llds, copy_fn.c_str(), copy_fn.size()),// data.copy_fn,
        llds_get_ext_fn_in_core(llds, dtor_fn.c_str(), dtor_fn.size()), // data.dtor_fn,
        llds_get_const_null(llds), // data.where_cond_fn,
        llds_get_const_int64(llds, 0), // related_impl_cap
        llds_get_const_null(llds), // related_impls
        llvm::Constant::getNullValue(hashmap) // all_instantiated
    };

    llvm::Constant * agg = llvm::ConstantStruct::get(llds->zn_generic_type_meta, ll_fields);

    if (auto * gv = llds->module->getGlobalVariable(meta_name)) {
        gv->setInitializer(agg);
        all_generic_type_meta.push_back(gv);
        return gv;
    } else {
        auto *meta_gv = new llvm::GlobalVariable(
                *llds->module, llds->zn_generic_type_meta, false, llvm::GlobalValue::ExternalLinkage, agg, meta_name);
        all_generic_type_meta.push_back(meta_gv);
        return meta_gv;
    }
}

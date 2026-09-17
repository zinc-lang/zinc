
#include "llvm_data_structures.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/TargetParser/Host.h"

#include <string>
#include <cassert>
#include <cstdint>
#include <iostream>
#include <filesystem>
#include <map>

#include "zinc_core_map.h"
#include "zinc_core_typemeta.h"

LLVMDataStructures::LLVMDataStructures(std::string name) {
    this->ctx = new llvm::LLVMContext;
    this->module = new llvm::Module(name, *(this->ctx));
    this->builder = new llvm::IRBuilder(*(this->ctx));
    this->di_builder = new llvm::DIBuilder(*(this->module));
}

LLVMDataStructures::~LLVMDataStructures() {
    delete di_builder;
    delete builder;
    delete module;
    delete ctx;
}

void LLVMDataStructures::init_typemeta_structs() {
    // 把 HashMap 类型当成定长 byte 数组生成
    llvm::Type * int8 = llvm::Type::getInt8Ty(*this->ctx);
    llvm::Type * int64 = llvm::Type::getInt64Ty(*this->ctx);
    auto s = sizeof(HashMap);
    llvm::Type * hashmap = llvm::ArrayType::get(int8, s);
    llvm::Type * void_ptr = llvm::PointerType::get(*this->ctx, 0);

    //====================
    zn_type_meta = llvm::StructType::create(*this->ctx, "ZnTypeMeta");
    std::vector<llvm::Type*> elems1 = {
        void_ptr, // const char * name;
        int8, // uint8_t kind;
        int8, // uint8_t ty_param_count;
        int8, // uint8_t ty_argc;
        int8, // uint8_t align_byte;
        int64, // uintptr_t size_byte;

        void_ptr,// const void * copy_fn;
        void_ptr,// const void * dtor_fn;

        void_ptr,// struct ZnTypeMeta ** ty_argv;

        void_ptr,// struct ZnGenericTypeMeta * instantiated_from;

        hashmap,// HashMap          impls;
    };
    zn_type_meta->setBody(elems1);
    assert(this->module->getDataLayout().getTypeAllocSize(zn_type_meta) == sizeof(ZnTypeMeta)); // 验证

    //====================
    zn_generic_type_meta = llvm::StructType::create(*this->ctx, "ZnGenericTypeMeta");
    std::vector<llvm::Type*> elems2 = {
        void_ptr,// const char *    name;
        int8,// uint8_t         kind;
        int8,// uint8_t         type_param_count;

        void_ptr,// const void *    size_fn;
        void_ptr,// const void *    align_fn;

        void_ptr,// const void *    copy_fn;
        void_ptr,// const void *    dtor_fn;
        void_ptr,// const void *    where_cond_fn;

        int64, // size_t          related_impl_cap;
        void_ptr,// ZnTraitImpl **  related_impls;

        hashmap// HashMap         all_instantiated;
    };
    zn_generic_type_meta->setBody(elems2);
    assert(this->module->getDataLayout().getTypeAllocSize(zn_generic_type_meta) == sizeof(ZnGenericTypeMeta)); // 验证

    //====================
    zn_trait_impl = llvm::StructType::create(*this->ctx, "ZnTraitImpl");
    std::vector<llvm::Type*> elems3 = {
        int8,// uint8_t       type_param_count;
        int8,// uint8_t       assoc_type_count;

        void_ptr,// const void *  target_def;
        void_ptr,// const void *  trait_def;
        void_ptr,// const void *  trait_fn;

        void_ptr,// const void ** associated_types_fn;

        void_ptr,// const void *  func_table;
        void_ptr,// const void *  where_cond;
    };
    zn_trait_impl->setBody(elems3);
    assert(this->module->getDataLayout().getTypeAllocSize(zn_trait_impl) == sizeof(ZnTraitImpl));

    //====================
    zn_component = llvm::StructType::create(*this->ctx, "ZnComponent");
    std::vector<llvm::Type*> elems4 = {
        void_ptr,// const char *          name;
        void_ptr,// ZnTypeMeta**          non_generic_type_meta;
        void_ptr,// ZnGenericTypeMeta **  generic_type_meta;
        void_ptr,// ZnTraitImpl **        trait_impls;
        void_ptr,// struct ZnComponent ** imported_components;
        int8,// int8_t has_init;
    };
    zn_component->setBody(elems4);
    assert(this->module->getDataLayout().getTypeAllocSize(zn_component) == sizeof(ZnComponent));

    //====================
    // 构造 fat pointer type
    std::vector<llvm::Type*> elements;
    elements.push_back(void_ptr);
    elements.push_back(void_ptr);
    elements.push_back(void_ptr);
    zn_fat_ptr = llvm::StructType::get(*this->ctx, elements, false);
}


LLVMDataStructures * llds_create(const char * name, unsigned long len) {

    std::string n{name, (size_t)len};
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    // llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    auto* llds = new LLVMDataStructures(n);

    std::string target_triple = llvm::sys::getDefaultTargetTriple();
    llvm::Triple triple {target_triple};
    llds->module->setTargetTriple(triple);

    std::string err_msg;
    auto Target = llvm::TargetRegistry::lookupTarget(triple, err_msg);
    if (!Target) {
        llvm::errs() << err_msg;
        return nullptr;
    }
    auto CPU = "generic";
    auto Features = "";
    llvm::TargetOptions opt;
    llds->target_machine = Target->createTargetMachine(triple, CPU, Features, opt, llvm::Reloc::Model::PIC_);
    llds->module->setDataLayout(llds->target_machine->createDataLayout());

    llds->init_typemeta_structs();

    ///// FIXME
    
    return llds;
}

void llds_setup_compile_unit(LLVMDataStructures *llds, const char * entry, size_t len) {
    std::string entry_source_file{entry, len};
    std::filesystem::path p(entry_source_file);
    auto name = p.filename().string();
    auto dir = p.parent_path().string();
    llvm::DIFile * dif = llds->di_builder->createFile(name, dir);
    llds->di_compile_unit = llds->di_builder->createCompileUnit(
        llvm::dwarf::DW_LANG_C,
        dif,
        llds->module->getName(),
        false,
        "",
        0,
        "",
        llvm::DICompileUnit::FullDebug
    );

    llds->entry_source_file = name;
}

void llds_destroy(LLVMDataStructures *p) {
    delete p;
}

void llds_dump_module_llir(LLVMDataStructures *ds) {
    ds->module->print(llvm::outs(), nullptr);
}

llvm::Type * llds_get_ZnTypeMeta(LLVMDataStructures *ds) {
    return ds->zn_type_meta;
}

llvm::Type * llds_get_ZnGenericTypeMeta(LLVMDataStructures *ds) {
    return ds->zn_generic_type_meta;
}

llvm::Type * llds_get_ZnTraitImpl(LLVMDataStructures *ds) {
    return ds->zn_trait_impl;
}

llvm::Type * llds_get_ZnComponent(LLVMDataStructures *ds) {
    return ds->zn_component;
}

llvm::Type * llds_get_fat_ptr_type(LLVMDataStructures *ds) {
    return ds->zn_fat_ptr;
}

llvm::GlobalVariable * llds_get_or_insert_global(LLVMDataStructures *llds, char * n, size_t len, llvm::Type * ty) {
    std::string name{n, len};
    return llds->module->getOrInsertGlobal(name, ty);
}
bool llds_global_has_initializer(llvm::GlobalVariable * g) {
    return g && g->hasInitializer();
}
void llds_global_set_initializer(llvm::GlobalVariable * g, llvm::Constant * c) {
    if (g) g->setInitializer(c);
}
llvm::Function* llds_get_function(LLVMDataStructures *ds, char * name, size_t len) {
    std::string fname{name, len};
    return ds->module->getFunction(fname);
}

llvm::Type * llds_get_pointer_type(LLVMDataStructures *ds) {
    return llvm::PointerType::get(*ds->ctx, 0);
}
llvm::Type * llds_get_void_type(LLVMDataStructures *ds) {
    return ds->builder->getVoidTy();
}
llvm::Type * llds_get_int1_type(LLVMDataStructures *ds) {
    return llvm::Type::getInt1Ty(*ds->ctx);
}
llvm::Type * llds_get_int8_type(LLVMDataStructures *ds) {
    return llvm::Type::getInt8Ty(*ds->ctx);
}
llvm::Type * llds_get_int16_type(LLVMDataStructures *ds) {
    return llvm::Type::getInt16Ty(*ds->ctx);
}
llvm::Type * llds_get_int32_type(LLVMDataStructures *ds) {
    return llvm::Type::getInt32Ty(*ds->ctx);
}
llvm::Type * llds_get_int64_type(LLVMDataStructures *ds) {
    return llvm::Type::getInt64Ty(*ds->ctx);
}
llvm::Type * llds_get_f32_type(LLVMDataStructures *ds) {
    return llvm::Type::getFloatTy(*ds->ctx);
}
llvm::Type * llds_get_f64_type(LLVMDataStructures *ds) {
    return llvm::Type::getDoubleTy(*ds->ctx);
}
llvm::FunctionType * llds_create_function_type(LLVMDataStructures *ds, llvm::Type * ret, llvm::Type ** tys, size_t len) {
    std::vector<llvm::Type*> params{tys, tys+len};
    return llvm::FunctionType::get(
        ret, params, false
    );
}

llvm::Type * llds_type_of_value(llvm::Value* v) {
    return v->getType();
}

llvm::Type * llds_get_struct_type(LLVMDataStructures *ds, llvm::Type ** tys, size_t len) {
    std::vector<llvm::Type*> elements{tys, tys+len};
    auto ty = llvm::StructType::get(*ds->ctx, elements, false);
    return ty;
}
bool llds_is_struct_type(llvm::Type * ty) {
    return ty->isStructTy();
}
bool llds_is_pointer_type(llvm::Type * ty) {
    return ty->isPointerTy();
}
llvm::Type * llds_get_array_type_of(LLVMDataStructures *ds, llvm::Type* ty, size_t size) {
    return llvm::ArrayType::get(ty, size);
}
llvm::Type * llds_get_byte_array_type(LLVMDataStructures *ds, size_t size) {
    return llvm::ArrayType::get(ds->builder->getInt8Ty(), size);
}
llvm::Type * llds_get_ptr_array_type(LLVMDataStructures *ds, size_t count) {
    return llvm::ArrayType::get(llds_get_pointer_type(ds), count);
}

llvm::Constant * llds_get_const_null(LLVMDataStructures *ds) {
    auto * void_ptr = llds_get_pointer_type(ds);
    return llvm::ConstantPointerNull::get(void_ptr);
}
llvm::Constant * llds_get_null_value_of(LLVMDataStructures *ds, llvm::Type* t) {
    return llvm::Constant::getNullValue(t);
}
llvm::Constant * llds_get_const_int1(LLVMDataStructures *ds, bool v) {
    return ds->builder->getInt1(v);
}
llvm::Constant * llds_get_const_int8(LLVMDataStructures *ds, int8_t v) {
    return ds->builder->getInt8(v);
}
llvm::Constant * llds_get_const_int16(LLVMDataStructures *ds, int16_t v) {
    return ds->builder->getInt16(v);
}
llvm::Constant * llds_get_const_int32(LLVMDataStructures *ds, int32_t v) {
    return ds->builder->getInt32(v);
}
llvm::Constant * llds_get_const_int64(LLVMDataStructures *ds, int64_t v) {
    return ds->builder->getInt64(v);
}
llvm::Constant * llds_get_const_f32(LLVMDataStructures *ds, float v) {
    return llvm::ConstantFP::get(ds->builder->getFloatTy(), llvm::APFloat(v));
}
llvm::Constant * llds_get_const_f64(LLVMDataStructures *ds, double v) {
    return llvm::ConstantFP::get(ds->builder->getDoubleTy(), llvm::APFloat(v));
}
llvm::Constant * llds_get_const_struct(LLVMDataStructures *ds, llvm::Constant ** fields, size_t len, llvm::StructType* ty) {
    std::vector<llvm::Constant*> ll_fields{fields, fields+len};
    return llvm::ConstantStruct::get(ty, ll_fields);
}
llvm::Constant * llds_get_const_array(LLVMDataStructures *ds, llvm::Constant ** elems, size_t len, llvm::StructType* ty) {
    std::vector<llvm::Constant*> ll_elems{elems, elems+len};
    llvm::ArrayType* array_ty = llvm::ArrayType::get(ty, ll_elems.size() );
    llvm::Constant * array = llvm::ConstantArray::get(array_ty, ll_elems);
    return array;
}
llvm::Constant * llds_get_const_array_gv(LLVMDataStructures *ds, llvm::Constant ** elems, size_t len, llvm::StructType* ty) {
    std::vector<llvm::Constant*> ll_elems{elems, elems+len};
    llvm::Constant * null_ptr = llvm::ConstantPointerNull::get(ty);
    ll_elems.push_back(null_ptr); // 增加一个 null 结尾的元素

    llvm::ArrayType* array_ty = llvm::ArrayType::get(ty, ll_elems.size() );
    llvm::Constant * array = llvm::ConstantArray::get(array_ty, ll_elems);
    auto *arr_gv = new llvm::GlobalVariable(
            *ds->module, array->getType(), true, llvm::GlobalValue::PrivateLinkage, array);
    arr_gv->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    return arr_gv;
}
llvm::Constant * llds_get_const_string(LLVMDataStructures *ds, char * s, size_t len) {
    std::string str{s, len};
    llvm::Constant * array = llvm::ConstantDataArray::getString(*ds->ctx, str, true, true);
    auto *name_gv = new llvm::GlobalVariable(
            *ds->module, array->getType(), true, llvm::GlobalValue::PrivateLinkage, array);
    name_gv->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

    auto * zero = ds->builder->getInt32(0);
    auto * name_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(array->getType(), name_gv, zero);
    return name_ptr;
}

llvm::Function * llds_get_ext_fn_in_core(LLVMDataStructures *ds, const char * name, size_t len) {
    std::string fn_name{name, len};
    llvm::FunctionType * copy_fn_ty = llvm::FunctionType::get(ds->builder->getVoidTy(), {
        llvm::PointerType::get(*ds->ctx, 0),
        llvm::PointerType::get(*ds->ctx, 0)
    }, false);
    llvm::FunctionType * inc_fn_ty = llvm::FunctionType::get(ds->builder->getVoidTy(), {
        llvm::PointerType::get(*ds->ctx, 0)
    }, false);
    llvm::FunctionType * dec_fn_ty = llvm::FunctionType::get(ds->builder->getVoidTy(), {
        llvm::PointerType::get(*ds->ctx, 0),
        llvm::PointerType::get(*ds->ctx, 0)
    }, false);
    llvm::FunctionType * dtor_fn_ty = llvm::FunctionType::get(ds->builder->getVoidTy(), {
        llvm::PointerType::get(*ds->ctx, 0),              // void* obj
        llvm::PointerType::get(*ds->ctx, 0)                             // ZnTypeMeta* ty_meta
    }, false);

    if (fn_name == "zinc_core_instantiated") {
        static llvm::Function *  fn_zinc_core_instantiated = nullptr;
        if (fn_zinc_core_instantiated == nullptr) { // 声明一个函数
            llvm::FunctionType * fn_ty = llvm::FunctionType::get(
                llvm::PointerType::get(*ds->ctx, 0), 
                {
                    llvm::PointerType::get(*ds->ctx, 0),
                    ds->builder->getInt8Ty(),
                    llvm::PointerType::get(*ds->ctx, 0)
                },
                false);
            fn_zinc_core_instantiated = llds_create_function(ds, fn_name.c_str(), fn_name.size(), fn_ty);
        }
        return fn_zinc_core_instantiated;
    }
    if (fn_name == "zinc_core_instantiate_array") {
        static llvm::Function * fn_zinc_core_instantiate_array = nullptr;
        if (fn_zinc_core_instantiate_array == nullptr) { // 声明一个函数
            llvm::FunctionType * fn_ty = llvm::FunctionType::get(
                llvm::PointerType::get(*ds->ctx, 0), 
                {
                    llvm::PointerType::get(*ds->ctx, 0),
                    ds->builder->getInt8Ty(),
                    llvm::PointerType::get(*ds->ctx, 0)
                },
                false);
            fn_zinc_core_instantiate_array = llds_create_function(ds, fn_name.c_str(), fn_name.size(), fn_ty);
        }
        return fn_zinc_core_instantiate_array;
    }
    if (fn_name == "zinc_core_find_impl") {
        static llvm::Function * fn_zinc_core_find_impl = nullptr;
        if (fn_zinc_core_find_impl == nullptr) { // 声明一个函数
            llvm::FunctionType * fn_ty = llvm::FunctionType::get(
                llvm::PointerType::get(*ds->ctx, 0), 
                {
                    llvm::PointerType::get(*ds->ctx, 0),
                    llvm::PointerType::get(*ds->ctx, 0)
                },
                false);
            fn_zinc_core_find_impl = llds_create_function(ds, fn_name.c_str(), fn_name.size(), fn_ty);
        }
        return fn_zinc_core_find_impl;
    }
    if (fn_name == "zinc_core_panic") {
        static llvm::Function * fn_zinc_core_panic = nullptr;
        if (fn_zinc_core_panic == nullptr) {
            llvm::FunctionType * fn_ty = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ds->ctx), 
            {
                llvm::PointerType::get(*ds->ctx, 0), // file
                ds->builder->getInt64Ty(),         // line
                llvm::PointerType::get(*ds->ctx, 0), // msg
                ds->builder->getInt64Ty(),         // len
            },
            false);
            fn_zinc_core_panic = llds_create_function(ds, fn_name.c_str(), fn_name.size(), fn_ty);
        }
        return fn_zinc_core_panic;
    }
    if (fn_name == "zinc_core_conditional_destruct") {
        static llvm::Function * fn_zinc_core_conditional_destruct = nullptr;
        if (fn_zinc_core_conditional_destruct == nullptr) {
            llvm::FunctionType * fn_ty = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ds->ctx), 
            {
                llvm::PointerType::get(*ds->ctx, 0),
                llvm::PointerType::get(*ds->ctx, 0),
                llvm::PointerType::get(*ds->ctx, 0),
                llvm::PointerType::get(*ds->ctx, 0)
            },
            false);
            fn_zinc_core_conditional_destruct = llds_create_function(ds, fn_name.c_str(), fn_name.size(), fn_ty);
        }
        return fn_zinc_core_conditional_destruct;
    }
    if (fn_name == "zinc_core_arc_copyfn") {
        static llvm::Function * arc_copy_fn = nullptr;
        if (arc_copy_fn == nullptr) {
            arc_copy_fn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), copy_fn_ty);
        }
        return arc_copy_fn;
    }
    if (fn_name == "zinc_core_inc_strong_atomic") {
        static llvm::Function * inc_strong_atomic = nullptr;
        if (inc_strong_atomic == nullptr) {
            inc_strong_atomic = llds_create_function(ds, fn_name.c_str(), fn_name.size(), inc_fn_ty);
        }
        return inc_strong_atomic;
    }
    if (fn_name == "zinc_core_inc_strong") {
        static llvm::Function * inc_strong = nullptr;
        if (inc_strong == nullptr) {
            inc_strong = llds_create_function(ds, fn_name.c_str(), fn_name.size(), inc_fn_ty);
        }
        return inc_strong;
    }
    if (fn_name == "zinc_core_dec_strong_atomic") {
        static llvm::Function * dec_strong_atomic = nullptr;
        if (dec_strong_atomic == nullptr) {
            dec_strong_atomic = llds_create_function(ds, fn_name.c_str(), fn_name.size(), dec_fn_ty);
        }
        return dec_strong_atomic;
    }
    if (fn_name == "zinc_core_dec_strong") {
        static llvm::Function * dec_strong = nullptr;
        if (dec_strong == nullptr) {
            dec_strong = llds_create_function(ds, fn_name.c_str(), fn_name.size(), dec_fn_ty);
        }
        return dec_strong;
    }
    if (fn_name == "zinc_core_dec_weak_atomic") {
        static llvm::Function * dec_weak_atomic = nullptr;
        if (dec_weak_atomic == nullptr) {
            dec_weak_atomic = llds_create_function(ds, fn_name.c_str(), fn_name.size(), dec_fn_ty);
        }
        return dec_weak_atomic;
    }
    if (fn_name == "zinc_core_dec_weak") {
        static llvm::Function * dec_weak = nullptr;
        if (dec_weak == nullptr) {
            dec_weak = llds_create_function(ds, fn_name.c_str(), fn_name.size(), dec_fn_ty);
        }
        return dec_weak;
    }
    if (fn_name == "zinc_core_weak_copyfn") {
        static llvm::Function * weak_copy_fn = nullptr;
        if (weak_copy_fn == nullptr) {
            weak_copy_fn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), copy_fn_ty);
        }
        return weak_copy_fn;
    }
    if (fn_name == "zinc_core_inc_weak_atomic") {
        static llvm::Function * inc_weak_atomic = nullptr;
        if (inc_weak_atomic == nullptr) {
            inc_weak_atomic = llds_create_function(ds, fn_name.c_str(), fn_name.size(), inc_fn_ty);
        }
        return inc_weak_atomic;
    }
    if (fn_name == "zinc_core_inc_weak") {
        static llvm::Function * inc_weak = nullptr;
        if (inc_weak == nullptr) {
            inc_weak = llds_create_function(ds, fn_name.c_str(), fn_name.size(), inc_fn_ty);
        }
        return inc_weak;
    }
    if (fn_name == "zinc_core_option_copyfn") {
        static llvm::Function * fn_zinc_core_option_copyfn = nullptr;
        if (fn_zinc_core_option_copyfn == nullptr) {
            fn_zinc_core_option_copyfn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), copy_fn_ty);
        }
        return fn_zinc_core_option_copyfn;
    }
    if (fn_name == "zinc_core_tuple_copyfn") {
        static llvm::Function * fn_zinc_core_tuple_copyfn = nullptr;
        if (fn_zinc_core_tuple_copyfn == nullptr) {
            fn_zinc_core_tuple_copyfn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), copy_fn_ty);
        }
        return fn_zinc_core_tuple_copyfn;
    }
    if (fn_name == "zinc_core_array_copyfn") {
        static llvm::Function * fn_zinc_core_array_copyfn = nullptr;
        if (fn_zinc_core_array_copyfn == nullptr) {
            fn_zinc_core_array_copyfn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), copy_fn_ty);
        }
        return fn_zinc_core_array_copyfn;
    }
    if (fn_name == "zinc_core_arc_dtorfn") {
        static llvm::Function * fn_zinc_core_arc_dtorfn = nullptr;
        if (fn_zinc_core_arc_dtorfn == nullptr) {
            fn_zinc_core_arc_dtorfn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), dtor_fn_ty);
        }
        return fn_zinc_core_arc_dtorfn;
    }
    if (fn_name == "zinc_core_weak_dtorfn") {
        static llvm::Function * fn_zinc_core_weak_dtorfn = nullptr;
        if (fn_zinc_core_weak_dtorfn == nullptr) {
            fn_zinc_core_weak_dtorfn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), dtor_fn_ty);
        }
        return fn_zinc_core_weak_dtorfn;
    }
    if (fn_name == "zinc_core_tuple_dtorfn") {
        static llvm::Function * fn_zinc_core_tuple_dtorfn = nullptr;
        if (fn_zinc_core_tuple_dtorfn == nullptr) {
            fn_zinc_core_tuple_dtorfn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), dtor_fn_ty);
        }
        return fn_zinc_core_tuple_dtorfn;
    }
    if (fn_name == "zinc_core_array_dtorfn") {
        static llvm::Function * fn_zinc_core_array_dtorfn = nullptr;
        if (fn_zinc_core_array_dtorfn == nullptr) {
            fn_zinc_core_array_dtorfn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), dtor_fn_ty);
        }
        return fn_zinc_core_array_dtorfn;
    }
    if (fn_name == "zinc_core_option_dtorfn") {
        static llvm::Function * fn_zinc_core_option_dtorfn = nullptr;
        if (fn_zinc_core_option_dtorfn == nullptr) {
            fn_zinc_core_option_dtorfn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), dtor_fn_ty);
        }
        return fn_zinc_core_option_dtorfn;
    }
    if (fn_name == "zinc_core_agg_size") {
        static llvm::Function * fn_zinc_core_agg_size = nullptr;
        if (fn_zinc_core_agg_size == nullptr) {
            llvm::FunctionType * zinc_core_agg_size_ty = llvm::FunctionType::get(
                ds->builder->getInt64Ty(),
                {
                    ds->builder->getInt64Ty(),
                    llvm::PointerType::get(*ds->ctx, 0)
                }, false
            );
            fn_zinc_core_agg_size = llds_create_function(ds, fn_name.c_str(), fn_name.size(), zinc_core_agg_size_ty);
        }
        return fn_zinc_core_agg_size;
    }
    if (fn_name == "zinc_core_agg_align") {
        static llvm::Function * fn_zinc_core_agg_align = nullptr;
        if (fn_zinc_core_agg_align == nullptr) {
            llvm::FunctionType * zinc_core_agg_align_ty = llvm::FunctionType::get(
                ds->builder->getInt64Ty(),
                {
                    ds->builder->getInt64Ty(),
                    llvm::PointerType::get(*ds->ctx, 0)
                }, false
            );
            fn_zinc_core_agg_align = llds_create_function(ds, fn_name.c_str(), fn_name.size(), zinc_core_agg_align_ty);
        }
        return fn_zinc_core_agg_align;
    }
    if (fn_name == "zinc_core_ptr_sizefn") {
        static llvm::Function * fn_zinc_core_ptr_size = nullptr;
        if (fn_zinc_core_ptr_size == nullptr) {
            llvm::FunctionType * sizefn_ty = llvm::FunctionType::get(
                ds->builder->getInt64Ty(),
                {
                    llvm::PointerType::get(*ds->ctx, 0)
                }, false
            );
            fn_zinc_core_ptr_size = llds_create_function(ds, fn_name.c_str(), fn_name.size(), sizefn_ty);
        }
        return fn_zinc_core_ptr_size;
    }
    if (fn_name == "zinc_core_ptr_alignfn") {
        static llvm::Function * fn_zinc_core_ptr_align = nullptr;
        if (fn_zinc_core_ptr_align == nullptr) {
            llvm::FunctionType * alignfn_ty = llvm::FunctionType::get(
                ds->builder->getInt64Ty(),
                {
                    llvm::PointerType::get(*ds->ctx, 0)
                }, false
            );
            fn_zinc_core_ptr_align = llds_create_function(ds, fn_name.c_str(), fn_name.size(), alignfn_ty);
        }
        return fn_zinc_core_ptr_align;
    }
    if (fn_name == "zinc_core_tuple_sizefn") {
        static llvm::Function * fn_zinc_core_tuple_size = nullptr;
        if (fn_zinc_core_tuple_size == nullptr) {
            llvm::FunctionType * sizefn_ty = llvm::FunctionType::get(
                ds->builder->getInt64Ty(),
                {
                    llvm::PointerType::get(*ds->ctx, 0)
                }, false
            );
            fn_zinc_core_tuple_size = llds_create_function(ds, fn_name.c_str(), fn_name.size(), sizefn_ty);
        }
        return fn_zinc_core_tuple_size;
    }
    if (fn_name == "zinc_core_array_sizefn") {
        static llvm::Function * fn_zinc_core_array_sizefn = nullptr;
        if (fn_zinc_core_array_sizefn == nullptr) {
            llvm::FunctionType * sizefn_ty = llvm::FunctionType::get(
                ds->builder->getInt64Ty(),
                {
                    llvm::PointerType::get(*ds->ctx, 0)
                }, false
            );
            fn_zinc_core_array_sizefn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), sizefn_ty);
        }
        return fn_zinc_core_array_sizefn;
    }
    if (fn_name == "zinc_core_option_sizefn") {
        static llvm::Function * fn_zinc_core_option_sizefn = nullptr;
        if (fn_zinc_core_option_sizefn == nullptr) {
            llvm::FunctionType * sizefn_ty = llvm::FunctionType::get(
                ds->builder->getInt64Ty(),
                {
                    llvm::PointerType::get(*ds->ctx, 0)
                }, false
            );
            fn_zinc_core_option_sizefn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), sizefn_ty);
        }
        return fn_zinc_core_option_sizefn;
    }
    if (fn_name == "zinc_core_tuple_alignfn") {
        static llvm::Function * fn_zinc_core_tuple_alignfn = nullptr;
        if (fn_zinc_core_tuple_alignfn == nullptr) {
            llvm::FunctionType * alignfn_ty = llvm::FunctionType::get(
                ds->builder->getInt64Ty(),
                {
                    llvm::PointerType::get(*ds->ctx, 0)
                }, false
            );
            fn_zinc_core_tuple_alignfn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), alignfn_ty);
        }
        return fn_zinc_core_tuple_alignfn;
    }
    if (fn_name == "zinc_core_array_alignfn") {
        static llvm::Function * fn_zinc_core_array_alignfn = nullptr;
        if (fn_zinc_core_array_alignfn == nullptr) {
            llvm::FunctionType * alignfn_ty = llvm::FunctionType::get(
                ds->builder->getInt64Ty(),
                {
                    llvm::PointerType::get(*ds->ctx, 0)
                }, false
            );
            fn_zinc_core_array_alignfn = llds_create_function(ds, fn_name.c_str(), fn_name.size(), alignfn_ty);
        }
        return fn_zinc_core_array_alignfn;
    }
    if (fn_name == "zinc_core_alloc_boxed") {
        static llvm::Function * fn_box = nullptr;
        if (fn_box == nullptr) {
            llvm::FunctionType * boxfn_ty = llvm::FunctionType::get(
                llvm::PointerType::get(*ds->ctx, 0),
                {
                    ds->builder->getInt64Ty()
                }, false
            );
            fn_box = llds_create_function(ds, fn_name.c_str(), fn_name.size(), boxfn_ty);
        }
        return fn_box;
    }
    if (fn_name == "zinc_core_fill_in_repeat") {
        static llvm::Function * fn_repeat = nullptr;
        if (fn_repeat == nullptr) {
            llvm::FunctionType * repeatfn_ty = llvm::FunctionType::get(
                ds->builder->getVoidTy(),
                {
                    llvm::PointerType::get(*ds->ctx, 0),
                    ds->builder->getInt64Ty(),
                    llvm::PointerType::get(*ds->ctx, 0),
                    llvm::PointerType::get(*ds->ctx, 0)
                }, false
            );
            fn_repeat = llds_create_function(ds, fn_name.c_str(), fn_name.size(), repeatfn_ty);
        }
        return fn_repeat;
    }
    if (fn_name == "zinc_core_mem_compare") {
        static llvm::Function * fn_cmp = nullptr;
        if (fn_cmp == nullptr) {
            llvm::FunctionType * cmpfn_ty = llvm::FunctionType::get(
                ds->builder->getInt32Ty(),
                {
                    llvm::PointerType::get(*ds->ctx, 0),
                    llvm::PointerType::get(*ds->ctx, 0),
                    ds->builder->getInt64Ty(),
                }, false
            );
            fn_cmp = llds_create_function(ds, fn_name.c_str(), fn_name.size(), cmpfn_ty);
        }
        return fn_cmp;
    }
    if (fn_name == "zinc_core_option_some_fn") {
        static llvm::Function * fn_some = nullptr;
        if (fn_some == nullptr) {
            llvm::FunctionType * somefn_ty = llvm::FunctionType::get(
                ds->builder->getVoidTy(),
                {
                    llvm::PointerType::get(*ds->ctx, 0),
                    ds->builder->getInt64Ty(),
                    llvm::PointerType::get(*ds->ctx, 0),
                    llvm::PointerType::get(*ds->ctx, 0),
                }, false
            );
            fn_some = llds_create_function(ds, fn_name.c_str(), fn_name.size(), somefn_ty);
        }
        return fn_some;
    }
    if (fn_name == "zinc_core_option_none_fn") {
        static llvm::Function * fn_none = nullptr;
        if (fn_none == nullptr) {
            llvm::FunctionType * nonefn_ty = llvm::FunctionType::get(
                ds->builder->getVoidTy(),
                {
                    llvm::PointerType::get(*ds->ctx, 0),
                    ds->builder->getInt64Ty(),
                    llvm::PointerType::get(*ds->ctx, 0),
                }, false
            );
            fn_none = llds_create_function(ds, fn_name.c_str(), fn_name.size(), nonefn_ty);
        }
        return fn_none;
    }
    if (fn_name == "zinc_core_component_init") {
        static llvm::Function * fn_init = nullptr;
        if (fn_init == nullptr) {
            llvm::FunctionType * initfn_ty = llvm::FunctionType::get(
                ds->builder->getVoidTy(),
                {
                    llvm::PointerType::get(*ds->ctx, 0)
                }, false
            );
            fn_init = llds_create_function(ds, fn_name.c_str(), fn_name.size(), initfn_ty);
        }
        return fn_init;
    }
    if (fn_name == "zinc_core_set_main_args") {
        static llvm::Function * fn_set_arg = nullptr;
        if (fn_set_arg == nullptr) {
            llvm::FunctionType * fn_ty = llvm::FunctionType::get(
                ds->builder->getVoidTy(),
                {
                    ds->builder->getInt32Ty(),
                    llvm::PointerType::get(*ds->ctx, 0)
                }, false
            );
            fn_set_arg = llds_create_function(ds, fn_name.c_str(), fn_name.size(), fn_ty);
        }
        return fn_set_arg;
    }
    if (fn_name == "zinc_core_protect_stack") {
        static llvm::Function * fn_protect_stack = nullptr;
        if (fn_protect_stack == nullptr) {
            llvm::FunctionType * fn_ty = llvm::FunctionType::get(
                ds->builder->getVoidTy(),
                {},
                false
            );
            fn_protect_stack = llds_create_function(ds, fn_name.c_str(), fn_name.size(), fn_ty);
        }
        return fn_protect_stack;
    }

    return nullptr;
}

size_t llds_storage_size_of(LLVMDataStructures *ds, llvm::Type * ty) {
    return ds->module->getDataLayout().getTypeAllocSize(ty);
}
size_t llds_alignment_of(LLVMDataStructures *ds, llvm::Type * ty) {
    return ds->module->getDataLayout().getPrefTypeAlign(ty).value();
}
bool llds_verify_function(llvm::Function* f) {
    if (f == nullptr) return false;

    bool r = llvm::verifyFunction(*f, &llvm::errs());
    if (r) {
        llvm::errs() << "[Debug] LLVM verify failed: " << f->getName() << "\n";
        // dump() is compiled out of Release LLVM; print() is always available.
        f->print(llvm::errs());
        llvm::errs() << "\n=======================\n";
    }
    return !r;
}
void llds_dump_function(llvm::Function* f) {
    if (f == nullptr) return;

    f->print(llvm::outs());
    llvm::outs() << "\n";
}
void * llds_save_ip(LLVMDataStructures *ds) {
    auto ip = ds->builder->saveIP();
    auto * pip = new llvm::IRBuilderBase::InsertPoint(ip);
    return pip;
}
void llds_restore_ip(LLVMDataStructures *ds, void* ip) {
    assert(ip != nullptr);
    auto * pip = (llvm::IRBuilderBase::InsertPoint *)ip;
    ds->builder->restoreIP(*pip);
    delete pip;
}
void llds_set_ip_for_instantiate(LLVMDataStructures *ds, llvm::Function* f) {
    auto bb = &f->getEntryBlock();
    if (auto * t = bb->getTerminatorOrNull()) {
        ds->builder->SetInsertPoint(t);
    } else {
        ds->builder->SetInsertPoint(bb);
    }
}

llvm::Function * llds_create_function(LLVMDataStructures *ds, const char * name, size_t len, llvm::FunctionType * ty) {
    std::string fname{name, len};
    if (auto * f = ds->module->getFunction(fname)) {
        return f;
    }
    return llvm::Function::Create(ty, llvm::Function::ExternalLinkage, fname, ds->module);
}
void llds_create_do_nothing(LLVMDataStructures *ds) {
    llvm::Function *do_nothing = llvm::Intrinsic::getOrInsertDeclaration(
        ds->module, llvm::Intrinsic::donothing
    );
    ds->builder->CreateCall(do_nothing);
}
bool llds_function_is_empty(llvm::Function* f) {
    return f->empty();
}
llvm::BasicBlock * llds_function_entry_basicblock(llvm::Function* f) {
    if (f->empty()) {
        return nullptr;
    }
    return &f->getEntryBlock();
}
llvm::BasicBlock * llds_create_basicblock(LLVMDataStructures *ds, char * name, size_t len, llvm::Function * fn) {
    std::string bbname{name, len};
    return llvm::BasicBlock::Create(*ds->ctx, bbname, fn);
}
llvm::Value * llds_get_arg(llvm::Function * f, size_t i) {
    return f->getArg(i);
}
void llds_set_arg_name(llvm::Function * f, size_t i, char* name, size_t len) {
    std::string arg_name{name, len};
    f->getArg(i)->setName(arg_name);
}
llvm::Value * llds_create_struct_gep(LLVMDataStructures *ds, llvm::Type * ty, llvm::Value * val, size_t idx) {
    return ds->builder->CreateStructGEP(ty, val, idx);
}
llvm::Value * llds_create_gep(LLVMDataStructures *ds, llvm::Type * ty, llvm::Value * val, llvm::Value * idx) {
    return ds->builder->CreateGEP(ty, val, {idx});
}
llvm::Value * llds_extract_value(LLVMDataStructures *ds, llvm::Value * val, size_t idx) {
    if (!val->getType()->isStructTy()) {
        llvm::errs() << "[BUG] extract value from a wrong type:";
        val->getType()->print(llvm::errs());
        llvm::errs() << "\n";
    }
    return ds->builder->CreateExtractValue(val, idx);
}
void llds_set_insert_point(LLVMDataStructures *ds, llvm::BasicBlock* bb) {
    ds->builder->SetInsertPoint(bb);
}
llvm::Value * llds_create_alloca_sized(LLVMDataStructures *ds, llvm::Type * ty) {
    return ds->builder->CreateAlloca(ty);
}
llvm::Value * llds_create_alloca_unsized(LLVMDataStructures *ds, llvm::Value* size) {
    auto * alloca = ds->builder->CreateAlloca(ds->builder->getInt8Ty(), size);
    llvm::Value * zero = llvm::ConstantInt::get(ds->builder->getInt8Ty(), 0);
    ds->builder->CreateMemSet(alloca, zero, size, llvm::MaybeAlign(8));
    return alloca; // 默认清零
}
void llds_create_store(LLVMDataStructures *ds, llvm::Value* val, llvm::Value* dst) {
    ds->builder->CreateStore(val, dst);
}
llvm::Value * llds_create_load(LLVMDataStructures *ds, llvm::Type* ty, llvm::Value* ptr) {
    return ds->builder->CreateLoad(ty, ptr);
}
void llds_create_memcpy(LLVMDataStructures *ds, llvm::Value* dst, llvm::Value* src, llvm::Value* size) {
    llvm::Align align(1);
    ds->builder->CreateMemCpy(dst, align, src, align, size);
}
void llds_create_memset(LLVMDataStructures *ds, llvm::Value* dst, llvm::Value* val, llvm::Value* count) {
    llvm::Align align(1);
    ds->builder->CreateMemSet(dst, val, count, align);
}
void llds_create_br(LLVMDataStructures *ds, llvm::BasicBlock * bb) {
    ds->builder->CreateBr(bb);
}
void llds_create_cond_br(LLVMDataStructures *ds, llvm::Value * cond, llvm::BasicBlock * bb1, llvm::BasicBlock * bb2) {
    ds->builder->CreateCondBr(cond, bb1, bb2);
}
void llds_create_ret(LLVMDataStructures *ds, llvm::Value * v) {
    ds->builder->CreateRet(v);
}
void llds_create_retvoid(LLVMDataStructures *ds) {
    ds->builder->CreateRetVoid();
}
void llds_create_unreachable(LLVMDataStructures *ds) {
    ds->builder->CreateUnreachable();
}
llvm::Value * llds_create_call(LLVMDataStructures *ds, llvm::Function* callee, llvm::Value** arg_ptr, size_t len) {
    std::vector<llvm::Value*> args{arg_ptr, arg_ptr + len};

    // only for debug
    if (callee->getFunctionType()->getNumParams() != args.size()) {
        llvm::errs() << "[Debug] Calling function " << callee->getName()
                    << " param count " << callee->getFunctionType()->getNumParams()
                    << " does not equal to arg count " << args.size() << "\n";
        abort();
    }
    for(int i = 0; i < args.size(); i++) {
        assert(args[i] != nullptr);
        if (callee->getFunctionType()->getParamType(i) != args[i]->getType()) {
            llvm::errs() << "[Debug] Calling function argument type mismatch " << callee->getName() << "\n    ";
            callee->getFunctionType()->getParamType(i)->print(llvm::errs());
            llvm::errs() << "\n    ";
            args[i]->getType()->print(llvm::errs());
            llvm::errs() << "\n";
        }
    }
    return ds->builder->CreateCall(callee, args);
}
llvm::Value * llds_create_call_with_fnty(LLVMDataStructures *ds, llvm::FunctionType * fnty, llvm::Function* f, llvm::Value** arg_ptr, size_t len) {
    if (fnty->getNumParams() != len) {
        llvm::errs() << "[Debug] Calling function " << f->getName()
                    << " param count " << fnty->getNumParams()
                    << " does not equal to arg count " << len << "\n";
        abort();
    }
    std::vector<llvm::Value*> args{arg_ptr, arg_ptr + len};
    for(int i = 0; i < len; i++) {
        if (fnty->getParamType(i) != args[i]->getType()) {
            llvm::errs() << "[Debug] Calling function argument type mismatch " << f->getName() << "\n    ";
            fnty->getParamType(i)->print(llvm::errs());
            llvm::errs() << "\n    ";
            args[i]->getType()->print(llvm::errs());
            llvm::errs() << "\n";
        }
    }

    return ds->builder->CreateCall(fnty, f, args);
}
llvm::Value * llds_create_fat_ptr(LLVMDataStructures *ds, llvm::Value* p1, llvm::Value* p2, llvm::Value* p3) {
    llvm::Value * fat_ptr = llvm::UndefValue::get(ds->zn_fat_ptr);
    fat_ptr = ds->builder->CreateInsertValue(fat_ptr, p1, {0});
    fat_ptr = ds->builder->CreateInsertValue(fat_ptr, p2, {1});
    fat_ptr = ds->builder->CreateInsertValue(fat_ptr, p3, {2});
    return fat_ptr;
}

llvm::Value * llds_create_ptr_to_int(LLVMDataStructures *ds, llvm::Value* ptr) {
    return ds->builder->CreatePtrToInt(ptr, ds->builder->getInt64Ty());
}
llvm::Value * llds_create_int_to_ptr(LLVMDataStructures *ds, llvm::Value* i) {
    return ds->builder->CreateIntToPtr(i, ds->builder->getPtrTy());
}
llvm::Value * llds_create_zext(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty) {
    return ds->builder->CreateZExt(val, ty);
}
llvm::Value * llds_create_sext_or_trunc(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty) {
    return ds->builder->CreateSExtOrTrunc(val, ty);
}
llvm::Value * llds_create_zext_or_trunc(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty) {
    return ds->builder->CreateZExtOrTrunc(val, ty);
}
llvm::Value * llds_create_si_to_fp(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty) {
    return ds->builder->CreateSIToFP(val, ty);
}
llvm::Value * llds_create_ui_to_fp(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty) {
    return ds->builder->CreateUIToFP(val, ty);
}
llvm::Value * llds_create_fp_to_si(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty) {
    return ds->builder->CreateFPToSI(val, ty);
}
llvm::Value * llds_create_fp_to_ui(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty) {
    return ds->builder->CreateFPToUI(val, ty);
}
llvm::Value * llds_create_fp_trunc(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty) {
    return ds->builder->CreateFPTrunc(val, ty);
}
llvm::Value * llds_create_fp_ext(LLVMDataStructures *ds, llvm::Value* val, llvm::Type * ty) {
    return ds->builder->CreateFPExt(val, ty);
}

llvm::Value * llds_create_add(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateAdd(lhs, rhs);
}
llvm::Value * llds_create_sub(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateSub(lhs, rhs);
}
llvm::Value * llds_create_mul(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateMul(lhs, rhs);
}
llvm::Value * llds_create_sdiv(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateSDiv(lhs, rhs);
}
llvm::Value * llds_create_udiv(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateUDiv(lhs, rhs);
}
llvm::Value * llds_create_srem(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateSRem(lhs, rhs);
}
llvm::Value * llds_create_urem(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateURem(lhs, rhs);
}
llvm::Value * llds_create_fadd(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateFAdd(lhs, rhs);
}
llvm::Value * llds_create_fsub(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateFSub(lhs, rhs);
}
llvm::Value * llds_create_fmul(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateFMul(lhs, rhs);
}
llvm::Value * llds_create_fdiv(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateFDiv(lhs, rhs);
}
llvm::Value * llds_create_frem(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateFRem(lhs, rhs);
}
llvm::Value * llds_create_neg(LLVMDataStructures *ds, llvm::Value* val) {
    return ds->builder->CreateNeg(val);
}
llvm::Value * llds_create_fneg(LLVMDataStructures *ds, llvm::Value* val) {
    return ds->builder->CreateFNeg(val);
}
llvm::Value * llds_create_logic_and(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateLogicalAnd(lhs, rhs);
}
llvm::Value * llds_create_logic_or(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateLogicalOr(lhs, rhs);
}
llvm::Value * llds_create_bit_and(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateAnd(lhs, rhs);
}
llvm::Value * llds_create_bit_or(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateOr(lhs, rhs);
}
llvm::Value * llds_create_bit_xor(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateXor(lhs, rhs);
}
llvm::Value * llds_create_bit_shl(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateShl(lhs, rhs);
}
llvm::Value * llds_create_bit_lshr(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateLShr(lhs, rhs);
}
llvm::Value * llds_create_bit_ashr(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateAShr(lhs, rhs);
}
llvm::Value * llds_create_bit_not(LLVMDataStructures *ds, llvm::Value* val) {
    return ds->builder->CreateNot(val);
}
llvm::Value * llds_create_bit_reverse(LLVMDataStructures *ds, llvm::Value* arg) {
    auto *f = llvm::Intrinsic::getOrInsertDeclaration(ds->module, llvm::Intrinsic::bitreverse, 
                    {arg->getType()});
    return ds->builder->CreateCall(f, {arg});
}
llvm::Value * llds_create_bit_rotate_left(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    auto *f = llvm::Intrinsic::getOrInsertDeclaration(ds->module, llvm::Intrinsic::fshl, 
                    {lhs->getType()});
    return ds->builder->CreateCall(f, {lhs, lhs, rhs});
}
llvm::Value * llds_create_bit_rotate_right(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    auto *f = llvm::Intrinsic::getOrInsertDeclaration(ds->module, llvm::Intrinsic::fshr, 
                    {lhs->getType()});
    return ds->builder->CreateCall(f, {lhs, lhs, rhs});
}
llvm::Value * llds_create_icmp_eq(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateICmpEQ(lhs, rhs);
}
llvm::Value * llds_create_icmp_ne(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateICmpNE(lhs, rhs);
}
llvm::Value * llds_create_icmp_uge(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateICmpUGE(lhs, rhs);
}
llvm::Value * llds_create_icmp_ugt(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateICmpUGT(lhs, rhs);
}
llvm::Value * llds_create_icmp_ule(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateICmpULE(lhs, rhs);
}
llvm::Value * llds_create_icmp_ult(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateICmpULT(lhs, rhs);
}
llvm::Value * llds_create_icmp_sge(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateICmpSGE(lhs, rhs);
}
llvm::Value * llds_create_icmp_sgt(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateICmpSGT(lhs, rhs);
}
llvm::Value * llds_create_icmp_sle(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateICmpSLE(lhs, rhs);
}
llvm::Value * llds_create_icmp_slt(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateICmpSLT(lhs, rhs);
}
llvm::Value * llds_create_fcmp_oeq(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateFCmpOEQ(lhs, rhs);
}
llvm::Value * llds_create_fcmp_one(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateFCmpONE(lhs, rhs);
}
llvm::Value * llds_create_fcmp_ogt(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateFCmpOGT(lhs, rhs);
}
llvm::Value * llds_create_fcmp_oge(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateFCmpOGE(lhs, rhs);
}
llvm::Value * llds_create_fcmp_olt(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateFCmpOLT(lhs, rhs);
}
llvm::Value * llds_create_fcmp_ole(LLVMDataStructures *ds, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateFCmpOLE(lhs, rhs);
}

llvm::Value * llds_create_select(LLVMDataStructures *ds, llvm::Value * cond, llvm::Value* lhs, llvm::Value* rhs) {
    return ds->builder->CreateSelect(cond, lhs, rhs);
}
llvm::Value * llds_create_phi(LLVMDataStructures *ds, llvm::Type * ty, size_t num) {
    return ds->builder->CreatePHI(ty, num);
}
void llds_phi_add_incoming(llvm::PHINode * phi, llvm::Value* val, llvm::BasicBlock * bb) {
    phi->addIncoming(val, bb);
}
llvm::Value* llds_create_insert_value(LLVMDataStructures *ds, llvm::Value* agg, llvm::Value * val, size_t idx) {
    return ds->builder->CreateInsertValue(agg, val, {(unsigned int)idx});
}

llvm::DIFile * llds_create_di_file(LLVMDataStructures *ds, const char * f, size_t len) {
    std::string file_name{f, len};

    static std::map<std::string, llvm::DIFile*> cache;
    if (cache.contains(file_name)) {
        return cache[file_name];
    }

    std::filesystem::path p(file_name);
    auto name = p.filename().string();
    auto dir = p.parent_path().string();
    llvm::DIFile * dif = ds->di_builder->createFile(name, dir);
    cache[file_name] = dif;
    return dif;
}

llvm::DISubprogram * llds_create_di_subprogram(
    LLVMDataStructures *ds,
    llvm::Function * f,
    const char * short_name,
    llvm::DIFile * file,
    unsigned int line) {

    std::string sname = short_name;

    // 尚未实现, 统一设置为空类型
    std::vector<llvm::Metadata*> types;
    auto params = ds->di_builder->getOrCreateTypeArray(types);
    llvm::DISubroutineType *func_di_ty = ds->di_builder->createSubroutineType(params);

    llvm::DISubprogram *sp = ds->di_builder->createFunction(
        ds->di_compile_unit,          // scope
        sname,                        // name
        f->getName(),                 // linkage name
        file,                         // file
        line,                         // line
        func_di_ty,                   // type
        line,                         // scope line
        llvm::DINode::FlagZero,       // flags
        llvm::DISubprogram::SPFlagDefinition
    );
    f->setSubprogram(sp);
    return sp;
}

llvm::DILocation* llds_create_di_location(
        LLVMDataStructures *ds,
        llvm::Function* func,
        unsigned int line,
        unsigned int column
    ) {

    auto * func_di = func->getSubprogram();
    if (func_di == nullptr) {
        assert(false);
    }

    auto debug_loc = llvm::DILocation::get(*ds->ctx, line, column, func_di);
    return debug_loc;
}

void llds_set_di_loc(LLVMDataStructures *ds, llvm::DILocation* di_loc) {
    ds->builder->SetCurrentDebugLocation(di_loc);
}
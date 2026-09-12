#pragma once

#include <cstdint>
#include <stddef.h>

#include "llvm/IR/Value.h"
#include "llvm/IR/Type.h"

#include "llvm_data_structures.h"

#ifdef __cplusplus
extern "C" {
#endif

llvm::GlobalVariable* codegen_builtin_typemeta(LLVMDataStructures* ds, char *name, size_t len, size_t byte_size);
llvm::GlobalVariable* codegen_tuple_generictypemeta(LLVMDataStructures* llds);
llvm::GlobalVariable* codegen_array_generictypemeta(LLVMDataStructures* llds);

llvm::Function* empty_copy_fn(LLVMDataStructures* llds);
llvm::Function* empty_dtor_fn(LLVMDataStructures* llds);

llvm::Function * codegen_tuple_array_sizefn(LLVMDataStructures* llds);
llvm::Function * codegen_tuple_array_alignfn(LLVMDataStructures* llds);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif

#pragma once

#include "llvm_data_structures.h"

#ifdef __cplusplus
extern "C" {
#endif

void codegen_set_out_dir(LLVMDataStructures * llds, char * dir, size_t len);

bool codegen_write_bc_file(LLVMDataStructures * llds);
bool codegen_write_obj_file(LLVMDataStructures * llds, size_t opt_level);
bool codegen_write_exe_file(LLVMDataStructures * llds, bool link_cpp, char* link, size_t link_len);

#ifdef __cplusplus
} // 结束 extern "C" 块
#endif

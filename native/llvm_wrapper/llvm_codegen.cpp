#include "llvm_codegen.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Verifier.h"

#include <string>
#include <iostream>
#include <filesystem>
#include <unistd.h>
#include <limits.h>

void codegen_set_out_dir(LLVMDataStructures * llds, char * dir, size_t len) {
    llds->out_dir = std::string{dir, len};
}

bool codegen_write_bc_file(LLVMDataStructures * llds) {
    std::string comp_name = llds->module->getName().str();
    auto out_dir = std::filesystem::path(llds->out_dir);

    auto file_name = out_dir / (comp_name + ".bc");

    std::error_code EC;
    llvm::raw_fd_ostream dest(file_name.string(), EC, llvm::sys::fs::OF_None);

    if (EC) {
        llvm::errs() << "Could not open file: " << EC.message();
        return false;
    }
    llds->module->addModuleFlag(llvm::Module::Warning, "Dwarf Version", llvm::dwarf::DWARF_VERSION);
    llds->module->addModuleFlag(llvm::Module::Error, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);

    if (llvm::verifyModule(*llds->module, &llvm::errs())) {
        std::cerr << "[Compiler Internal Error] 模块验证失败" << std::endl;
        return false;
    }

    llvm::WriteBitcodeToFile(*llds->module, dest);

    llds->di_builder->finalize();
    dest.flush();

    llvm::outs() << "[CMD] write bitcode file at: " << file_name << "\n";
    return true;
}

static std::filesystem::path get_zinc_compiler_path() {
    char buf[PATH_MAX] = {0};
    ssize_t len = ::readlink("/proc/self/exe", buf, sizeof(buf) - 1);
    if (len <= 0) return {};
    buf[len] = '\0';
    return std::filesystem::canonical(std::string(buf, len));
}

bool codegen_write_obj_file(LLVMDataStructures * llds, size_t opt_level) {
    std::string comp_name = llds->module->getName().str();
    auto out_dir = std::filesystem::path(llds->out_dir);
    auto input = out_dir / (comp_name + ".bc");

    // 优化
    {
        auto zinc_root = get_zinc_compiler_path().parent_path();
        auto opt_path = zinc_root / "../llvm/bin/opt";

        std::string cmd = opt_path.string();
        cmd += " -passes='default<O";
        cmd += std::to_string(opt_level);
        cmd += ">";
        cmd += "' ";
        cmd += input.string();
        cmd += " -o ";
        cmd += input.string();
        std::cout << "[CMD] opt command: " << cmd << std::endl;
        int r = system(cmd.c_str());
        if (r != 0) {
            return false;
        }
    }

    // 生成 object file
    auto output = out_dir / (comp_name + ".o");
    {
        auto zinc_root = get_zinc_compiler_path().parent_path();
        auto llc_path = zinc_root / "../llvm/bin/llc";
        std::string cmd = llc_path.string();
        cmd += " -filetype=obj --relocation-model=pic ";
        cmd += input.string();
        cmd += " -o ";
        cmd += output.string();
        std::cout << "[CMD] llc command: " << cmd << std::endl;
        int r = system(cmd.c_str());
        if (r != 0) {
            return false;
        }
    }
    return true;
}

bool codegen_write_exe_file(LLVMDataStructures * llds, bool link_cpp, char* extra_link, size_t extra_link_len) {
    std::string comp_name = llds->module->getName().str();
    auto out_dir = std::filesystem::path(llds->out_dir);
    auto input = out_dir / (comp_name + ".o");

    std::string exe_name = llds->entry_source_file;
    if (exe_name.ends_with(".zn")) {
        exe_name = exe_name.substr(0, exe_name.size() - 3);
    }
    auto output = out_dir / exe_name;

    std::string cmd = link_cpp ? "clang++ -fPIE -pie -g -rdynamic " : "clang -fPIE -pie -g -rdynamic ";

    cmd += " -o ";
    cmd += output.string();
    cmd += "  ";

    cmd += input.string();

    std::string extra_link_str{extra_link, extra_link_len};
    if (!extra_link_str.empty()) {
        cmd += " ";
        cmd += extra_link_str;
        cmd += " ";
    }

    // 需要默认链接 libstd.a
    auto zinc_root = get_zinc_compiler_path().parent_path();
    auto std_lib = zinc_root / "../lib/libstd.a";
    cmd += " ";
    cmd += std_lib.string();

    std::cout << "[CMD] linker command: " << cmd << "\n" << std::endl;
    return system(cmd.c_str());
}

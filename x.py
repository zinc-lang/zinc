#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#

import os
import sys
import subprocess
import shutil

REPO_DIR = os.path.dirname(os.path.abspath(__file__))
BUILD_DIR = os.path.join(REPO_DIR, "build")
CMAKE_BUILD_DIR = os.path.join(BUILD_DIR, "build")
CMAKE_OUTPUT_DIR = os.path.join(REPO_DIR, "out")
LLVM_CMAKE_OUTPUT_DIR = os.path.join(CMAKE_OUTPUT_DIR, "llvm") # out/llvm

# 添加 LLVM 依赖项 
# (如果 wsl 下有网络问题, 建议在 windows 下执行, 但是要手动把 third_party/llvm-project/llvm/cmake/config.guess 的 CRLF 替换为 LF)
# git submodule add https://github.com/llvm/llvm-project.git third_party/llvm-project
# git submodule update --init --recursive


# 这个列表是通过观察 c++ 版本的链接过程打印出来的, 再靠 AI 排序, 它们对链接顺序敏感
# todo: 真的需要这么多的 lib 吗, 能不能剔除一部分?
LLVM_LIBS = [
    './out/llvm/lib/libLLVMX86CodeGen.a',
    './out/llvm/lib/libLLVMX86AsmParser.a',
    './out/llvm/lib/libLLVMX86Desc.a',
    './out/llvm/lib/libLLVMX86Disassembler.a',
    './out/llvm/lib/libLLVMX86Info.a', 
    './out/llvm/lib/libLLVMGlobalISel.a', 
    './out/llvm/lib/libLLVMSelectionDAG.a', 
    './out/llvm/lib/libLLVMAsmPrinter.a', 
    './out/llvm/lib/libLLVMCFGuard.a', 
    './out/llvm/lib/libLLVMCGData.a', 
    './out/llvm/lib/libLLVMTarget.a', 
    './out/llvm/lib/libLLVMCodeGenTypes.a', 
    './out/llvm/lib/libLLVMCodeGen.a', 
    './out/llvm/lib/libLLVMIRPrinter.a', 
    './out/llvm/lib/libLLVMObjCARCOpts.a', 
    './out/llvm/lib/libLLVMScalarOpts.a', 
    './out/llvm/lib/libLLVMAggressiveInstCombine.a', 
    './out/llvm/lib/libLLVMInstCombine.a', 
    './out/llvm/lib/libLLVMInstrumentation.a', 
    './out/llvm/lib/libLLVMTransformUtils.a', 
    './out/llvm/lib/libLLVMAnalysis.a', 
    './out/llvm/lib/libLLVMProfileData.a', 
    './out/llvm/lib/libLLVMDebugInfoDWARF.a',
    './out/llvm/lib/libLLVMDebugInfoDWARFLowLevel.a',
    './out/llvm/lib/libLLVMDebugInfoPDB.a', 
    './out/llvm/lib/libLLVMDebugInfoCodeView.a', 
    './out/llvm/lib/libLLVMDebugInfoMSF.a', 
    './out/llvm/lib/libLLVMDebugInfoBTF.a', 
    './out/llvm/lib/libLLVMSymbolize.a', 
    './out/llvm/lib/libLLVMObject.a', 
    './out/llvm/lib/libLLVMBitWriter.a', 
    './out/llvm/lib/libLLVMBitReader.a', 
    './out/llvm/lib/libLLVMRemarks.a', 
    './out/llvm/lib/libLLVMBitstreamReader.a', 
    './out/llvm/lib/libLLVMTextAPI.a', 
    './out/llvm/lib/libLLVMIRReader.a', 
    './out/llvm/lib/libLLVMAsmParser.a', 
    './out/llvm/lib/libLLVMCore.a', 
    './out/llvm/lib/libLLVMSupport.a', 
    './out/llvm/lib/libLLVMDemangle.a', 
    './out/llvm/lib/libLLVMMCParser.a', 
    './out/llvm/lib/libLLVMMCDisassembler.a', 
    './out/llvm/lib/libLLVMMC.a', 
    './out/llvm/lib/libLLVMBinaryFormat.a', 
    './out/llvm/lib/libLLVMTargetParser.a'
    ]

def build_llvm():
    if not os.path.exists(LLVM_CMAKE_OUTPUT_DIR):
        shutil.rmtree('./build-llvm', ignore_errors=True)
        os.makedirs('./build-llvm')
        os.chdir("./build-llvm")
        subprocess.run('cmake -DCMAKE_BUILD_TYPE=Debug -DLLVM_TARGETS_TO_BUILD=host ../third_party/llvm-project/llvm', shell=True,check=True,text=True)
        subprocess.run('cmake --build . --config Debug --parallel 4', shell=True,check=True,text=True)
        subprocess.run(f'cmake -DCMAKE_INSTALL_PREFIX={LLVM_CMAKE_OUTPUT_DIR} -P cmake_install.cmake', shell=True,check=True,text=True)
    else:
        print(f'LLVM has already been built in {LLVM_CMAKE_OUTPUT_DIR}, if you want to re-build, please delete this directory')

def build_std(folder):
    shutil.rmtree('./build-std/', ignore_errors=True)
    os.makedirs('./build-std/', exist_ok = True)
    os.makedirs('./build-std/objects', exist_ok = True)

    subprocess.run('cmake -S ./library/zinc_core -B build-std -DCMAKE_BUILD_TYPE=Debug -G Ninja', shell=True,check=True,text=True)
    subprocess.run('cmake --build build-std --config Debug', shell=True,check=True,text=True)
    subprocess.run('cd ./build-std/objects && ar -x ../libzinc_core.a', shell=True,check=True,text=True)

    if not os.path.exists(f"{folder}/bin/zinc"):
        return

    print(f"execute: {folder}/bin/zinc -O0 ./library/zinc_std/lib.zn --out-dir=./build-std/")
    subprocess.run(f'{folder}/bin/zinc -O0 ./library/zinc_std/lib.zn --out-dir=./build-std/', shell=True,check=True,text=True)
    # 链接 zinc_core
    shutil.rmtree(f'{folder}/lib', ignore_errors=True)
    os.makedirs(f'{folder}/lib', exist_ok = True)
    shutil.copyfile("./build-std/std.bc", f"{folder}/lib/std.bc")

    subprocess.run(f'ar -rcs {folder}/lib/libstd.a  ./build-std/objects/*.o  ./build-std/std.o', shell=True,check=True,text=True)
    # 拷贝 std.zno 文件到 ./out/stage/lib
    shutil.copyfile("./build-std/std.zno", f"{folder}/lib/std.zno")

def build(stage, check_only):
    os.chdir(REPO_DIR)
    if stage != 1 and stage != 2 and stage != 3:
        print('stage can only support 1, 2 and 3')
        os._exit(1)

    if stage == 1:
        out_dir = './out/stage1'
        compiler = './out/stage0/bin/zinc'
    if stage == 2:
        out_dir = './out/stage2'
        compiler = './out/stage1/bin/zinc'
    if stage == 3:
        out_dir = "./out/stage3"
        compiler = './out/stage2/bin/zinc'

    if check_only:
        cmd = f'{compiler} -O0 ./compiler/zinc.zn --out-dir=./out/bin --check-only '
        print(f"run: {cmd}")
        subprocess.run(cmd, check=True, shell=True, text=True)
    else:
        subprocess.run('cmake -S ./native/sqlite_wrapper -B ./build_sqlite_wrapper -DCMAKE_BUILD_TYPE=Debug -G Ninja', shell=True,check=True,text=True)
        subprocess.run('cmake --build ./build_sqlite_wrapper --config Debug', shell=True,check=True,text=True)

        subprocess.run('cmake -S ./native/llvm_wrapper -B ./build_llvm_wrapper -DCMAKE_BUILD_TYPE=Debug -G Ninja', shell=True,check=True,text=True)
        subprocess.run('cmake --build ./build_llvm_wrapper --config Debug', shell=True,check=True,text=True)

        os.makedirs(f'{out_dir}/bin', exist_ok = True)

        cmd = f'{compiler} -O0 ./compiler/zinc.zn --out-dir={out_dir}/bin --link-c++ -L ./build_sqlite_wrapper -L ./build_llvm_wrapper -L ./out/llvm/lib '
        cmd += ' -l libsqlite_wrapper.a -l libllvm_wrapper.a '
        # 链接 llvm_libs
        link_llvm = " --extra-link='"
        link_llvm += "-Wl,--start-group "
        link_llvm += ' '.join(LLVM_LIBS)
        link_llvm += " -Wl,--end-group'"
        cmd += link_llvm

        cmd += " --extra-link='-lzstd -lz -lpthread -lm -ldl' "

        print(f"run: {cmd}")
        subprocess.run(cmd, check=True, shell=True, text=True)

        # 在 stage1 目录下生成一个软链接指向上层的 llvm 目录
        if os.path.exists(f'{out_dir}/llvm'):
            os.remove(f'{out_dir}/llvm')
        os.symlink(LLVM_CMAKE_OUTPUT_DIR, f'{out_dir}/llvm', target_is_directory=True)

        # 后面用这个编译器把标准库编译一遍
        build_std(out_dir)

if __name__ == "__main__":

    if sys.argv[1] == "check":
        build(1, True)

    if sys.argv[1] == "build" or sys.argv[1] == "build1":
        build(1, False)

    if sys.argv[1] == "build2":
        if not os.path.exists('./out/stage1/bin'):
            build(1, False)
        build(2, False)

    if sys.argv[1] == "build3":
        if not os.path.exists('./out/stage1/bin'):
            build(1, False)
        if not os.path.exists('./out/stage2/bin'):
            build(2, False)
        build(3, False)
    
    if sys.argv[1] == "build-llvm":
        build_llvm()

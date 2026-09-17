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

STAGE0_URL = os.environ.get(
    "ZINC_STAGE0_URL",
    "https://github.com/zinc-lang/zinc/releases/download/v0.0.1/stage0.zip",
)

# Zinc's codegen shells out to these binaries at out/llvm/bin/{opt,llc}.
LLVM_REQUIRED_TOOLS = ("opt", "llc")

def llvm_tool_path(name):
    return os.path.join(LLVM_CMAKE_OUTPUT_DIR, "bin", name)

def llvm_has_required_tools():
    return all(os.path.exists(llvm_tool_path(name)) for name in LLVM_REQUIRED_TOOLS)

def require_llvm_tools():
    missing = [name for name in LLVM_REQUIRED_TOOLS if not os.path.exists(llvm_tool_path(name))]
    if missing:
        raise SystemExit(
            "LLVM install is missing " + ", ".join(llvm_tool_path(n) for n in missing) + ". "
            "The compiler invokes these to emit object files. Rebuild with `python x.py build-llvm` "
            "(delete out/llvm first if an old install is incomplete)."
        )

def link_stage_llvm(stage_dir):
    if not os.path.isdir(stage_dir):
        return
    link = os.path.join(stage_dir, "llvm")
    if os.path.islink(link) or os.path.exists(link):
        if os.path.islink(link):
            os.remove(link)
        else:
            return
    if os.path.exists(LLVM_CMAKE_OUTPUT_DIR):
        os.symlink(LLVM_CMAKE_OUTPUT_DIR, link, target_is_directory=True)

def setup_stage0():
    os.chdir(REPO_DIR)
    stage0 = os.path.join(CMAKE_OUTPUT_DIR, "stage0")
    zinc_bin = os.path.join(stage0, "bin", "zinc")
    if os.path.exists(zinc_bin):
        print(f"stage0 already present at {stage0}")
        link_stage_llvm(stage0)
        return

    os.makedirs(CMAKE_OUTPUT_DIR, exist_ok=True)
    zip_path = os.path.join(CMAKE_OUTPUT_DIR, "stage0.zip")
    if not os.path.exists(zip_path):
        print(f"download: {STAGE0_URL}")
        subprocess.run(["curl", "-fL", "-o", zip_path, STAGE0_URL], check=True)

    tmp = os.path.join(CMAKE_OUTPUT_DIR, "stage0-unpack")
    shutil.rmtree(tmp, ignore_errors=True)
    os.makedirs(tmp)
    print(f"unpack {zip_path}")
    if shutil.which("unzip"):
        subprocess.run(["unzip", "-q", zip_path, "-d", tmp], check=True)
    else:
        import zipfile
        with zipfile.ZipFile(zip_path) as zf:
            zf.extractall(tmp)

    if os.path.isdir(os.path.join(tmp, "stage0")):
        shutil.move(os.path.join(tmp, "stage0"), stage0)
    elif os.path.isdir(os.path.join(tmp, "stage1")):
        # v0.0.1 ships the compiler under stage1/
        shutil.move(os.path.join(tmp, "stage1"), stage0)
    elif os.path.isdir(os.path.join(tmp, "bin")):
        shutil.move(tmp, stage0)
        tmp = None
    else:
        raise SystemExit(f"unrecognized stage0 zip layout in {tmp}")

    if tmp is not None:
        shutil.rmtree(tmp, ignore_errors=True)
    os.chmod(os.path.join(stage0, "bin", "zinc"), 0o755)
    link_stage_llvm(stage0)
    print(f"stage0 ready at {stage0}")

def build_llvm():
    os.chdir(REPO_DIR)
    if os.path.exists(LLVM_CMAKE_OUTPUT_DIR) and llvm_has_required_tools():
        print(f'LLVM has already been built in {LLVM_CMAKE_OUTPUT_DIR}, if you want to re-build, please delete this directory')
        link_stage_llvm(os.path.join(CMAKE_OUTPUT_DIR, "stage0"))
        return
    if os.path.exists(LLVM_CMAKE_OUTPUT_DIR):
        print(f'LLVM at {LLVM_CMAKE_OUTPUT_DIR} is missing opt/llc; rebuilding')
        shutil.rmtree(LLVM_CMAKE_OUTPUT_DIR)

    llvm_src = os.path.join(REPO_DIR, "third_party", "llvm-project", "llvm")
    if not os.path.exists(os.path.join(llvm_src, "CMakeLists.txt")):
        raise SystemExit(
            "LLVM sources missing. Fetch the pinned submodule, e.g.\n"
            "  git fetch --depth 1 origin $(git ls-tree HEAD third_party/llvm-project | awk '{print $3}')"
        )

    build_type = os.environ.get("ZINC_LLVM_BUILD_TYPE", "Debug")
    jobs = os.environ.get("ZINC_LLVM_JOBS", str(os.cpu_count() or 4))
    shutil.rmtree('./build-llvm', ignore_errors=True)
    os.makedirs('./build-llvm')
    cmake = [
        "cmake",
        f"-DCMAKE_BUILD_TYPE={build_type}",
        f"-DCMAKE_INSTALL_PREFIX={LLVM_CMAKE_OUTPUT_DIR}",
        "-DLLVM_TARGETS_TO_BUILD=host",
        "-DLLVM_INCLUDE_TESTS=OFF",
        "-DLLVM_INCLUDE_EXAMPLES=OFF",
        "-DLLVM_INCLUDE_BENCHMARKS=OFF",
        "-DLLVM_INCLUDE_DOCS=OFF",
        "-DLLVM_ENABLE_BINDINGS=OFF",
        "-DLLVM_ENABLE_OCAMLDOC=OFF",
        # Keep the default `all` target to libraries; Zinc only needs opt and llc.
        "-DLLVM_BUILD_TOOLS=OFF",
        "-DLLVM_ENABLE_ZLIB=ON",
        "-DLLVM_ENABLE_ZSTD=ON",
        "-DLLVM_ENABLE_TERMINFO=OFF",
        "-DLLVM_ENABLE_LIBXML2=OFF",
        "-DLLVM_PARALLEL_LINK_JOBS=1",
        llvm_src,
    ]
    if shutil.which("ninja"):
        cmake[1:1] = ["-G", "Ninja"]
    try:
        os.chdir("./build-llvm")
        subprocess.run(cmake, check=True, text=True)
        subprocess.run(["cmake", "--build", ".", "--config", build_type, "--parallel", jobs], check=True, text=True)
        tool_build = ["cmake", "--build", ".", "--config", build_type, "--parallel", jobs]
        for name in LLVM_REQUIRED_TOOLS:
            tool_build.extend(["--target", name])
        subprocess.run(tool_build, check=True, text=True)
        subprocess.run(["cmake", "--install", ".", "--prefix", LLVM_CMAKE_OUTPUT_DIR], check=True, text=True)
        # LLVM_BUILD_TOOLS=OFF still generates opt/llc targets, but skips their install rules.
        dest_bin = os.path.join(LLVM_CMAKE_OUTPUT_DIR, "bin")
        os.makedirs(dest_bin, exist_ok=True)
        for name in LLVM_REQUIRED_TOOLS:
            built = os.path.join("bin", name)
            if not os.path.isfile(built):
                raise SystemExit(f"LLVM build did not produce {os.path.abspath(built)}")
            shutil.copy2(built, os.path.join(dest_bin, name))
            os.chmod(os.path.join(dest_bin, name), 0o755)
    finally:
        os.chdir(REPO_DIR)
    require_llvm_tools()
    link_stage_llvm(os.path.join(CMAKE_OUTPUT_DIR, "stage0"))

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
        sys.exit(1)

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

        require_llvm_tools()
        if not shutil.which("clang++"):
            raise SystemExit("clang++ is required to link Zinc (install the clang package)")
        print(f"run: {cmd}")
        subprocess.run(cmd, check=True, shell=True, text=True)

        zinc_out = os.path.join(out_dir, "bin", "zinc")
        if not os.path.exists(zinc_out):
            raise SystemExit(
                f"build did not produce {zinc_out}. "
                "stage0 needs working LLVM tools at out/llvm/bin/opt and out/llvm/bin/llc "
                "(symlinked as out/stage0/llvm)."
            )

        # 在 stage1 目录下生成一个软链接指向上层的 llvm 目录
        if os.path.exists(f'{out_dir}/llvm'):
            os.remove(f'{out_dir}/llvm')
        os.symlink(LLVM_CMAKE_OUTPUT_DIR, f'{out_dir}/llvm', target_is_directory=True)

        # 后面用这个编译器把标准库编译一遍
        build_std(out_dir)


def run_tests():
    # compile-pass cases use new lexer/std APIs; they need the stage1 compiler
    # produced by `python x.py build`, not the bootstrap stage0.
    compiler = "./out/stage1/bin/zinc"
    if not os.path.exists(compiler):
        print("python x.py test requires ./out/stage1/bin/zinc. Run `python x.py build` first.")
        sys.exit(1)

    pass_dir = os.path.join(REPO_DIR, "tests", "compile-pass")
    fail_dir = os.path.join(REPO_DIR, "tests", "compile-fail")
    failed = 0
    ran = 0

    def zinc_check(path):
        cmd = [compiler, "-O0", path, "--check-only"]
        print("run:", " ".join(cmd))
        # 编译器的 stdout/stderr 全部丢弃, 测试结果只通过退出码判断
        return subprocess.run(cmd, text=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    if os.path.isdir(pass_dir):
        for name in sorted(os.listdir(pass_dir)):
            if not name.endswith(".zn"):
                continue
            ran += 1
            result = zinc_check(os.path.join(pass_dir, name))
            if result.returncode != 0:
                print(f"FAIL compile-pass {name}")
                failed += 1
            else:
                print(f"ok   compile-pass {name}")

    if os.path.isdir(fail_dir):
        for name in sorted(os.listdir(fail_dir)):
            if not name.endswith(".zn"):
                continue
            ran += 1
            result = zinc_check(os.path.join(fail_dir, name))
            if result.returncode == 0:
                print(f"FAIL compile-fail {name} (compiler accepted it)")
                failed += 1
            else:
                print(f"ok   compile-fail {name}")

    print(f"{ran - failed}/{ran} tests passed")
    if failed:
        sys.exit(1)

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
    
    if sys.argv[1] == "setup-stage0":
        setup_stage0()

    if sys.argv[1] == "build-llvm":
        build_llvm()

    if sys.argv[1] == "test":
        run_tests()

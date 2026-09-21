#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Zinc 测试执行器。

通常由 `python x.py test` 调用, 也可以单独运行: `python tests/run_tests.py`。

| 目录 | 期望 |
|------|------|
| tests/compile-pass | `zinc --check-only` 成功 |
| tests/compile-fail | `zinc --check-only` 失败 |
| tests/run-pass       | 编译成可执行文件并运行, 正常退出 (没有 panic) |
"""

import os
import sys
import subprocess
import shutil

TESTS_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_DIR = os.path.dirname(TESTS_DIR)

# run-pass 用例单个程序的执行超时 (秒), 防止死循环卡住整个测试流程
RUN_TEST_TIMEOUT = 120


def run_tests() -> int:
    """执行 tests 下的所有用例, 返回进程退出码 (0 表示全部通过)。"""
    os.chdir(REPO_DIR)

    # compile-pass 用例使用新的 lexer/std API, 需要 `python x.py build` 产出的
    # stage1 编译器, 而不是 bootstrap 的 stage0。
    compiler = "./out/stage1/bin/zinc"
    if not os.path.exists(compiler):
        print("python x.py test requires ./out/stage1/bin/zinc. Run `python x.py build` first.")
        return 1

    pass_dir = os.path.join(TESTS_DIR, "compile-pass")
    fail_dir = os.path.join(TESTS_DIR, "compile-fail")
    run_dir = os.path.join(TESTS_DIR, "run-pass")
    run_build_dir = os.path.join(REPO_DIR, "out", "test-run")
    failed = 0
    ran = 0

    def zinc_check(path):
        cmd = [compiler, "-O0", path, "--check-only"]
        print("run:", " ".join(cmd))
        # 编译器的 stdout/stderr 全部丢弃, 测试结果只通过退出码判断
        return subprocess.run(cmd, text=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    def zinc_run(path, name):
        # 每个用例单独一个输出目录, 避免 self.bc / self.o 互相覆盖
        out_dir = os.path.join(run_build_dir, name)
        shutil.rmtree(out_dir, ignore_errors=True)
        os.makedirs(out_dir, exist_ok=True)
        # 编译器产出的可执行文件名 = 源文件名去掉 .zn 后缀
        exe = os.path.join(out_dir, name[:-3])

        cmd = [compiler, "-O0", path, f"--out-dir={out_dir}"]
        print("run:", " ".join(cmd))
        compiled = subprocess.run(cmd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        output = compiled.stdout or ""
        if compiled.returncode != 0:
            return False, "compilation failed", output
        if not os.path.exists(exe):
            return False, f"executable not found: {exe}", output

        print("run:", exe)
        try:
            executed = subprocess.run(
                [exe], text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                cwd=REPO_DIR, timeout=RUN_TEST_TIMEOUT)
        except subprocess.TimeoutExpired:
            return False, f"timed out after {RUN_TEST_TIMEOUT}s", output
        output += executed.stdout or ""
        # 通过标准: 程序正常退出, 即断言全部成立、没有 panic
        if executed.returncode != 0:
            return False, f"process exited with code {executed.returncode}", output
        return True, "", output

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

    if os.path.isdir(run_dir):
        for name in sorted(os.listdir(run_dir)):
            if not name.endswith(".zn"):
                continue
            ran += 1
            ok, reason, output = zinc_run(os.path.join(run_dir, name), name)
            if ok:
                print(f"ok   run-pass {name}")
            else:
                print(f"FAIL run-pass {name} ({reason})")
                if output:
                    print(output.rstrip())
                failed += 1

    print(f"{ran - failed}/{ran} tests passed")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(run_tests())

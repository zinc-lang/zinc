#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Zinc 测试执行器。

通常由 `python x.py test` 调用, 也可以单独运行: `python tests/run_tests.py`。

用例按"特性 / 期望"两层目录组织:

    tests/<特性>/<期望>/<用例>.zn

例如:

    tests/lifetime/compile-pass/lifetime_borrow_same_scope.zn
    tests/lambda/compile-fail/lambda_call_arg_type.zn
    tests/hash/run-pass/hashmap_ops.zn

| 期望目录 | 要求 |
|----------|------|
| compile-pass | `zinc --check-only` 成功 |
| compile-fail | `zinc --check-only` 失败 |
| run-pass     | 编译成可执行文件, 运行后正常退出 (没有 panic) |

新增用例: 在对应特性目录下选一个期望目录放进去即可; 特性目录不存在就新建一个。
放在其它位置的 `.zn` 不会被收集, 执行器最后会给出提示。
"""

import os
import sys
import subprocess
import shutil

TESTS_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_DIR = os.path.dirname(TESTS_DIR)

# run-pass 用例单个程序的执行超时 (秒), 防止死循环卡住整个测试流程
RUN_TEST_TIMEOUT = 120

COMPILE_PASS = "compile-pass"
COMPILE_FAIL = "compile-fail"
RUN_PASS = "run-pass"
KINDS = (COMPILE_PASS, COMPILE_FAIL, RUN_PASS)


def feature_dirs():
    """tests 下的特性目录 (跳过 __pycache__、以 . / _ 开头的杂物)。"""
    for name in sorted(os.listdir(TESTS_DIR)):
        if name.startswith(".") or name.startswith("_"):
            continue
        path = os.path.join(TESTS_DIR, name)
        if os.path.isdir(path):
            yield name, path


def case_files(kind_dir):
    """某个期望目录下的 .zn 用例名 (排序)。"""
    if not os.path.isdir(kind_dir):
        return []
    return [name for name in sorted(os.listdir(kind_dir)) if name.endswith(".zn")]


def stray_cases():
    """收集 tests 下位置不合法 (不是 <特性>/<期望>/<用例>.zn) 的 .zn 用例。"""
    strays = []
    for dirpath, dirnames, filenames in os.walk(TESTS_DIR):
        dirnames[:] = [d for d in dirnames if d != "__pycache__"]
        for name in filenames:
            if not name.endswith(".zn"):
                continue
            rel = os.path.relpath(os.path.join(dirpath, name), TESTS_DIR)
            parts = rel.split(os.sep)
            if len(parts) != 3 or parts[1] not in KINDS:
                strays.append(rel)
    return sorted(strays)


def run_tests() -> int:
    """执行 tests 下的所有用例, 返回进程退出码 (0 表示全部通过)。"""
    os.chdir(REPO_DIR)

    # compile-pass 用例使用新的 lexer/std API, 需要 `python x.py build` 产出的
    # stage1 编译器, 而不是 bootstrap 的 stage0。
    compiler = "./out/stage1/bin/zinc"
    if not os.path.exists(compiler):
        print("python x.py test requires ./out/stage1/bin/zinc. Run `python x.py build` first.")
        return 1

    run_build_dir = os.path.join(REPO_DIR, "out", "test-run")
    failed = 0
    ran = 0

    def zinc_check(path):
        cmd = [compiler, "-O0", path, "--check-only"]
        print("run:", " ".join(cmd))
        # 编译器的 stdout/stderr 全部丢弃, 测试结果只通过退出码判断
        return subprocess.run(cmd, text=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    def zinc_run(path, name, out_dir):
        # 每个用例单独一个输出目录, 避免 self.bc / self.o 互相覆盖
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

    for feature, feature_dir in feature_dirs():
        for kind in KINDS:
            kind_dir = os.path.join(feature_dir, kind)
            for name in case_files(kind_dir):
                path = os.path.join(kind_dir, name)
                # 输出里带上完整相对路径, 便于定位 (同一个用例在两种期望下可能同名)
                label = f"{feature}/{kind}/{name}"
                ran += 1

                if kind == COMPILE_PASS:
                    if zinc_check(path).returncode != 0:
                        print(f"FAIL {label} (expected: compilation succeeds)")
                        failed += 1
                    else:
                        print(f"ok   {label}")
                elif kind == COMPILE_FAIL:
                    if zinc_check(path).returncode == 0:
                        print(f"FAIL {label} (expected: compilation fails, but the compiler accepted it)")
                        failed += 1
                    else:
                        print(f"ok   {label}")
                else:
                    out_dir = os.path.join(run_build_dir, feature, name)
                    ok, reason, output = zinc_run(path, name, out_dir)
                    if ok:
                        print(f"ok   {label}")
                    else:
                        print(f"FAIL {label} ({reason})")
                        if output:
                            print(output.rstrip())
                        failed += 1

    for stray in stray_cases():
        print(f"warning: 未被收集的用例 (要放在 <特性>/<期望>/ 下): tests/{stray}")

    print(f"{ran - failed}/{ran} tests passed")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(run_tests())

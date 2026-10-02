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
| compile-fail | `zinc --check-only` 失败, 并且给出了错误诊断 (`[错误]: ...`) |
| run-pass     | 编译成可执行文件, 运行后正常退出 (没有 panic) |

调试时可以覆盖:
    ZINC_TEST_COMPILER=./out/stage2/bin/zinc python tests/run_tests.py
    python tests/run_tests.py --self-test   # 用假编译器自检崩溃检测逻辑

新增用例: 在对应特性目录下选一个期望目录放进去即可; 特性目录不存在就新建一个。
放在其它位置的 `.zn` 不会被收集, 执行器最后会给出提示。
"""

import os
import re
import shutil
import signal
import stat
import subprocess
import sys

TESTS_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_DIR = os.path.dirname(TESTS_DIR)

COMPILE_PASS = "compile-pass"
COMPILE_FAIL = "compile-fail"
RUN_PASS = "run-pass"
KINDS = (COMPILE_PASS, COMPILE_FAIL, RUN_PASS)

# 默认用 stage1 (bootstrap 的 stage0 是旧版本, 语义/API 可能不一致)
DEFAULT_COMPILER = os.environ.get("ZINC_TEST_COMPILER", "./out/stage1/bin/zinc")
# 编译单个用例的超时 (秒): 编译器卡死也是 bug, 不能把整个测试流程挂住
COMPILE_TIMEOUT = float(os.environ.get("ZINC_TEST_COMPILE_TIMEOUT", "300"))
# run-pass 用例单个程序的执行超时 (秒), 防止死循环卡住整个测试流程
RUN_TEST_TIMEOUT = float(os.environ.get("ZINC_TEST_RUN_TIMEOUT", "120"))

# 编译器自己崩溃时输出的特征。正常诊断不会用这些字样, 但诊断里会回显用例源码,
# 所以检查前要先排除源码回显行 (见 SOURCE_ECHO_RE)。
CRASH_MARKERS = (
    "⚠️ Panic:",         # zinc 运行时的 panic (编译器自身或被测程序)
    "Panic:",            # zinc_core 里的原生断言
    "BUG:",              # 编译器内部断言 panic "BUG: ..."
    "stackoverflow",     # 运行时检测到的栈溢出 (parser 无限递归等)
    "panicked at",       # 兜底: rust 风格 panic 文本
    "Aborted",           # abort()
    "Segmentation fault",
)
# 诊断输出里的源码回显行 (例如 " 12 |     let a = 1_i;") 和 caret 行
SOURCE_ECHO_RE = re.compile(r"^\s*\d+ \|")
CARET_RE = re.compile(r"^\s*\^+\s*$")
# compile-fail 必须给出至少一条错误诊断
ERROR_MARKER = "[错误]"


def iter_cases(tests_dir):
    """递归收集所有用例, 返回 [(label, kind, path)]。

    布局是 <特性>/.../<期望>/<用例>.zn —— 特性目录可以嵌套任意层
    (例如 expressions/binary/compile-pass/add.zn), 只要用例的父目录是期望目录。
    """
    cases = []
    for dirpath, dirnames, filenames in os.walk(tests_dir):
        dirnames[:] = [d for d in dirnames if not d.startswith(".") and d != "__pycache__"]
        kind = os.path.basename(dirpath)
        if kind not in KINDS:
            continue
        rel_dir = os.path.relpath(dirpath, tests_dir)
        feature = os.path.dirname(rel_dir)
        for name in sorted(filenames):
            if name.endswith(".zn"):
                cases.append((f"{feature}/{kind}/{name}", kind, os.path.join(dirpath, name)))
    return sorted(cases)


def stray_cases(tests_dir):
    """收集位置不合法 (父目录不是 compile-pass/compile-fail/run-pass) 的 .zn。"""
    strays = []
    for dirpath, dirnames, filenames in os.walk(tests_dir):
        dirnames[:] = [d for d in dirnames if d != "__pycache__"]
        if os.path.basename(dirpath) in KINDS:
            continue  # 期望目录里的 .zn 都是用例
        for name in filenames:
            if name.endswith(".zn"):
                strays.append(os.path.relpath(os.path.join(dirpath, name), tests_dir))
    return sorted(strays)


def signal_name(num):
    try:
        return signal.Signals(num).name
    except ValueError:
        return f"signal {num}"


def crash_reason(returncode, output):
    """编译器自身崩溃的说明; 没有崩溃返回 None。

    注意: 这里只看编译器进程, 不看被测程序的输出 (run-pass 的程序可以有意打印
    panic 文本, 例如 backtrace 用例)。
    """
    if returncode is not None and returncode < 0:
        return f"编译器被信号杀死 ({signal_name(-returncode)})"
    for line in output.splitlines():
        if SOURCE_ECHO_RE.match(line) or CARET_RE.match(line):
            continue
        for marker in CRASH_MARKERS:
            if marker in line:
                return f"编译器崩溃 ({marker})"
    return None


class CaseResult:
    def __init__(self, label, ok, reason="", output=""):
        self.label = label
        self.ok = ok
        self.reason = reason
        self.output = output


def run_process(cmd, timeout):
    """跑一个子进程, 合并 stdout/stderr, 返回 (returncode, output, timed_out)。"""
    try:
        proc = subprocess.run(
            cmd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            timeout=timeout)
    except subprocess.TimeoutExpired as exc:
        out = exc.stdout or ""
        if isinstance(out, bytes):
            out = out.decode("utf-8", "replace")
        return None, out, True
    return proc.returncode, proc.stdout or "", False


def check_case(compiler, path, verbose=True):
    """用 `--check-only` 编译一个用例, 返回 (returncode, output, timed_out)。"""
    cmd = [compiler, "-O0", path, "--check-only"]
    if verbose:
        print("run:", " ".join(cmd))
    return run_process(cmd, COMPILE_TIMEOUT)


def run_case(compiler, path, name, out_dir, verbose=True):
    """编译并运行一个 run-pass 用例, 返回 (ok, reason, output)。"""
    shutil.rmtree(out_dir, ignore_errors=True)
    os.makedirs(out_dir, exist_ok=True)
    # 编译器产出的可执行文件名 = 源文件名去掉 .zn 后缀
    exe = os.path.join(out_dir, name[:-3])

    cmd = [compiler, "-O0", path, f"--out-dir={out_dir}"]
    if verbose:
        print("run:", " ".join(cmd))
    code, output, timed_out = run_process(cmd, COMPILE_TIMEOUT)
    if timed_out:
        return False, f"编译器卡死 (超过 {COMPILE_TIMEOUT:.0f}s)", output
    crashed = crash_reason(code, output)
    if crashed:
        return False, crashed, output
    if code != 0:
        return False, "compilation failed", output
    if not os.path.exists(exe):
        return False, f"executable not found: {exe}", output

    if verbose:
        print("run:", exe)
    code, run_output, timed_out = run_process([exe], RUN_TEST_TIMEOUT)
    output += run_output
    if timed_out:
        return False, f"程序运行超时 (超过 {RUN_TEST_TIMEOUT:.0f}s)", output
    if code is None:
        return False, "程序没有正常返回", output
    if code < 0:
        return False, f"程序被信号杀死 ({signal_name(-code)})", output
    # 通过标准: 程序正常退出, 即断言全部成立、没有 panic
    if code != 0:
        return False, f"process exited with code {code}", output
    return True, "", output


def collect_results(compiler, tests_dir, run_build_dir, verbose=True):
    """收集所有用例的结果 (不打印结论)。"""
    results = []
    for label, kind, path in iter_cases(tests_dir):
        # 输出里带上完整相对路径, 便于定位 (同一个用例在两种期望下可能同名)
        feature = os.path.dirname(os.path.dirname(label))
        name = os.path.basename(path)
        if True:
                if kind == COMPILE_PASS:
                    code, output, timed_out = check_case(compiler, path, verbose)
                    if timed_out:
                        results.append(CaseResult(
                            label, False, f"编译器卡死 (超过 {COMPILE_TIMEOUT:.0f}s)", output))
                        continue
                    crashed = crash_reason(code, output)
                    if crashed:
                        results.append(CaseResult(label, False, crashed, output))
                    elif code != 0:
                        results.append(CaseResult(
                            label, False, "expected: compilation succeeds", output))
                    else:
                        results.append(CaseResult(label, True))
                elif kind == COMPILE_FAIL:
                    code, output, timed_out = check_case(compiler, path, verbose)
                    if timed_out:
                        results.append(CaseResult(
                            label, False, f"编译器卡死 (超过 {COMPILE_TIMEOUT:.0f}s)", output))
                        continue
                    crashed = crash_reason(code, output)
                    if crashed:
                        # 重点: 崩溃不是"用例失败", 是编译器的 bug, 不能算通过
                        results.append(CaseResult(label, False, crashed, output))
                    elif code == 0:
                        results.append(CaseResult(
                            label, False, "expected: compilation fails, but the compiler accepted it", output))
                    elif ERROR_MARKER not in output:
                        results.append(CaseResult(
                            label, False, "compilation failed but the compiler printed no diagnostic", output))
                    else:
                        results.append(CaseResult(label, True))
                else:
                    out_dir = os.path.join(run_build_dir, feature, name)
                    ok, reason, output = run_case(compiler, path, name, out_dir, verbose)
                    results.append(CaseResult(label, ok, reason, output))
    return results


def run_tests(compiler=None, tests_dir=None, verbose=True):
    """执行 tests 下的所有用例, 返回进程退出码 (0 表示全部通过)。"""
    compiler = compiler or DEFAULT_COMPILER
    tests_dir = tests_dir or TESTS_DIR
    if verbose:
        os.chdir(REPO_DIR)

    if not os.path.exists(compiler):
        print(f"python x.py test requires {compiler}. Run `python x.py build` first.")
        return 1

    run_build_dir = os.path.join(REPO_DIR, "out", "test-run")
    results = collect_results(compiler, tests_dir, run_build_dir, verbose)

    failed = 0
    for res in results:
        if res.ok:
            print(f"ok   {res.label}")
        else:
            print(f"FAIL {res.label} ({res.reason})")
            if res.output:
                print(res.output.rstrip())
            failed += 1

    for stray in stray_cases(tests_dir):
        print(f"warning: 未被收集的用例 (要放在 <特性>/<期望>/ 下): tests/{stray}")

    print(f"{len(results) - failed}/{len(results)} tests passed")
    return 1 if failed else 0


# --------------------------------------------------------------------------
# 自检: 用假编译器验证"崩溃必须被揪出来"的逻辑本身是可靠的
# --------------------------------------------------------------------------

FAKE_COMPILER = r"""#!/usr/bin/env python3
import os, sys
mode = os.environ.get("FAKE_MODE", "ok")
if mode == "ok":
    sys.exit(0)
if mode == "diagnostic-fail":
    print("[错误]: fake error")
    sys.exit(1)
if mode == "silent-fail":
    sys.exit(1)
if mode == "panic":
    print("\u26a0\ufe0f Panic: BUG: fake panic")
    sys.exit(1)
if mode == "stackoverflow":
    print("Error: stackoverflow detected")
    sys.exit(2)
if mode == "signal":
    os.kill(os.getpid(), 9)
if mode == "hang":
    import time
    time.sleep(3600)
sys.exit(0)
"""


def self_test():
    import tempfile

    # (FAKE_MODE, compile-pass 应该通过?, compile-fail 应该通过?)
    cases = [
        ("ok", True, False),
        ("diagnostic-fail", False, True),
        ("silent-fail", False, False),
        ("panic", False, False),
        ("stackoverflow", False, False),
        ("signal", False, False),
        ("hang", False, False),
    ]
    failures = 0
    with tempfile.TemporaryDirectory() as td:
        tests_dir = os.path.join(td, "tests")
        # 嵌套特性目录也要能被收集 (expressions/nested/<kind>/...)
        for kind, name in ((COMPILE_PASS, "ok_case"), (COMPILE_FAIL, "bad_case")):
            kind_dir = os.path.join(tests_dir, "expressions", "nested", kind)
            os.makedirs(kind_dir, exist_ok=True)
            with open(os.path.join(kind_dir, name + ".zn"), "w") as f:
                f.write("fn main() { }\n")
        fake = os.path.join(td, "fake_zinc")
        with open(fake, "w") as f:
            f.write(FAKE_COMPILER)
        os.chmod(fake, os.stat(fake).st_mode | stat.S_IEXEC | stat.S_IXGRP | stat.S_IXOTH)

        # 卡死用例要跑得快: 直接改模块级超时 (函数在调用时读全局)
        global COMPILE_TIMEOUT
        old_timeout = COMPILE_TIMEOUT
        COMPILE_TIMEOUT = 3
        try:
            for mode, expect_pass, expect_fail in cases:
                os.environ["FAKE_MODE"] = mode
                results = collect_results(
                    fake, tests_dir, os.path.join(td, "out"), verbose=False)
                # label 形如 <特性>/.../<期望>/<用例>.zn: 期望目录永远是倒数第二段
                got = {res.label.split("/")[-2]: res.ok for res in results}
                ok = (got.get(COMPILE_PASS) == expect_pass) and (got.get(COMPILE_FAIL) == expect_fail)
                print(f"{'ok  ' if ok else 'FAIL'} self-test mode={mode:16s} "
                      f"compile-pass={got.get(COMPILE_PASS)} compile-fail={got.get(COMPILE_FAIL)}"
                      f" (expect {expect_pass}/{expect_fail})")
                if not ok:
                    failures += 1
                    for res in results:
                        if not res.ok:
                            print(f"     {res.label}: {res.reason}")
        finally:
            COMPILE_TIMEOUT = old_timeout
            os.environ.pop("FAKE_MODE", None)

    print(f"{len(cases) - failures}/{len(cases)} self-test cases passed")
    return 1 if failures else 0


if __name__ == "__main__":
    if "--self-test" in sys.argv[1:]:
        sys.exit(self_test())
    sys.exit(run_tests())

# Zinc compiler tests

Run `python x.py build` then `python x.py test`. The harness uses `./out/stage1/bin/zinc`.

测试执行器是 `tests/run_tests.py` (`x.py test` 只是调用它), 也可以单独运行:

```
python tests/run_tests.py
```

## 目录结构

用例按 **特性 / 期望** 两层目录组织: `tests/<特性>/<期望>/<用例>.zn`

```
tests/
  lifetime/          # 生命周期 / 借用 / 装箱时的生命周期擦除
    compile-pass/    #   zinc --check-only 成功
    compile-fail/    #   zinc --check-only 失败
    run-pass/        #   编译成可执行文件并运行, 正常退出 (没有 panic)
  lambda/            # 闭包: 语法、调用、捕获、作为值 / 函数指针
  fn/                # 函数项、fn 指针、fn 类型
  hash/              # HashMap / HashSet
  string/            # String / 字符串字面量 (含 raw string、C string)
  range/             # range 表达式与 for-in range
  misc/              # 其它还没有单独分类的特性 (整数辅助函数、BackTrace 等)
```

| 期望目录 | 要求 |
|----------|------|
| `compile-pass/` | `zinc --check-only` 成功 |
| `compile-fail/` | `zinc --check-only` 失败 |
| `run-pass/` | 编译成可执行文件, 运行后正常退出 (没有 panic / 非零退出码) |

新增用例: 在对应特性目录下选一个期望目录放进去; 特性目录不存在就新建一个。
`.zn` 放在其它位置 (比如直接放在 `tests/` 或特性目录下) 不会被收集, 执行器结束前会打印
`warning: 未被收集的用例`。

## run-pass 用例

`run-pass/` 里的每个 `.zn` 文件都会被完整编译成可执行文件并运行一次:

- 编译失败、可执行文件缺失、运行超时 (120s) 或程序非正常退出 (退出码非 0, 例如 `panic`) 都算失败;
- 通过与否只看程序有没有 panic, 因此用例内部要用标准库的 assert 系列函数自己判断结果:

```zinc
use std::{assert, assert_eq, assert_ne};

fn main() {
    assert(1_i + 1_i == 2_i);
    assert_eq(2_i + 3_i, 5_i);
    assert_ne(2_i + 3_i, 6_i);
}
```

编译产物放在 `out/test-run/<特性>/<用例名>/` 下, 每个用例一个独立目录。


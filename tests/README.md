# Zinc compiler tests

Run `python x.py build` then `python x.py test`. The harness uses `./out/stage1/bin/zinc`.

| Directory | Expectation |
|-----------|-------------|
| `compile-pass/` | `zinc --check-only` succeeds |
| `compile-fail/` | `zinc --check-only` fails |
| `run-pass/` | compiles, then the produced program runs and exits normally (no panic / non-zero exit) |

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

编译产物放在 `out/test-run/<用例名>/` 下, 每个用例一个独立目录。


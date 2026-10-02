# Zinc compiler tests

Run `python x.py build` then `python x.py test`. The harness uses `./out/stage1/bin/zinc`.

测试执行器是 `tests/run_tests.py` (`x.py test` 只是调用它), 也可以单独运行:

```
python tests/run_tests.py
```

## 目录结构

用例按 **特性 / 期望** 目录组织: `tests/<特性>/.../<期望>/<用例>.zn`。
特性目录**可以嵌套任意层**, 唯一的要求是用例的父目录必须是期望目录
(`compile-pass/`、`compile-fail/`、`run-pass/`)。

```
tests/
  lifetime/          # 生命周期 / 借用 / 装箱时的生命周期擦除
    compile-pass/    #   zinc --check-only 成功
    compile-fail/    #   zinc --check-only 失败 (且必须有 [错误] 诊断)
    run-pass/        #   编译成可执行文件并运行, 正常退出 (没有 panic)
  lambda/            # 闭包: 语法、调用、捕获、作为值 / 函数指针
  fn/                # 函数项、fn 指针、fn 类型
  hash/              # HashMap / HashSet
  string/            # String / 字符串字面量 (含 raw string、C string、插值)
  range/             # range 表达式与 for-in range
  enum/              # 枚举: 判别值、match 分支
  misc/              # 其它还没有单独分类的特性 (整数辅助函数、BackTrace 等)

  # 下面两棵树按 parser 的语法结构铺开: 一个子目录 = 一个语法结构
  expressions/
    binary_ops/      # 二元运算: 算术/比较/逻辑、优先级、结合性
    unary_ops/       # 一元运算: 取负/取反/解引用/取引用 ...
    assign/          # 赋值与复合赋值
    ternary/         # `if c then a else b`
    as_cast/         # `as` 类型转换
    is_expr/         # `is` 模式测试
    question/        # `?` 运算符 (未实现, 只有 compile-fail)
    paren/           # 括号分组
    literal/         # 整型/浮点/布尔/字符/字节 等字面量
    array/           # 数组字面量与重复字面量
    tuple/           # 元组字面量
    struct_init/     # 结构体初始化 (含 `...` 默认值、上下文表达式)
    index/           # 下标访问
    member_access/   # 字段访问与方法调用链
    call/            # 函数调用 / 方法调用
    type_ascription/ # `expr: Type`
    name/            # 名字与限定路径
  statements/
    let/             # let 绑定与模式解构
    if/              # if / else if / else
    while/           # while 循环
    do_while/        # do-while 循环
    for/             # for-in 循环
    match/           # match 语句 (arm 之间不写分隔符)
    break_continue/  # break / continue
    return/          # return
    block/           # 块与作用域
    expr_stmt/       # 表达式语句
    empty_stmt/      # 空语句
    panic/           # panic 语句
    unsafe/          # unsafe 块
```

| 期望目录 | 要求 |
|----------|------|
| `compile-pass/` | `zinc --check-only` 成功 |
| `compile-fail/` | `zinc --check-only` 失败, **并且给出了错误诊断** (`[错误]: ...`) |
| `run-pass/` | 编译成可执行文件, 运行后正常退出 (没有 panic / 非零退出码) |

**任何情况下编译器都不允许 panic / abort / ICE / 被信号杀死 / 卡死** —— 那是编译器的 bug,
不是"用例失败"。所以三种用例都会先检查编译器是否崩溃, 崩溃一律判为 `FAIL`:

* 输出里出现崩溃特征 (`⚠️ Panic:`、`Panic:`、`BUG:`、`stackoverflow` 等);
* 进程被信号杀死 (段错误 / abort, 退出码为负);
* 编译超时 (默认 300s)。

这样做是必要的: 只看退出码的话, `compile-fail` 用例里编译器**崩溃**也会被当成"按预期失败"
而通过。`compile-fail` 额外要求输出里至少有一条 `[错误]`, 这样"静默退出 1"也不会漏过。

新增用例: 在对应特性目录下选一个期望目录放进去即可; 特性目录不存在就新建一个
(支持 `expressions/xxx/compile-pass/` 这种嵌套)。`.zn` 放在其它位置 (比如直接放在
`tests/` 或特性目录下) 不会被收集, 执行器结束前会打印 `warning: 未被收集的用例`。

## 已知编译器 bug 的"记录型"用例

有少数用例在注释里标明了"当前行为是 bug"（例如进制字面量解析、浮点后缀数值、整型范围
不校验、命名参数按位置绑定）。修好对应 bug 之后这些用例会失败, 需要同步改成正确期望。

## run-pass 用例

`run-pass` 用例编译出的可执行文件会被运行, 要求正常退出。断言写在一起:

```zinc
use std::{assert, assert_eq};

fn main() {
    assert(1_i + 1_i == 2_i);
    assert_eq(1_i + 1_i, 2_i);
}
```

## 调试

```
ZINC_TEST_COMPILER=./out/stage2/bin/zinc ZINC_TEST_COMPILE_TIMEOUT=600 python tests/run_tests.py
python tests/run_tests.py --self-test      # 用假编译器自检崩溃检测逻辑
```

| 环境变量 | 默认值 | 说明 |
|----------|--------|------|
| `ZINC_TEST_COMPILER` | `./out/stage1/bin/zinc` | 用哪个编译器跑用例 (例如换 stage0/stage2 对比) |
| `ZINC_TEST_COMPILE_TIMEOUT` | `300` | 单个用例的编译超时 (秒) |
| `ZINC_TEST_RUN_TIMEOUT` | `120` | run-pass 用例的运行超时 (秒) |

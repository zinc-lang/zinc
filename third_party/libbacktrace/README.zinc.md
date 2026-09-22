# libbacktrace (vendored)

Vendored copy of Ian Lance Taylor's standalone libbacktrace:
<https://github.com/ianlancetaylor/libbacktrace> (downloaded from `master`, 2026-09).

libbacktrace 用 `_Unwind_Backtrace` 采帧、自己解析 ELF + DWARF, 得到
`file:line:col` 和函数名, 不需要 LLVM, 也不需要 `llvm-symbolizer` 子进程。
GCC 自己也用它来做 sanitizer / `addr2line` 的符号化。

## 保留了哪些文件

只保留 Linux/ELF + 用了 mmap 分配器时要编译的文件 (对应上游 `Makefile.am` 里
`libbacktrace_la_SOURCES` + 各 `*_FILE` 的选择结果):

- 核心: `atomic.c dwarf.c fileline.c posix.c sort.c state.c`
- 采帧: `backtrace.c simple.c`
- 格式: `elf.c`
- 内存: `mmap.c` (分配), `mmapio.c` (视图)
- 头文件: `backtrace.h internal.h filenames.h`
- `LICENSE` (BSD-3-Clause 风格的 FSF 许可)

`print.c` (只提供 `backtrace_print`) 没有保留: 我们只用 `backtrace_pcinfo` +
`backtrace_syminfo`。测试 (`*test.c`)、其它平台的格式后端
(`macho.c`/`pecoff.c`/`xcoff.c`/`unknown.c`)、`alloc.c`、`read.c` 以及 autotools
的构建文件也都没有引入。

## 相对上游的改动

只增加/替换了两个由 configure 生成的文件, 库源码本身未改动:

- `config.h`: Linux/glibc + GCC/Clang 的取值 (`_GNU_SOURCE`、`HAVE_DL_ITERATE_PHDR`、
  64 位 ELF、`__atomic`/`__sync` 等); 未启用 zlib/zstd/lzma 压缩 debug section。
- `backtrace-supported.h`: `BACKTRACE_SUPPORTED=1`、`BACKTRACE_USES_MALLOC=0`(mmap)、
  `BACKTRACE_SUPPORTS_THREADS=1`、`BACKTRACE_SUPPORTS_DATA=1`。

如果要支持 ELF 以外的目标, 需要按平台重新生成这两个文件并换用对应的格式后端。

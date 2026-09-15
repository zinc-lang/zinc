# Contributing to Zinc

Zinc is a proof-of-concept systems language. The compiler is self-hosted and currently builds on **Linux/x64 only**.

Language-design changes belong in [zinc-lang/zinc-design](https://github.com/zinc-lang/zinc-design). Implementation lives here.

## Setup

1. Download `stage0.zip` from [Releases](https://github.com/zinc-lang/zinc/releases) and unpack it as `./out/stage0`.
2. `python x.py build-llvm`
3. Symlink `./out/stage0/llvm` -> `../llvm`
4. `python x.py build`

See the root README for the expected `./out` layout.

## Tests

```
python x.py build
python x.py test
```

The harness uses `./out/stage1/bin/zinc` (the compiler produced by `python x.py build`). Bootstrap stage0 is not enough: compile-pass cases cover new lexer and std APIs.

`--check-only` only typechecks. It does not execute wrapping overflow, UTF-8 `insert`, or `for` loops.

- `tests/compile-pass/` — files that should typecheck (`--check-only`)
- `tests/compile-fail/` — files that should produce a compile error

Add a short comment at the top of each test describing what it covers.

## Good first contributions

- More `compile-pass` / `compile-fail` cases
- Standard-library methods with tests (collections, strings, integers)
- Diagnostics and error-message wording
- VS Code grammar, snippets, and README (`tools/vscode-zinc`)
- Build-script cleanup in `x.py` (LLVM library list, docs)

## Larger work

File an issue first. These need a design proposal in zinc-design before a large implementation:

- unwind / exceptions
- const generics
- unsized types as parameters/generics
- ABI-stable dynamic libraries
- async I/O

The compiler currently stubs several parsed features (`lambda`, `?`, `await`/`yield`, generic traits, operator overload). Prefer finishing a stub with tests over adding a new keyword.

## Pull requests

- Keep PRs focused.
- Link an issue.
- Do not mix language-design prose with compiler changes; split across the two repositories.

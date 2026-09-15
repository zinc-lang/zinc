# vscode-zinc

VS Code support for the Zinc language (`.zn` files).

## Features

- Syntax highlighting for keywords, types, numbers, comments, strings, raw strings (`r#"..."#`), f-strings, and C strings
- Bracket matching, comment toggling, and auto-closing pairs
- Snippets: `fn`, `struct`, `impl`, `match`, `mod`

This extension does **not** include an LSP yet. Use it for editing; compile with the `zinc` CLI.

## Package

```
vsce package
```

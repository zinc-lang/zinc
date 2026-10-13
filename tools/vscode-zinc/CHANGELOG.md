# Change Log

All notable changes to the "zinc" extension will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.

## [0.1.2]

- Snippets for Zinc `println` / `print` / f-strings (not Rust `println!`)
- Expanded automated tests (fixtures, compiler integration, snippets, manifest)
- Extension Host UI tests via `@vscode/test-electron` (`npm run test:ui`)

## [0.1.1]

- Register definition, format, and completion in the extension host
- Debug adapter injects `zinc.compilerPath` and runs without pausing at entry

## [0.1.0]

- Language server: diagnostics, completion, go-to-definition, formatting
- Debug adapter: compile-and-run launch configuration
- Unit tests for parser, symbols, formatter, completion, and CLI backend

## [0.0.2]

- Syntax highlighting and snippets

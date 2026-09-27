# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

stone is a small Python-like language implemented in Rust (edition 2024, only dependency: `clap`). One binary provides both a tree-walking interpreter and an x86-64 compiler. Syntax is designed to minimize Shift-key use: blocks open with `;` instead of `:` (e.g. `if x;`, `def f(a, b);`), functions return with `ret`, `continue` is `cont`. See `random.st` for a sample program.

## Commands

- Build: `cargo build`
- Test: `cargo test`. CI also runs `cargo fmt --check` and `cargo clippy --all-targets -- -D warnings`
- Single test: `cargo test <name>` (e.g. `cargo test simple_functions`). Unit tests live in `#[cfg(test)]` modules in `src/lexer.rs`, `src/parser/tests.rs`, and `src/driver.rs`
- Golden-output tests: `tests/programs.rs` runs every `.st` in `examples/` and `tests/programs/` through both `stone run` and `stone build`, comparing stdout with the sibling `.out` file. Programs a backend can't handle yet are listed in `SKIPS` with a reason. Add a program plus its `.out` to cover new behavior
- Interpret a file: `cargo run -- run examples/basics.st` (or `cargo run -- examples/basics.st`)
- Compile a file: `cargo run -- build examples/basics.st [-o build/out]` writes `<output>.s`, then invokes `gcc -g -no-pie` to produce `<output>` (requires gcc on PATH; the default `build/out` is relative to the working directory)
- Format: `cargo fmt`
- Lint: `cargo clippy --all-targets`
- `check` subcommand and REPL mode (no args) are `todo!()` stubs.

The `debug!` macro (defined in `src/lib.rs`) prints to stderr only in debug builds, so `cargo run` shows token dumps; use `--release` or redirect stderr for clean output.

## Workflow

- Practice TDD. Write the test first, run it and confirm it fails for the expected reason, then write the implementation that makes it pass.
- After every change, run `cargo fmt` and `cargo clippy`, and address what clippy reports.

## Architecture

The crate is a library (`src/lib.rs`) plus a thin CLI (`src/main.rs`, clap only). Pipelines are wired in `src/driver.rs`:

- **run** (`driver::interpret`): `Lexer` → `Parser` → `TypeChecker` (currently a no-op) → `Interpreter`
- **build** (`driver::compile`): `Lexer` → `Parser` → `X64Generator::compile` (scan pass, then generate pass, then gcc)

Key pieces:

- `src/token.rs`: `Token`/`TokenType` and `RESERVED_KEYWORDS`. The lexer (`src/lexer.rs`) emits Python-style `Indent`/`Dedent`/`Newline` tokens.
- `src/ast.rs`: AST (`Mod`, `Stmt`, `Expr`, `Constant`, `ParserError`, the `Type` annotation enum, …), modeled on the ASDL in `docs/grammar/stone.asdl`.
- `src/parser.rs`: hand-written recursive-descent PEG parser. The struct, token helpers, and `parse()` entry are here; rules live in `src/parser/expressions.rs` and `src/parser/statements.rs` as `pub(super)` methods so siblings can call each other. Functions mirror rule names in `docs/grammar/stone.gram` (derived from CPython's grammar, kept as `python_grammar.gram` for reference), e.g. `parse_t_primary`, `parse_star_targets`, `*_loop0` for repetition. Keep the `.gram`/`.asdl` docs in sync when changing syntax.
- `src/interpreter.rs`: evaluates the AST directly using a scope stack; `ControlFlow` carries break/continue/return.
- `src/codegen.rs`: `AssemblyGenerator` trait (`compile`/`scan`/`generate`/`emit`/`architecture`) plus assembler/linker discovery helpers.
- `src/codegen/x64.rs`: `X64Generator`, emitting GNU-as Intel syntax (`.intel_syntax noprefix`). Pass 1 scans the AST for stack sizes per function/closure and string literals; pass 2 generates code into a buffer, which `compile` writes out. Top-level statements are wrapped in a synthesized `main` unless the program defines `main`. String literals are interned and emitted in `.rodata`.
- Builtins: `BUILTINS` in `src/stdlib.rs` lists names shared by both backends; `src/codegen/x64/builtins.rs` emits hand-written assembly for each via `&mut dyn AssemblyGenerator`. Only builtins actually called are emitted (`collect_stdlib_calls`). Adding a builtin means updating `BUILTINS`, the x64 emitter, the dispatch in `X64Generator::emit_stdlib`, and the interpreter, plus a program in `tests/programs/`.

`build/` is gitignored scratch output.

## Style

These apply to code, comments, docs, and commit messages.

- Never use em dashes.
- Never use emojis.
- Always use American English (e.g. "initialize", "color", "behavior").
- Doc comments (`///`, `//!`) are written in full sentences, and should include examples wherever practical:

  ```rust
  /// Rounds a frame size up to the 16-byte stack alignment required by the System V ABI.
  ///
  /// For example, `align16(20)` returns `32` and `align16(32)` returns `32`.
  fn align16(size: i32) -> i32 {
  ```

- Inline comments (`//`) start lowercase and are almost never full sentences, e.g. `// globals live in main's frame`, not `// Globals live in main's frame.`

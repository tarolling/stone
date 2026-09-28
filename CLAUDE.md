# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

stone is a small Python-like language implemented in Rust (edition 2024, only dependency: `clap`). The workspace also holds `lsp/`, the `stone-lsp` language server, which has its own dependencies, and `editors/vscode/`, a VS Code extension that runs it. One binary provides both a tree-walking interpreter and an x86-64 compiler. Syntax is designed to minimize Shift-key use: blocks open with `;` instead of `:` (e.g. `if x;`, `def f(a, b);`), functions return with `ret`, `continue` is `cont`. See `random.st` for a sample program.

## Commands

- Build: `cargo build`
- Test: `cargo test`. CI also runs `cargo fmt --check` and `cargo clippy --all-targets -- -D warnings`
- Single test: `cargo test <name>` (e.g. `cargo test simple_functions`). Unit tests live in `#[cfg(test)]` modules in `src/lexer.rs`, `src/parser/tests.rs`, `src/checker/tests.rs`, `src/codegen/x64.rs`, and `src/driver.rs`. `tests/cli.rs` runs the binary to check error output and exit codes
- Golden-output tests: `tests/programs.rs` runs every `.st` in `examples/` and `tests/programs/` through both `stone run` and `stone build`, comparing stdout with the sibling `.out` file. A program that should fail at runtime also has a `.err` file holding text its stderr must contain. Both backends must agree on everything the checker accepts, so `SKIPS` should stay empty. Add a program plus its `.out` to cover new behavior
- Fuzzing: the `fuzz/` crate (cargo-fuzz, nightly) has crash targets `lex`, `parse`, `interpret`, `codegen`, and `structured`, plus `differential`, which compares `stone run` with `stone build` on generated programs. Run one with `cargo +nightly fuzz run parse -- -dict=fuzz/stone.dict`. `cargo test --manifest-path fuzz/Cargo.toml` checks the program generator on stable, including that generated programs pass the checker and that 150 of them print the same under both backends (needs gcc). Turn each crash into a regression test, as a unit test or a `tests/programs/` program
- Interpret a file: `cargo run -- run examples/basics.st` (or `cargo run -- examples/basics.st`)
- Compile a file: `cargo run -- build examples/basics.st [-o build/out]` writes `<output>.s`, then invokes `gcc -g -no-pie` to produce `<output>` (requires gcc on PATH; the default `build/out` is relative to the working directory)
- Format: `cargo fmt`
- Lint: `cargo clippy --all-targets`
- Check a file without running it: `cargo run -- check examples/basics.st`. Errors print as `file:line:col: error: message` with the source line and a caret, and the exit status is 1
- REPL mode (no args) is a `todo!()` stub.
- Language server: `cargo test -p stone-lsp` runs its tests, and `cargo install --path lsp` installs `stone-lsp`. See `editors/vscode/README.md` to try it in VS Code

The `debug!` macro (defined in `src/lib.rs`) prints token dumps and parser traces to stderr only in debug builds with `STONE_DEBUG` set (e.g. `STONE_DEBUG=1 cargo run -- run file.st`), and never under `cargo fuzz`.

## Workflow

- Practice TDD. Write the test first, run it and confirm it fails for the expected reason, then write the implementation that makes it pass.
- After every change, run `cargo fmt` and `cargo clippy`, and address what clippy reports.

## Architecture

The crate is a library (`src/lib.rs`) plus a thin CLI (`src/main.rs`, clap only). Pipelines are wired in `src/driver.rs`:

- **run** (`driver::interpret`): `Lexer` → `Parser` → `TypeChecker` → `Interpreter`
- **build** (`driver::compile`): `Lexer` → `Parser` → `TypeChecker` → `X64Generator::compile` (scan pass, then generate pass, then gcc)
- **check** (`driver::check`): `Lexer` → `Parser` → `TypeChecker`, returning every `Diagnostic` instead of running. This is the entry point for editor tooling

Key pieces:

- `src/span.rs`: `Pos` (1-based line and col, counted in chars) and half-open `Span`. Every token and AST node carries one.
- `src/diagnostic.rs`: `Diagnostic` (severity, span, message) with `render` for the CLI, and `Diagnostics` for returning several as one error. Every stage's errors convert to it.
- `src/token.rs`: `Token`/`TokenType` and `RESERVED_KEYWORDS`. The lexer (`src/lexer.rs`) emits Python-style `Indent`/`Dedent`/`Newline` tokens.
- `src/ast.rs`: AST (`Mod`, `Stmt`, `Expr`, `Constant`, `ParserError`, the `Type` annotation enum, …), modeled on the ASDL in `docs/grammar/stone.asdl`. `Expr` and `Stmt` are structs of a `kind` (`ExprKind`/`StmtKind`, the ASDL variants) plus a `span`, so match on `&expr.kind`. Parser tests build expected trees with `.into()` and compare them through `without_spans`.
- `src/parser.rs`: hand-written recursive-descent PEG parser. Syntax errors come from the furthest token any rule failed at: `expect`, `expect_name`, and `record_expected` note what was wanted there, and "quiet" expectations (operators, keywords, and other optional continuations) only show when nothing else was expected. The struct, token helpers, and `parse()` entry are here; rules live in `src/parser/expressions.rs` and `src/parser/statements.rs` as `pub(super)` methods so siblings can call each other. Functions mirror rule names in `docs/grammar/stone.gram` (derived from CPython's grammar, kept as `python_grammar.gram` for reference), e.g. `parse_t_primary`, `parse_star_targets`, `*_loop0` for repetition. Keep the `.gram`/`.asdl` docs in sync when changing syntax.
- `src/checker.rs`: `TypeChecker::analyze` infers types (`int`, `bool`, `str`, `none`, `list[T]`, monomorphic functions) by unification, resolves names Python-style (a function's params and assigned names are local, other names are globals or top-level functions, which are visible before their definition), and returns an `Analysis`: sorted diagnostics, the `Type` of every expression keyed by span, and `Symbol`s with every `Reference`. It only accepts what both backends run identically, e.g. conditions must be `int` or `bool`, `==` cannot compare lists, `range` only appears as a `for` iterable, functions must be top level, and top-level code and functions must assign a variable on every path before reading it (`AssignmentCheck`).
- `src/interpreter.rs`: evaluates the AST directly over runtime `Value`s (lists are shared `Rc<RefCell<Vec>>`); `ControlFlow` carries break/continue/return, and a function sees only its own scope and the globals. `Limits` bounds fuel (loop iterations plus calls) and total evaluation depth, and `print` writes to an injectable writer (`driver::interpret_with`).
- Every stage must return an error, never panic, on bad input: `LexError` for bad literals, a `Diagnostic` from `Parser::parse`, `parser::MAX_DEPTH` for nesting, `Limits` in the interpreter, and `Err(String)` from codegen (`X64Generator::assemble` generates assembly without gcc).
- `src/codegen.rs`: `AssemblyGenerator` trait (`compile`/`scan`/`generate`/`emit`/`architecture`) plus assembler/linker discovery helpers.
- `src/codegen/x64.rs`: `X64Generator`, emitting GNU-as Intel syntax (`.intel_syntax noprefix`). `assemble` runs the checker first and uses its types, e.g. to pick a `print` routine per argument. Pass 1 scans the AST for stack sizes per function and string literals; pass 2 generates code into a buffer, which `compile` writes out. Top-level statements always go in a synthesized `main`, globals live in `.bss` as `g.<name>`, and user functions are `fn.<name>`, so they cannot collide with libc. Arguments are evaluated onto the stack left to right and only loaded into registers right before a `call`; callees read arguments past the sixth from the caller's stack. String literals are interned and emitted in `.rodata`.
- Builtins: `BUILTINS` in `src/stdlib.rs` lists names shared by both backends (`print`, `len`, `range`, `append`). `src/codegen/x64/builtins.rs` emits the hand-written runtime via `&mut dyn AssemblyGenerator`: `stone.print_*` routines (emitted if the program prints), and the string and list runtimes (emitted if the program uses those types). Routines that call `malloc`/`realloc` realign the stack first, since generated code does not keep it aligned. Adding a builtin means updating `BUILTINS`, its type rule in `Inference::infer_builtin`, the interpreter's `Interpreter::call`, and the x64 call dispatch, plus a program in `tests/programs/`.

Language server (`lsp/`, crate `stone-lsp`, depends on `lsp-server`, `lsp-types`, `serde_json`):

- `src/lib.rs`: `run(connection)` does the initialize handshake (preferring UTF-8 positions, falling back to UTF-16), keeps open documents with full-text sync, publishes diagnostics on open/change/close, and dispatches requests through `Server::answer`.
- `src/features.rs`: one pure function per request (`diagnostics`, `hover`, `definition`, `references`, `prepare_rename`, `rename`, `document_symbols`, `completion`) over a `Document`, which holds the text, its `stone::driver::analyze` result, and a `LineIndex`. Add a feature here with unit tests in `src/features/tests.rs`, then wire it into `Server::handle_request` and `capabilities`.
- `src/line_index.rs`: converts stone's 1-based char positions to LSP's 0-based UTF-16 or UTF-8 positions.
- Tests: unit tests for features and positions, `tests/server.rs` over an in-memory connection, and `tests/stdio.rs` against the real binary.
- `driver::analyze` recovers from syntax errors at the top-level statement level (`Parser::parse_recovering`), so features keep working around a typo, and it hides type errors until the syntax errors are fixed.

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

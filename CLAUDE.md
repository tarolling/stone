# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

stone is a small Python-like language implemented in Rust (edition 2024, only dependency: `clap`). The workspace also holds `lsp/`, the `stone-lsp` language server, which has its own dependencies, and `editors/vscode/`, a VS Code extension that runs it. One binary provides both a tree-walking interpreter and an x86-64 compiler. Syntax is designed to minimize Shift-key use: blocks open with `;` instead of `:` (e.g. `if x;`, `def f(a, b);`), functions return with `ret`, `continue` is `cont`. See `random.st` for a sample program.

## Commands

- Build: `cargo build`
- Test: `cargo test`. CI also runs `cargo fmt --check` and `cargo clippy --all-targets -- -D warnings`
- Single test: `cargo test <name>` (e.g. `cargo test simple_functions`). Unit tests live in `#[cfg(test)]` modules in `src/lexer.rs`, `src/parser/tests.rs`, `src/checker/tests.rs`, `src/codegen/ir/lower/tests.rs`, `src/codegen/ir/liveness.rs`, `src/codegen/regalloc.rs`, `src/codegen/x64.rs`, and `src/driver.rs`. `tests/cli.rs` runs the binary to check error output and exit codes
- Golden-output tests: `tests/programs.rs` runs every `.st` in `examples/` and `tests/programs/` through both `stone run` and `stone build`, comparing stdout with the sibling `.out` file. A program that should fail at runtime also has a `.err` file holding text its stderr must contain. Both backends must agree on everything the checker accepts, so `SKIPS` should stay empty. Add a program plus its `.out` to cover new behavior
- Fuzzing: the `fuzz/` crate (cargo-fuzz, nightly) has crash targets `lex`, `parse`, `interpret`, `codegen`, and `structured`, plus `differential`, which compares `stone run` with `stone build` on generated programs. Run one with `cargo +nightly fuzz run parse -- -dict=fuzz/stone.dict`. `cargo test --manifest-path fuzz/Cargo.toml` checks the program generator on stable, including that generated programs pass the checker and that 150 of them print the same under both backends (needs gcc). Turn each crash into a regression test, as a unit test or a `tests/programs/` program
- Benchmarks: `cargo run --release --manifest-path bench/Cargo.toml -- [--runs 5] [--filter fib]` times each program in `bench/programs/` under `stone build`, `gcc -O2`, `gcc -O0`, and `python3`, after checking all four print its `.out` file, and prints a Markdown table (needs gcc and python3). Each benchmark is a `.st`, `.c`, `.py`, and `.out` with the same stem. `cargo test --manifest-path bench/Cargo.toml` checks the runner and that every benchmark passes the checker. See `bench/README.md`
- Interpret a file: `cargo run -- run examples/basics.st` (or `cargo run -- examples/basics.st`). `default-run` picks the `stone` binary, since the workspace also builds `stone-lsp`
- Compile a file: `cargo run -- build examples/basics.st [-o build/out]` writes `<output>.s`, then invokes `gcc -g -no-pie` to produce `<output>` (requires gcc on PATH; the default `build/out` is relative to the working directory)
- Format: `cargo fmt`
- Lint: `cargo clippy --all-targets`
- Check a file without running it: `cargo run -- check examples/basics.st`. Errors print as `file:line:col: error: message` with the source line and a caret, and the exit status is 1
- REPL mode (no args) is a `todo!()` stub.
- Releases: pushing a `v*` tag that matches `version` in `Cargo.toml` runs `.github/workflows/release.yml`, which builds `stone-<target>.tar.gz` plus a `.sha256` for x86-64 and arm64 Linux (musl) and macOS and publishes them as a GitHub release. `install.sh` (POSIX sh) downloads the archive for the current machine into `~/.local/bin` and verifies its checksum. `sh scripts/test-install.sh` (after `cargo build`) tests it against a local `file://` release via `STONE_DOWNLOAD_URL`, and CI runs it plus `shellcheck`
- Language server: `cargo test -p stone-lsp` runs its tests, and `cargo install --path lsp` installs `stone-lsp`. See `editors/vscode/README.md` to try it in VS Code

The `debug!` macro (defined in `src/lib.rs`) prints token dumps and parser traces to stderr only in debug builds with `STONE_DEBUG` set (e.g. `STONE_DEBUG=1 cargo run -- run file.st`), and never under `cargo fuzz`.

## Workflow

- Practice TDD. Write the test first, run it and confirm it fails for the expected reason, then write the implementation that makes it pass.
- After every change, run `cargo fmt` and `cargo clippy`, and address what clippy reports.

## Architecture

The crate is a library (`src/lib.rs`) plus a thin CLI (`src/main.rs`, clap only). Pipelines are wired in `src/driver.rs`:

- **run** (`driver::interpret`): `Lexer` → `Parser` → `TypeChecker` → `Interpreter`
- **build** (`driver::compile`): `Lexer` → `Parser` → `TypeChecker` → `X64Generator::compile` (scan pass lowers to IR, generate pass allocates registers and emits, then gcc)
- **check** (`driver::check`): `Lexer` → `Parser` → `TypeChecker`, returning every `Diagnostic` instead of running. This is the entry point for editor tooling

Key pieces:

- `src/span.rs`: `Pos` (1-based line and col, counted in chars) and half-open `Span`. Every token and AST node carries one.
- `src/diagnostic.rs`: `Diagnostic` (severity, span, message) with `render` for the CLI, and `Diagnostics` for returning several as one error. Every stage's errors convert to it.
- `src/token.rs`: `Token`/`TokenType` and `RESERVED_KEYWORDS`. The lexer (`src/lexer.rs`) emits Python-style `Indent`/`Dedent`/`Newline` tokens.
- `src/ast.rs`: AST (`Mod`, `Stmt`, `Expr`, `Constant`, `ParserError`, the `Type` annotation enum, …), modeled on the ASDL in `docs/grammar/stone.asdl`. `Expr` and `Stmt` are structs of a `kind` (`ExprKind`/`StmtKind`, the ASDL variants) plus a `span`, so match on `&expr.kind`. Parser tests build expected trees with `.into()` and compare them through `without_spans`.
- `src/parser.rs`: hand-written recursive-descent PEG parser. Syntax errors come from the furthest token any rule failed at: `expect`, `expect_name`, and `record_expected` note what was wanted there, and "quiet" expectations (operators, keywords, and other optional continuations) only show when nothing else was expected. The struct, token helpers, and `parse()` entry are here; rules live in `src/parser/expressions.rs` and `src/parser/statements.rs` as `pub(super)` methods so siblings can call each other. Functions mirror rule names in `docs/grammar/stone.gram` (derived from CPython's grammar, kept as `python_grammar.gram` for reference), e.g. `parse_t_primary`, `parse_star_targets`, `*_loop0` for repetition. Keep the `.gram`/`.asdl` docs in sync when changing syntax.
- `src/checker.rs`: `TypeChecker::analyze` infers types (`int`, `float`, `bool`, `str`, `none`, `list[T]`, monomorphic functions) by unification, resolves names Python-style (a function's params and assigned names are local, other names are globals or top-level functions, which are visible before their definition), and returns an `Analysis`: sorted diagnostics, the `Type` of every expression keyed by span, and `Symbol`s with every `Reference`. It only accepts what both backends run identically, e.g. conditions must be `int` or `bool`, arithmetic never mixes `int` and `float` (convert with `int()` or `float()`), `==` cannot compare lists, `range` only appears as a `for` iterable, functions must be top level, and top-level code and functions must assign a variable on every path before reading it (`AssignmentCheck`).
- `src/interpreter.rs`: evaluates the AST directly over runtime `Value`s (lists are shared `Rc<RefCell<Vec>>`); `ControlFlow` carries break/continue/return, and a function sees only its own scope and the globals. `Limits` bounds fuel (loop iterations plus calls), total evaluation depth, and active calls (`max_calls`, the language's `stdlib::MAX_CALL_DEPTH`), and `print` writes to an injectable writer (`driver::interpret_with`). The driver runs it on a thread with `Limits::STACK_SIZE` of stack so 1,000 nested calls fit.
- Runtime errors must match between backends: the interpreter's error message is what compiled code prints after `error: ` (e.g. `division by zero`, `integer overflow in division`, `recursion is too deep (more than 1000 nested calls)`, `'x' is used before it is assigned`, `list index out of range`, `cannot convert float to int (nan or out of range)`), with exit status 1. Floats follow IEEE 754 except that dividing by 0.0 is `division by zero`, as in Python. Golden programs check them with `.err` files.
- Every stage must return an error, never panic, on bad input: `LexError` for bad literals, a `Diagnostic` from `Parser::parse`, `parser::MAX_DEPTH` for nesting, `Limits` in the interpreter, and `Err(String)` from codegen (`X64Generator::assemble` generates assembly without gcc).
- `src/codegen.rs`: `AssemblyGenerator` trait (`compile`/`scan`/`generate`/`emit`/`architecture`) plus assembler/linker discovery helpers.
- `src/codegen/ir.rs`: a small IR between the AST and assembly. A `Function` is blocks (in layout order) of `Inst`s over vregs, each ending in a `Terminator`; it is not SSA, so each stone local is one vreg for the whole function. `Display` prints a readable dump (`v1 = add v0, 1`, `br_lt v1, v0 -> b2, b3`), which tests compare and `STONE_DEBUG` prints.
- `src/codegen/ir/lower.rs`: `lower` turns a checked AST into a `Program` (every function, then `main`, plus the global names), resolving names like the checker (`checker::collect_assigned`). Evaluation is left to right, so runtime errors happen in the interpreter's order. `x = <expr>` writes `x`'s vreg directly. `if`/`while` tests go through `Lowerer::cond`, so comparisons, `and`, `or`, and `not` become `CmpBranch`/`Branch` terminators instead of materialized bools. A `for` loop counts with hidden vregs that the body cannot assign, and copies a variable bound or list first.
- `src/codegen/ir/liveness.rs`: `analyze` computes live-in/live-out sets, then one interval per vreg (no holes). Each block gets a start position, then each instruction an even position where it reads and the odd one after where it writes. Intervals carry a spill weight (`10^loop_depth` per access) and whether they cross a call.
- `src/codegen/regalloc.rs`: target-independent `linear_scan` (Poletto-Sarkar, spilling the lighter interval) with `Hint`s for preferred registers, and `parallel_moves`, which orders simultaneous moves and breaks cycles through a temporary.
- `src/codegen/x64.rs`: `X64Generator`, emitting GNU-as Intel syntax (`.intel_syntax noprefix`). `assemble` runs the checker first and lowers with its types, e.g. to pick a `print` routine per argument. Top-level statements always go in a synthesized `main`, globals live in `.bss` as `g.<name>`, and user functions are `fn.<name>`, so they cannot collide with libc. String literals are interned and emitted in `.rodata`. Runtime errors jump to a `fail_label(message)` stub that calls `stone.fail`.
- `src/codegen/x64/emit.rs`: instruction selection from allocated IR. Vregs that cross a call get callee-saved `rbx`/`r12`-`r15` (saved in the prologue only if used); others may also use `rsi`, `rdi`, `r8`-`r11`. `rax`, `rcx`, `rdx`, and `xmm0`-`xmm2` are never allocated and serve as scratch, for `idiv`, spilled operands, and wide immediates. `hints` asks for arguments to be computed in their argument registers, parameters to stay where they arrive, and copies and two-address results to share a register. The first six arguments go in registers via `parallel_moves`; arguments past the sixth are pushed, first deepest, and the callee reads them from `[rbp + 16 + ...]`. Each function counts itself in `stone.call_depth` against `MAX_CALL_DEPTH`, and each global has a `g.<name>.set` flag that `StoreGlobal` sets and that functions check before reading a global, since only top-level reads are checked statically. Floats travel as their bits in general registers and spill slots like every other value, and only move into `xmm` registers for SSE arithmetic, `ucomisd` comparisons, and conversions. Division checks for zero and `MIN / -1` before `idiv`, except that an immediate divisor other than 0 and -1 skips the checks, and a power of two becomes shifts (`divide_by_constant`). List indexing is inline (`list_slot`): it counts negative indexes from the end, checks the result against the list header's length, and jumps to a `fail_label` when out of range.
- Builtins: `BUILTINS` in `src/stdlib.rs` lists names shared by both backends (`print`, `len`, `range`, `append`, `int`, `float`), and `BUILTIN_DOCS` documents each for hover and the LSP's builtins reference. `src/codegen/x64/builtins.rs` emits the hand-written runtime via `&mut dyn AssemblyGenerator`: `stone.print_*` routines (emitted if the program prints; `stone.print_float` only if it prints a float, and it calls libc's `snprintf` and `strtod` to match `stdlib::format_float`, which prints floats like Python's `repr`), and the string and list runtimes (emitted if the program uses those types). Routines that call `malloc`/`realloc` realign the stack first, since generated code does not keep it aligned. Adding a builtin means updating `BUILTINS` and `BUILTIN_DOCS`, its type rule in `Inference::infer_builtin`, the interpreter's `Interpreter::call`, and its lowering in `Lowerer::call` (plus an IR instruction and its x64 emission if it is not a runtime call), plus a program in `tests/programs/`.

Language server (`lsp/`, crate `stone-lsp`, depends on `lsp-server`, `lsp-types`, `serde_json`):

- `src/lib.rs`: `run(connection)` does the initialize handshake (preferring UTF-8 positions, falling back to UTF-16), keeps open documents with full-text sync, publishes diagnostics on open/change/close, and dispatches requests through `Server::answer`.
- `src/features.rs`: one pure function per request (`diagnostics`, `hover`, `definition`, `references`, `prepare_rename`, `rename`, `document_symbols`, `completion`) over a `Document`, which holds the text, its `stone::driver::analyze` result, and a `LineIndex`. Builtins have no stone source, so `run` writes `stdlib::builtins_reference()` (comment-only, documenting each builtin from `stdlib::BUILTIN_DOCS`) to `<temp>/stone-lsp-<version>/builtins.st`, and `definition` of a builtin jumps to its line there (`Builtins`). Add a feature here with unit tests in `src/features/tests.rs`, then wire it into `Server::handle_request` and `capabilities`.
- `src/line_index.rs`: converts stone's 1-based char positions to LSP's 0-based UTF-16 or UTF-8 positions.
- Tests: unit tests for features and positions, `tests/server.rs` over an in-memory connection, and `tests/stdio.rs` against the real binary.
- `driver::analyze` recovers from syntax errors statement by statement, in blocks too (`Parser::parse_recovering`, `parse_statements`), so features keep working around a typo, and it hides type errors until the syntax errors are fixed.

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

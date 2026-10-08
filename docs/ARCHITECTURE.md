# stone Architecture

stone is a language that is both compiled and interpreted, depending on the developer's needs. Its features include:

- Semantic versioning in syntax (e.g., `import pkg@1.2.3`)
- AI-first design

## Compiler passes

1. Lower the checked AST to IR: blocks of instructions over virtual registers, one per local.
2. For each function, compute live intervals and assign registers by linear scan, spilling the least-used values to the stack.
3. Emit x86-64 or arm64 assembly from the allocated IR, then assemble and link it with gcc (or the target's cross gcc).

## Pipelines

`src/driver.rs` wires the stages together, and `src/main.rs` picks a pipeline from the command line.

- `stone run`: `project` (`lexer` -> `parser` for each file, then link) -> `checker` -> `interpreter`
- `stone build`: `project` -> `checker` -> `codegen::ir` (lower to IR) -> `codegen::regalloc` (linear scan) -> `codegen::x64` or `codegen::arm64` (emit, then gcc), picked by `--target`, which defaults to the host's `codegen::Architecture`
- `stone check`: `project` -> `checker`, printing every diagnostic without running

`project::link` lexes and parses the entry file, loads every module its `use` statements name, and
merges them into one module in which each library function is renamed to its module path, such
as `geometry.shapes.area`, and every use of it points there. Everything after it sees a program
of one file, except that each `Span` carries the `FileId` of the file it is in.

## Source map

| module | role |
| --- | --- |
| `span` | source positions (`Pos`), file ids (`FileId`), and ranges (`Span`) carried by tokens and AST nodes |
| `diagnostic` | errors and warnings with a span, and their terminal rendering |
| `token` | `Token`, `TokenType`, and reserved keywords |
| `lexer` | source text to tokens, including Python-style `Indent`/`Dedent` |
| `ast` | syntax tree nodes, modeled on `docs/grammar/stone.asdl` |
| `parser` | recursive-descent PEG parser; `parser/expressions.rs` and `parser/statements.rs` mirror the rules in `docs/grammar/stone.gram` |
| `checker` | type inference and name resolution, producing diagnostics, expression types, and symbols with their references |
| `interpreter` | tree-walking evaluator |
| `codegen` | the `AssemblyGenerator` trait, `Architecture` (the `--target` names and each one's linker), and `link`, which runs gcc |
| `codegen/context` | what both backends share about the program being compiled: the checked and lowered program, labels, interned strings, runtime failures, and which runtime routines it needs |
| `codegen/ir` | the IR the backend compiles through: lowering from the AST (`codegen/ir/lower.rs`) and liveness intervals (`codegen/ir/liveness.rs`) |
| `codegen/regalloc` | target-independent linear-scan register allocation and parallel-move ordering |
| `codegen/x64` | the x86-64 backend: instruction selection from allocated IR (`codegen/x64/emit.rs`) and its hand-written builtins (`codegen/x64/builtins.rs`) |
| `codegen/arm64` | the arm64 backend, with the same split (`codegen/arm64/emit.rs` and `codegen/arm64/builtins.rs`) and the same runtime routines under the same labels |
| `stdlib` | names of the builtins shared by both backends (`print`, `len`, `range`, `append`, `int`, and `float`), and `format_float`, which defines how both print floats |
| `project` | loading the modules a program uses (`Sources`), checking `use` and `pub`, linking them into one module, and `SourceMap` for rendering diagnostics from any file |
| `driver` | the run/build/check pipelines, and `analyze_linked` for editor tooling |

## Language server

`lsp/` is the `stone-lsp` crate, a language server built on `lsp-server` and `lsp-types`, kept out of the main crate so stone itself still depends only on `clap`. On every change it reanalyzes each open document as part of its program (the one whose entry file is the nearest `main.st` above it) with `project::link` and `driver::analyze_linked`, reading open documents' text over what is on disk, and answers requests from the resulting `checker::Analysis`: its diagnostics, the type of every expression, and every symbol with all of its references, in every file.

| request | answered from |
| --- | --- |
| diagnostics | `Analysis::diagnostics`, with a syntax error for every statement that fails to parse, in blocks too |
| hover | the symbol's signature, a builtin's documentation, or the innermost expression's type |
| definition, references, rename | `Analysis::reference_at` and `Analysis::references_to`, across files; a `use` path's definition is the module or function it names, and a builtin's is its line in a generated `builtins.st` reference |
| document symbols | globals and functions, with each function's parameters and locals |
| completion | `Analysis::visible_at`, names bound by `use`, builtins, and keywords; a module's `pub` functions after its name and a `.`; modules and directories in a `use` |

`editors/vscode/` is a VS Code extension that provides highlighting and indentation rules and starts `stone-lsp` for `.st` files.

## Fuzzing

The `fuzz/` crate uses cargo-fuzz (libFuzzer, nightly Rust). It is a separate crate, so the main crate still depends only on `clap`.

| target | input | checks |
| --- | --- | --- |
| `lex`, `parse` | arbitrary text | the front end returns tokens, a module, or an error, and never panics, overflows the stack, or takes exponential time |
| `interpret` | arbitrary text | the interpreter finishes under `fuzz::LIMITS` without panicking |
| `codegen` | arbitrary text that parses | both backends' `assemble` succeeds or returns an error, without gcc |
| `structured` | programs from `stone_fuzz::generate` | the same stages on deep, valid programs, for both backends |
| `differential` | programs from `stone_fuzz::generate` | `stone run` and `stone build` print the same output |

`stone_fuzz::generate` writes source text rule by rule from the grammar, tracking scope so every name and call is defined and every program passes the checker. It covers int arithmetic, comparisons, functions with up to eight parameters, `if`/`while`/`for` with `break` and `cont`, multi-argument `print` with strings and booleans, and top-level lists. Every loop is bounded, and functions never read globals, which may not be assigned yet when they run. `differential` skips a program if the interpreter rejects it (out of fuel, division by zero, an unassigned variable).

Seeds for the text targets are the `.st` programs in `fuzz/corpus/<target>/`. `fuzz/stone.dict` lists stone's tokens.

## Benchmarks

The `bench/` crate compares compiled stone with C (`gcc -O2` and `gcc -O0`) and Python. It is a separate crate, like `fuzz/`, and depends on `stone` only for its tests. Each benchmark in `bench/programs/` is written three times, as `.st`, `.c`, and `.py`, and the runner refuses to time a program unless all four builds print the same `.out` file. See `bench/README.md` for the programs and a baseline.

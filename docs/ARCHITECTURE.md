# stone Architecture

stone is a language that is both compiled and interpreted, depending on the developer's needs. Its features include:

- Semantic versioning in syntax (e.g., `import pkg@1.2.3`)
- AI-first design

## Compiler passes

1. Run through AST and collect all stack sizes per closure.
2. Generate source code.

## Pipelines

`src/driver.rs` wires the stages together, and `src/main.rs` picks a pipeline from the command line.

- `stone run`: `lexer` -> `parser` -> `checker` -> `interpreter`
- `stone build`: `lexer` -> `parser` -> `checker` -> `codegen::x64` (scan, generate, then gcc)
- `stone check`: `lexer` -> `parser` -> `checker`, printing every diagnostic without running

## Source map

| module | role |
| --- | --- |
| `span` | source positions (`Pos`) and ranges (`Span`) carried by tokens and AST nodes |
| `diagnostic` | errors and warnings with a span, and their terminal rendering |
| `token` | `Token`, `TokenType`, and reserved keywords |
| `lexer` | source text to tokens, including Python-style `Indent`/`Dedent` |
| `ast` | syntax tree nodes, modeled on `docs/grammar/stone.asdl` |
| `parser` | recursive-descent PEG parser; `parser/expressions.rs` and `parser/statements.rs` mirror the rules in `docs/grammar/stone.gram` |
| `checker` | type inference and name resolution, producing diagnostics, expression types, and symbols with their references |
| `interpreter` | tree-walking evaluator |
| `codegen` | the `AssemblyGenerator` trait and toolchain discovery |
| `codegen/x64` | the x86-64 backend and its hand-written builtins (`codegen/x64/builtins.rs`) |
| `stdlib` | names of the builtins shared by both backends: `print`, `len`, `range`, and `append` |
| `driver` | the run/build/check pipelines, and `analyze` for editor tooling |

## Language server

`lsp/` is the `stone-lsp` crate, a language server built on `lsp-server` and `lsp-types`, kept out of the main crate so stone itself still depends only on `clap`. It reanalyzes a document on every change with `driver::analyze` and answers requests from the resulting `checker::Analysis`: its diagnostics, the type of every expression, and every symbol with all of its references.

| request | answered from |
| --- | --- |
| diagnostics | `Analysis::diagnostics`, with syntax errors from every top-level statement |
| hover | the symbol's signature, a builtin's documentation, or the innermost expression's type |
| definition, references, rename | `Analysis::reference_at` and `Analysis::references_to` |
| document symbols | globals and functions, with each function's parameters and locals |
| completion | `Analysis::visible_at`, builtins, and keywords |

`editors/vscode/` is a VS Code extension that provides highlighting and indentation rules and starts `stone-lsp` for `.st` files.

## Fuzzing

The `fuzz/` crate uses cargo-fuzz (libFuzzer, nightly Rust). It is a separate crate, so the main crate still depends only on `clap`.

| target | input | checks |
| --- | --- | --- |
| `lex`, `parse` | arbitrary text | the front end returns tokens, a module, or an error, and never panics, overflows the stack, or takes exponential time |
| `interpret` | arbitrary text | the interpreter finishes under `fuzz::LIMITS` without panicking |
| `codegen` | arbitrary text that parses | `X64Generator::assemble` succeeds or returns an error, without gcc |
| `structured` | programs from `stone_fuzz::generate` | the same stages on deep, valid programs |
| `differential` | programs from `stone_fuzz::generate` | `stone run` and `stone build` print the same output |

`stone_fuzz::generate` writes source text rule by rule from the grammar, tracking scope so every name and call is defined and every program passes the checker. It covers int arithmetic, comparisons, functions with up to eight parameters, `if`/`while`/`for` with `break` and `cont`, multi-argument `print` with strings and booleans, and top-level lists. Every loop is bounded, and functions never read globals, which may not be assigned yet when they run. `differential` skips a program if the interpreter rejects it (out of fuel, division by zero, an unassigned variable).

Seeds for the text targets are the `.st` programs in `fuzz/corpus/<target>/`. `fuzz/stone.dict` lists stone's tokens.

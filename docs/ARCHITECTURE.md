# stone Architecture

stone is a language that is both compiled and interpreted, depending on the developer's needs. Its features include:

- Semantic versioning in syntax (e.g., `import pkg@1.2.3`)
- AI-first design

## Compiler passes

1. Run through AST and collect all stack sizes per closure.
2. Generate source code.

## Pipelines

`src/driver.rs` wires the stages together, and `src/main.rs` picks a pipeline from the command line.

- `stone run`: `lexer` -> `parser` -> `checker` (a no-op for now) -> `interpreter`
- `stone build`: `lexer` -> `parser` -> `codegen::x64` (scan, generate, then gcc)

## Source map

| module | role |
| --- | --- |
| `token` | `Token`, `TokenType`, and reserved keywords |
| `lexer` | source text to tokens, including Python-style `Indent`/`Dedent` |
| `ast` | syntax tree nodes, modeled on `docs/grammar/stone.asdl` |
| `parser` | recursive-descent PEG parser; `parser/expressions.rs` and `parser/statements.rs` mirror the rules in `docs/grammar/stone.gram` |
| `checker` | type checker (accepts everything for now) |
| `interpreter` | tree-walking evaluator |
| `codegen` | the `AssemblyGenerator` trait and toolchain discovery |
| `codegen/x64` | the x86-64 backend and its hand-written builtins (`codegen/x64/builtins.rs`) |
| `stdlib` | names of the builtins shared by both backends |
| `driver` | the run/build pipelines |

## Fuzzing

The `fuzz/` crate uses cargo-fuzz (libFuzzer, nightly Rust). It is a separate crate, so the main crate still depends only on `clap`.

| target | input | checks |
| --- | --- | --- |
| `lex`, `parse` | arbitrary text | the front end returns tokens, a module, or an error, and never panics, overflows the stack, or takes exponential time |
| `interpret` | arbitrary text | the interpreter finishes under `fuzz::LIMITS` without panicking |
| `codegen` | arbitrary text that parses | `X64Generator::assemble` succeeds or returns an error, without gcc |
| `structured` | programs from `stone_fuzz::generate` | the same stages on deep, valid programs |
| `differential` | programs from `stone_fuzz::generate` | `stone run` and `stone build` print the same output |

`stone_fuzz::generate` writes source text rule by rule from the grammar, tracking scope so every name and call is defined. It stays inside the subset where both backends agree. That means no strings, booleans, or nested functions, one argument per `print`, no globals read from functions, and every `while` bounded by a counter. `differential` skips a program if the interpreter rejects it (out of fuel, division by zero) or if it prints anything outside `0..=4095`, because compiled `print` treats larger or negative values as string pointers.

Seeds for the text targets are the `.st` programs in `fuzz/corpus/<target>/`. `fuzz/stone.dict` lists stone's tokens.

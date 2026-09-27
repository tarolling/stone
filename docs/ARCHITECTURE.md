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


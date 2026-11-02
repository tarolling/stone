# stone Architecture

## Compiler passes

1. Lower the checked AST to IR: blocks of instructions over virtual registers, one per local.
2. Inline small functions that call no others, then optimize each function: hoist and reuse index and uniqueness checks, rotate loops, and drop dead instructions.
3. For each function, compute live intervals and assign registers by linear scan, spilling the least-used values to the stack.
4. Emit x86-64 or arm64 assembly from the allocated IR, then assemble it with the built-in assembler and link it with the built-in linker into a static ELF executable for Linux, a signed Mach-O executable for macOS, or a PE executable for Windows.

## Pipelines

`src/driver.rs` wires the stages together, and `src/main.rs` picks a pipeline from the command line.

- `stone run`: `project` (`lexer` -> `parser` for each file, then link) -> `checker` -> `interpreter`
- `stone build`: `project` -> `checker` -> `codegen::ir` (lower to IR, then `ir::inline` and `ir::opt`) -> `codegen::regalloc` (linear scan) -> `codegen::x64` or `codegen::arm64` (emit), picked by `--target`, which defaults to the host, -> `codegen::asm` (assemble) -> `codegen::elf`, `codegen::macho`, or `codegen::pe` (link), by the target's `Os`
- `stone check`: `project` -> `checker` -> `unused`, printing every diagnostic and unused-name warning without running
- `stone fmt`: `format`, a token formatter that keeps comments
- the REPL: `repl`, which checks each entry together with the earlier ones and runs it on one `interpreter`
- `stone add`, `remove`, `fetch`, and `tree`: `deps`, over `package`

`project::link` lexes and parses the entry file, loads every module its `use` statements name, and
merges them into one module in which each library function is renamed to its module path, such
as `geometry.shapes.area`, and every use of it points there. It also parses `src/prelude.stn`,
which defines `option`, `result`, and the prelude's traits, into every program, adds the source
of `net` or `gui` when a program uses them, and turns each lambda into a function of its own
(`lift`). Everything after it sees a program
of one file, except that each `Span` carries the `FileId` of the file it is in.

The functions of a struct's `impl` blocks are named by the struct's linked name and their own,
as `geometry.Point.shift`. Which one a call such as `p.shift(1.0)` calls depends on the type of
`p`, so the checker resolves every such call and records it, by the call's span, in
`Analysis::methods`, which both the interpreter and lowering follow.

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
| `lift` | turns lambdas into functions, and those that use outside variables into closures holding copies |
| `last_use` | where each variable is used for the last time, so both backends move values there |
| `unused` | warnings about names a program defines and never uses, for `stone check` and the language server |
| `interpreter` | tree-walking evaluator, over a tree made of each body once with variables resolved to slots |
| `format` | `stone fmt`, a formatter over tokens that keeps comments |
| `repl` | the interactive session |
| `codegen` | the `AssemblyGenerator` trait, `Target` (the `--target` names: an `Architecture`, a `Level`, and an `Os`), and `link`, which writes the `.s` file and the executable |
| `codegen/device` | `Device`, the GPUs (`gfx1100`, `sm_90`) that code will run on beside a target; no backend generates GPU code yet |
| `codegen/asm` | the built-in assembler: parses the GNU as syntax both backends emit and encodes it (`codegen/asm/x64.rs`, `codegen/asm/arm64.rs`), picking the same encodings as GNU as, with jumps relaxed to their short form where they reach |
| `codegen/elf` | the built-in linker for Linux: lays out sections, resolves fixups, and writes a static ELF executable with a symbol table |
| `codegen/macho` | the built-in linker for macOS: a Mach-O executable that loads only `libSystem`, with an ad hoc code signature (`codegen/sha256`) |
| `codegen/pe` | the built-in linker for Windows: a PE32+ executable that imports only from the system's DLLs |
| `codegen/context` | what both backends share about the program being compiled: the checked and lowered program, labels, interned strings, runtime failures, and which runtime routines it needs |
| `codegen/ir` | the IR the backend compiles through: lowering from the AST (`codegen/ir/lower.rs`), the trigonometric functions expanded inline (`codegen/ir/trig.rs`), inlining (`codegen/ir/inline.rs`), optimization passes (`codegen/ir/opt.rs`), and liveness intervals (`codegen/ir/liveness.rs`) |
| `codegen/regalloc` | target-independent linear-scan register allocation and parallel-move ordering |
| `codegen/x64` | the x86-64 backend: instruction selection from allocated IR (`codegen/x64/emit.rs`), its hand-written runtime (`codegen/x64/builtins.rs` and the files beside it), and the system calls, which are Linux syscalls or, on Windows, shims over `kernel32.dll` (`codegen/x64/system.rs`, `codegen/x64/windows.rs`) |
| `codegen/arm64` | the arm64 backend, with the same split (`codegen/arm64/emit.rs` and `codegen/arm64/builtins.rs`) and the same runtime routines under the same labels, making Linux syscalls or calling `libSystem` on macOS |
| `stdlib` | the builtins shared by both backends (`BUILTINS`: `print`, `eprint`, `range`, `int`, `float`, `str`, `bytes`, `input`, `eof`, and `args`), their methods, and the documentation of each; the builtin modules (`os`, `math`, `random`, `time`, `fs`, and the stone sources of `net` and `gui`), with the functions the interpreter calls; the prelude's source; and `format_float`, which defines how both print floats |
| `project` | loading the modules a program uses (`Sources`), checking `use` and `pub`, linking them into one module, and `SourceMap` for rendering diagnostics from any file |
| `package` | packages: reading `stone.toml`, choosing versions, the store of downloads, `stone.lock`, and, with the `fetch` feature, downloading releases |
| `deps` | the `stone add`, `remove`, `fetch`, and `tree` commands |
| `update`, `uninstall` | `stone update` (with the `self-update` feature) and `stone uninstall` |
| `driver` | the run/build/check pipelines, and `analyze_linked` for editor tooling |

## Language server

`lsp/` is the `stone-lsp` crate, a language server built on `lsp-server` and `lsp-types`, kept out of the main crate so stone itself still depends only on `clap`. On every change it reanalyzes each open document as part of its program (the one whose entry file is the nearest `main.stn` above it, or the `lib.stn` beside the nearest `stone.toml`) with `project::link` and `driver::analyze_linked`, reading open documents' text over what is on disk, and answers requests from the resulting `checker::Analysis`: its diagnostics, the type of every expression, and every symbol with all of its references, in every file.

| request | answered from |
| --- | --- |
| diagnostics | `Analysis::diagnostics`, with a syntax error for every statement that fails to parse, in blocks too |
| hover | the symbol's signature, a builtin's documentation, or the innermost expression's type |
| definition, references, rename | `Analysis::reference_at` and `Analysis::references_to`, across files; a `use` path's definition is the module or function it names, and a builtin's is its line in a generated `builtins.stn` reference |
| document symbols | globals and functions, with each function's parameters and locals |
| completion | `Analysis::visible_at`, names bound by `use`, builtins, and keywords; a module's `pub` functions after its name and a `.`; a value's fields and methods after a `.`; types where a type goes; modules, directories, and packages in a `use` |
| formatting | `format::format`, as one edit of the whole document |

`editors/vscode/` is a VS Code extension that provides highlighting and indentation rules and starts `stone-lsp` for `.stn` files.

## Fuzzing

The `fuzz/` crate uses cargo-fuzz (libFuzzer, nightly Rust). It is a separate crate, so the main crate still depends only on `clap`.

| target | input | checks |
| --- | --- | --- |
| `lex`, `parse` | arbitrary text | the front end returns tokens, a module, or an error, and never panics, overflows the stack, or takes exponential time |
| `format` | arbitrary text | formatting returns an error or text that formatting again leaves unchanged |
| `interpret` | arbitrary text | the interpreter finishes under `fuzz::LIMITS` without panicking |
| `codegen` | arbitrary text that parses | both backends' `assemble` succeeds or returns an error, and the assembly it returns assembles and links |
| `structured` | programs from `stone_fuzz::generate` | the same stages on deep, valid programs, for both backends |
| `differential` | programs from `stone_fuzz::generate` | `stone run` and `stone build` print the same output |

`stone_fuzz::generate` writes source text rule by rule from the grammar, tracking scope so every name and call is defined and every program passes the checker. It covers int and float arithmetic, comparisons, functions with up to eight parameters, `if`/`while`/`for` with `break` and `cont`, multi-argument `print`, strings, lists, and, through `fuzz/src/compound.rs`, structs with `impl` blocks, enums, options, maps, and lists of them. Every loop is bounded, and functions never read globals, which may not be assigned yet when they run. `differential` skips a program if the interpreter rejects it (out of fuel, division by zero, an unassigned variable).

Seeds for the text targets are the `.stn` programs in `fuzz/corpus/<target>/`. `fuzz/stone.dict` lists stone's tokens.

## Benchmarks

The `bench/` crate compares compiled stone with C (`gcc -O2` and `gcc -O0`) and Python. It is a separate crate, like `fuzz/`, and depends on `stone` only for its tests. Each benchmark in `bench/programs/` is written three times, as `.stn`, `.c`, and `.py`, which all read the workload size from the arguments in a `.args` file, and the runner refuses to time a program unless all four builds print the same `.out` file. See `bench/README.md` for the programs and a baseline.

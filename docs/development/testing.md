# Testing

```sh
cargo test                  # everything in the stone and stone-lsp crates
cargo test simple_functions # tests whose name contains the filter
```

## Golden programs

`tests/programs.rs` runs every `.st` file in `examples/`, `tests/programs/`, and
`docs/examples/` through both `stone run` and `stone build`, and compares stdout with the
sibling `.out` file. This is the main guarantee that the interpreter and the compiler agree.

`stone build` compiles for the processor the tests run on. The compiler for the other one is
tested too, by building each program with `--target` and running it under qemu-user, when its
cross compiler and emulator are on `PATH`: `aarch64-linux-gnu-gcc` and `qemu-aarch64` on an
x86-64 machine, or `x86_64-linux-gnu-gcc` and `qemu-x86_64` on an arm64 one. Otherwise that pass
is skipped with a note. Compiled programs are static and need no libc, so qemu needs nothing
else. CI runs both backends: the x86-64 job installs the aarch64 cross tools, and an arm64 job
runs the suite natively.

To cover new behavior, add `name.st` and the exact output it should print as `name.out`:

```text
tests/programs/
  strings.st
  strings.out
```

A program that should fail at runtime also gets a `name.err` file holding text its stderr must
contain, such as `division by zero`. It must then exit with an error under both backends, after
printing its `.out`.

A program of several files is a directory instead, run from its `main.st`, with the `.out` (and
any `.err`, `.in`, or `.args`) next to `main.st`:

```text
tests/programs/
  module_cycle/
    main.st
    main.out
    parity/
      even.st
      odd.st
```

Both backends must agree on everything the checker accepts, so `SKIPS` in `tests/programs.rs`
should stay empty. If they disagree, either fix the backend or make the checker reject the
program.

## Unit tests

Unit tests live in `#[cfg(test)]` modules next to the code:

| file | covers |
| --- | --- |
| `src/lexer.rs` | tokens, indentation, literals |
| `src/parser/tests.rs` | syntax trees and syntax errors |
| `src/checker/tests.rs` | inferred types and type errors |
| `src/codegen/ir/lower/tests.rs` | IR produced from the AST |
| `src/codegen/ir/liveness.rs` | live intervals |
| `src/codegen/regalloc.rs` | linear scan and parallel moves |
| `src/codegen.rs` | target names and linker selection |
| `src/codegen/x64.rs` | generated x86-64 assembly |
| `src/codegen/arm64.rs` | generated arm64 assembly |
| `src/driver.rs` | the run, build, and check pipelines |
| `src/update.rs` | self-update (run with `--features self-update`) |

Parser tests build expected trees with `.into()` and compare them through `without_spans`.

## Other test suites

- `tests/cli.rs` runs the `stone` binary to check error output and exit codes.
- `tests/docs.rs` checks that this documentation covers every builtin and keyword.
- `cargo test -p stone-lsp` runs the language server's tests. See {doc}`language-server`.
- `cargo test --manifest-path fuzz/Cargo.toml` checks the fuzzer's program generator. See
  {doc}`fuzzing`.
- `cargo test --manifest-path bench/Cargo.toml` checks the benchmark runner. See
  {doc}`benchmarks`.

## Errors, never panics

Every stage must return an error rather than panic on bad input: `LexError` for bad literals, a
`Diagnostic` from the parser, `parser::MAX_DEPTH` for deep nesting, `Limits` in the interpreter,
and `Err(String)` from code generation. The fuzzers enforce this.

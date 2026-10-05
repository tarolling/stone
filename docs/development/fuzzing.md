# Fuzzing

The `fuzz/` crate uses [cargo-fuzz](https://github.com/rust-fuzz/cargo-fuzz) (libFuzzer), which
needs nightly Rust:

```sh
rustup toolchain install nightly
cargo install cargo-fuzz
cargo +nightly fuzz run parse -- -dict=fuzz/stone.dict
```

## Targets

| target | input | checks |
| --- | --- | --- |
| `lex`, `parse` | arbitrary text | the front end returns tokens, a module, or an error, and never panics, overflows the stack, or takes exponential time |
| `interpret` | arbitrary text | the interpreter finishes under `fuzz::LIMITS` without panicking |
| `codegen` | arbitrary text that parses | `X64Generator::assemble` succeeds or returns an error, without gcc |
| `structured` | generated programs | the same stages on deep, valid programs |
| `differential` | generated programs | `stone run` and `stone build` print the same output |

`stone_fuzz::generate` writes valid programs rule by rule from the grammar, tracking scope so
every name and call is defined and every program passes the checker. Seeds for the text targets
are the `.st` programs in `fuzz/corpus/<target>/`, and `fuzz/stone.dict` lists stone's tokens.

`cargo test --manifest-path fuzz/Cargo.toml` checks the generator on stable Rust, including
that generated programs pass the checker and that 150 of them print the same under both
backends (this needs gcc).

## When a fuzzer finds a crash

The failing input is saved under `fuzz/artifacts/<target>/`. Reproduce it with

```sh
cargo +nightly fuzz run parse fuzz/artifacts/parse/crash-<hash>
```

then fix the bug and turn the input into a regression test: a unit test, or a program in
`tests/programs/` with its `.out` (and `.err` if it should fail at runtime).

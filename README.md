# stone

stone is a language. it has a built-in interpreter and compiler so you can choose to wait for the program to run or to build.

it is also optimized for developers to press the shift key as little as possible.

## usage

```sh
cargo run --release -- run examples/basics.st                  # interpret
cargo run --release -- build examples/basics.st -o build/basics # compile (needs gcc)
./build/basics
```

`stone <file>` is shorthand for `stone run <file>`. `build` writes the assembly next to the executable (`build/basics.s`) and defaults to `build/out`.

## layout

| path | contents |
| --- | --- |
| `src/` | the `stone` library (lexer, parser, interpreter, codegen) and the CLI in `main.rs` |
| `examples/` | sample programs, each with the output it should print in a `.out` file |
| `tests/programs/` | small programs that each cover one feature, also with `.out` files |
| `tests/programs.rs` | runs every program above through both backends and compares output |
| `docs/` | architecture notes and the grammar (`stone.gram`, `stone.asdl`) |
| `bench/` | benchmarks comparing `stone build` with C and Python (see `bench/README.md`) |

## testing

```sh
cargo test
```

To add a golden test, drop `name.st` and `name.out` into `tests/programs/` (or `examples/`). If a backend can't handle a program yet, list it in `SKIPS` in `tests/programs.rs` and give the reason.

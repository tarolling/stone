<p align="center"><img src="docs/_static/logo.svg" alt="stone logo" width="128"></p>

# stone

stone is a language. it has a built-in interpreter and compiler so you can choose to wait for the program to run or to build.

it aims to be as simple to write as python and as fast and memory-safe as rust, with error messages as clear as rust's, by moving the complexity into the compiler. compiled programs are meant to need nothing from the system, not even libc. see [philosophy](docs/philosophy.md).

the [documentation](https://tarolling.github.io/stone/) has a tutorial, a language reference, and development guides.

## install

```sh
curl -fsSL https://raw.githubusercontent.com/tarolling/stone/main/install.sh | sh
```

this downloads a prebuilt `stone` for linux or macos (x86-64 or arm64) into `~/.local/bin`. pick another directory or release with `sh -s -- --dir /usr/local/bin --version v0.1.0`, or set `STONE_INSTALL_DIR` and `STONE_VERSION`.

to upgrade later, run `stone update` (or `stone update --check` to see if there is a new release). to remove it, run `stone uninstall`.

`stone run` and `stone check` work everywhere. `stone build` writes executables itself, with no assembler, linker, or C compiler installed: static x86-64 or arm64 linux programs, or arm64 macos programs that need only the system's `libSystem`. it builds for the machine it runs on, and `--target` (`x86_64`, `aarch64`, or `aarch64-macos`) builds for any of them from any machine.

to build from source instead, with rust installed:

```sh
cargo install --git https://github.com/tarolling/stone stone
```

## usage

```sh
cargo run --release -- run examples/basics.st                  # interpret
cargo run --release -- build examples/basics.st -o build/basics # compile
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

<p align="center"><img src="docs/_static/logo.svg" alt="stone logo" width="128"></p>

# stone

stone is a language. it has a built-in interpreter and compiler so you can choose to wait for the program to run or to build.

it aims to be as simple to write as python and as fast and memory-safe as rust, with error messages as clear as rust's, by moving the complexity into the compiler. compiled programs are meant to need nothing from the system, not even libc. see [philosophy](docs/philosophy.md).

the [documentation](https://tarolling.github.io/stone/) has a tutorial, a language reference, and development guides.

## install

```sh
curl -fsSL https://raw.githubusercontent.com/tarolling/stone/main/install.sh | sh
```

this downloads a prebuilt `stone` for linux or macos (x86-64 or arm64) into `~/.local/bin`. pick another directory or release with `sh -s -- --dir /usr/local/bin --version v0.2.0`, or set `STONE_INSTALL_DIR` and `STONE_VERSION`.

on x86-64 windows, in powershell:

```powershell
irm https://raw.githubusercontent.com/tarolling/stone/main/install.ps1 | iex
```

to upgrade later, run `stone update` (or `stone update --check` to see if there is a new release). to remove it, run `stone uninstall`.

`stone run` and `stone check` work everywhere. `stone build` writes executables itself, with no assembler, linker, or C compiler installed: static x86-64 or arm64 linux programs, arm64 macos programs that need only the system's `libSystem`, or x86-64 windows programs that need only `kernel32.dll` (without the `net` and `gui` modules or `os.run` for now). it builds for the machine it runs on, and `--target` (`x86_64-linux`, `aarch64-linux`, `aarch64-macos`, or `x86_64-windows`) builds for any of them from any machine.

to build from source instead, with rust installed:

```sh
cargo install --git https://github.com/tarolling/stone stone
```

## usage

```sh
cargo run --release -- run examples/basics.stn    # interpret
cargo run --release -- build examples/basics.stn  # compile
./examples/build/basics
```

`stone <file>` is shorthand for `stone run <file>`. `build` writes the executable to a `build` directory next to the file, named after it (a `main.stn` is named after its directory), with the assembly beside it (`examples/build/basics.s`). other targets go in `build/<target>/`, `-o` picks a path instead, and `stone clean` removes the directory. `stone fmt` rewrites files in the standard layout, and `stone fmt --check` only reports the ones that need it.

## coming from python

if you use a coding agent, give it [`docs/agents/python-to-stone.md`](https://raw.githubusercontent.com/tarolling/stone/main/docs/agents/python-to-stone.md) to convert your python code to stone. it covers the process, how each python idiom translates, and the differences that would change your program's output without an error. to have an agent write new stone code instead, give it [`docs/agents/stone.md`](https://raw.githubusercontent.com/tarolling/stone/main/docs/agents/stone.md), which teaches the language.

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

To add a golden test, drop `name.stn` and `name.out` into `tests/programs/` (or `examples/`). Every program runs under both `stone run` and `stone build`, which must print the same, so `SKIPS` in `tests/programs.rs` stays empty. See [CONTRIBUTING.md](CONTRIBUTING.md) for how to send a change.

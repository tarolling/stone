# Command line

```text
stone [FILE [ARGS...]] [COMMAND]
```

Every command reads one `.stn` file, the entry file, along with every module it uses (see
[Modules](modules.md)), then checks them before doing anything else. If the checker
finds errors, the command prints all of them and exits with status 1 without running anything.
Errors print as `file:line:col: error: message`, followed by the source line and a caret under
the problem.

## `stone run FILE [ARGS...]`

Interprets the program. `stone FILE` is shorthand for `stone run FILE`. Everything after the
file is passed to the program, which reads it with `args()`, even arguments that start with `-`.
The program's standard input is stone's own, for `input()` and `eof()`.

```sh
stone run examples/basics.stn
stone run sum.stn 1 2 3 < numbers.txt
```

## `stone build FILE... [-o OUTPUT] [--target TARGET]...`

Compiles each program to a native executable in its build directory (see
[Where executables go](#where-executables-go)), writing the assembly next to it with a `.s`
extension. stone assembles and links
the program itself, so no assembler, linker, or C compiler needs to be installed. A Linux
executable is a static program that needs no C library, so it runs on any Linux machine with
the same processor. A macOS executable calls the system only through `libSystem`, which every
Mac has, and is signed, as macOS requires, so it runs on any Apple silicon Mac with macOS 12 or
later. Either has a symbol table, so tools such as `gdb`, `lldb`, `perf`, and `objdump` show
its functions by name, such as `fn.area` for a stone function `area` and `main` for the
top-level code.

The executable is for the machine stone runs on: x86-64 or arm64 Linux, or arm64 macOS.
`--target` picks one explicitly, and works the same from any machine. Give it more than once to
build for several targets in one run:

| target | runs on |
| --- | --- |
| `x86_64-linux` (or `x86_64`, `x64`) | x86-64 Linux |
| `aarch64-linux` (or `aarch64`, `arm64`) | arm64 Linux |
| `aarch64-macos` (or `arm64-macos`) | arm64 macOS |

A target is named `<arch>[<level>]-<system>`. There is no vendor or C library part, since
compiled programs use no C library, but Rust's names for the same machines work too, such as
`x86_64-unknown-linux-musl` and `aarch64-apple-darwin`. A system left out means Linux.

The optional level names the processor features the program may assume: `v1` to `v4` for
x86-64 (the x86-64 psABI levels, so `x86_64v3-linux` assumes AVX2), and `v8`, `v8.1` to `v8.9`,
`v9`, or `v9.1` to `v9.5` for arm64 (as in `aarch64v8.2-macos`). Without one, the program runs on
every processor of the architecture. stone does not use the extra features yet, so for now every
level builds the same program.

```sh
stone build examples/basics.stn
./examples/build/basics
stone build examples/basics.stn --target aarch64-linux --target aarch64-macos
```

The release archives use the same names for the machines the `stone` binary itself runs on,
such as `stone-x86_64-linux.tar.gz`, plus `stone-x86_64-macos.tar.gz` for Intel Macs, where stone
runs programs but cannot build them.

The executable reads its own arguments and standard input the same way, so `./build/sum 1 2 3`
behaves like `stone run sum.stn 1 2 3`.

### Where executables go

Like Cargo's `target` directory, each program has a `build` directory next to its entry file,
the same directory its modules are found from, so the result does not depend on where you run
stone:

```text
examples/
  basics.stn
  build/
    basics              built for this machine
    basics.s
    aarch64-macos/
      basics            built with --target aarch64-macos
      basics.s
```

- An executable is named after its entry file, so `basics.stn` becomes `basics`. A `main.stn` is
  named after its directory instead, as a program of several files usually is, so
  `geometry/main.stn` becomes `geometry/build/geometry`.
- An executable for the machine stone runs on goes straight into `build/`, and one for any
  other target into `build/TARGET/`, named as `--target` names it (`aarch64-macos`,
  `x86_64v3-linux`). Builds for different targets never overwrite each other.
- Several files, as in `stone build a.stn b.stn`, each build into their own executable. If one
  has errors, stone prints them, still builds the rest, and exits with status 1.
- `-o OUTPUT` writes the executable to exactly `OUTPUT` instead, with the assembly at
  `OUTPUT.s`, so it needs exactly one file and one target.

stone marks each build directory it makes with a `.stone` file, which `stone clean` looks for.

## `stone clean [PATH]`

Removes the build directory of the program in `PATH`, which is an entry file or a directory
and defaults to the current directory. It only removes a `build` directory that `stone build`
made; any other is an error and stays as it is.

```sh
stone clean examples/basics.stn   # removes examples/build
stone clean                      # removes ./build
```

## `stone check FILE`

Reports every syntax and type error without running the program, and exits with status 1 if
there are any. Warnings alone print but leave the status at 0.

```sh
stone check examples/basics.stn
```

## `stone fmt [PATH...] [--check]`

Rewrites files in stone's one standard layout. A `PATH` is a file, or a directory, in which
every `.stn` file below it is formatted, skipping hidden directories and build directories
that `stone build` made; with no `PATH`, it formats the current directory. `-` formats
standard input and writes the result to standard output.

Formatting only changes whitespace, never the code itself:

- Each block is indented by four spaces, and tabs become spaces.
- Operators take a space on each side (`a + b`, `x = 1`, `a == b and c`), and commas a space
  after them; brackets take none inside them (`f(a, b)`, `[1, 2]`, `xs[0]`).
- A `-` before a value takes no space (`-x`, `xs[:-1]`), and neither does a field's value in a
  struct (`Point(x=1.0, y=-2.0)`), the `:` of a slice (`xs[1:n]`), or a `?` (`f()?`).
- A `:` takes a space after it everywhere else (`n: int`, `{"a": 1}`, `fn f[T: ord]`), and
  `->` one on each side.
- Values inside a string are spaced like code (`"{x + 1}"`), while the string's text, and every
  literal's spelling (`1e3`, `"\x41"`), stays as written.
- Comments stay, a comment after code following it by one space. A comment on a line of its own
  is indented like the block it sits in.
- Runs of blank lines become one, and a blank line right after a line that opens a block, or at
  the start or end of the file, is removed. Trailing whitespace goes, and the file ends with
  one newline.

Since every statement is one line, stone never moves code from one line to another.

A file that does not parse is left alone: stone prints its errors, formats the other files,
and exits with status 1. Types are not checked, so a file with type errors is formatted. With
`--check`, nothing is written; stone lists each file that is not formatted and exits with
status 1 if there is one, which suits continuous integration.

```sh
stone fmt                    # formats every .stn file under the current directory
stone fmt examples/basics.stn
stone fmt --check src
```

## `stone`

With no file, stone starts an interactive session. Each entry runs as soon as it is complete,
and variables, functions, and modules from earlier entries stay available. An expression on its
own prints its value, with strings quoted as they are inside a list, unless it is a call that
returns no value, such as `print(1)`.

```text
$ stone
stone 0.2.0. Press Ctrl+D to exit.
>>> x = 6
>>> x * 7
42
>>> fn greet(name: str) -> str;
...     ret "hi " + name
...
>>> greet("Ada")
'hi Ada'
```

A line that opens a block, such as `fn f(n: int):` or `if x:`, continues the entry with a `...`
prompt, and a blank line ends it. Every entry is checked together with the earlier ones as if
they were one file, so `x = "a"` after `x = 6` is an error. An entry with an error is reported as
`<stdin>:line:col` and discarded, and one that fails at runtime prints `error: message`; the
session continues either way. Defining a function again replaces the earlier definition.
`use` reads modules relative to the current directory, and `input()` reads the line after the
entry.

Ctrl+D ends the session. stone does no line editing of its own, so run `rlwrap stone` for
arrow keys and history. When standard input is not a terminal, stone prints no banner or
prompts, so `printf 'x = 6\nx * 7\n' | stone` prints just `42`.

## `stone update [--check] [--version TAG]`

Replaces the installed binary with a newer GitHub release, after verifying its checksum. With
`--check`, it only reports whether a newer release exists, and `--version` installs a specific
release, such as `v0.2.0`. Release binaries include this command. A `stone` built with `cargo`
explains how to upgrade instead, unless it was built with `--features self-update`.

## `stone uninstall [--yes]`

Deletes the running `stone` binary. It asks `remove PATH? [y/N]` first, and `--yes` (or `-y`)
skips the question. Without a terminal on standard input it needs `--yes`. A binary in
`$CARGO_HOME/bin` (by default `~/.cargo/bin`) was put there by `cargo install`, so stone leaves
it and tells you to run `cargo uninstall stone`.

## Other options

- `stone --version` prints the version.
- `stone --help` and `stone COMMAND --help` describe the commands and options.

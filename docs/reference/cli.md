# Command line

```text
stone [FILE [ARGS...]] [COMMAND]
```

Every command reads one `.st` file, the entry file, along with every module it uses (see
[Modules](modules.md)), then checks them before doing anything else. If the checker
finds errors, the command prints all of them and exits with status 1 without running anything.
Errors print as `file:line:col: error: message`, followed by the source line and a caret under
the problem.

## `stone run FILE [ARGS...]`

Interprets the program. `stone FILE` is shorthand for `stone run FILE`. Everything after the
file is passed to the program, which reads it with `args()`, even arguments that start with `-`.
The program's standard input is stone's own, for `input()` and `eof()`.

```sh
stone run examples/basics.st
stone run sum.st 1 2 3 < numbers.txt
```

## `stone build FILE [-o OUTPUT] [--target ARCH]`

Compiles the program to a native executable at `OUTPUT` (default `build/out`, relative to the
current directory), writing the assembly next to it as `OUTPUT.s`. This needs Linux with `gcc` on
`PATH`, which assembles and links the output.

The executable is for the processor stone runs on, x86-64 or arm64. `--target` picks one
explicitly, as `x86_64` (or `x64`) or `aarch64` (or `arm64`). Building for the other processor
needs its cross compiler instead of `gcc`: `aarch64-linux-gnu-gcc` for arm64 or
`x86_64-linux-gnu-gcc` for x86-64, as Debian and Ubuntu's `gcc-aarch64-linux-gnu` and
`gcc-x86-64-linux-gnu` packages install them.

```sh
stone build examples/basics.st -o build/basics
./build/basics
stone build examples/basics.st -o build/basics-arm64 --target aarch64
```

The executable reads its own arguments and standard input the same way, so `./build/sum 1 2 3`
behaves like `stone run sum.st 1 2 3`.

## `stone check FILE`

Reports every syntax and type error without running the program, and exits with status 1 if
there are any. Warnings alone print but leave the status at 0.

```sh
stone check examples/basics.st
```

## `stone`

With no file, stone starts an interactive session. Each entry runs as soon as it is complete,
and variables, functions, and modules from earlier entries stay available. An expression on its
own prints its value unless the value is `none`, with strings quoted as they are inside a list.

```text
$ stone
stone 0.1.3. Press Ctrl+D to exit.
>>> x = 6
>>> x * 7
42
>>> def greet(name);
...     ret "hi " + name
...
>>> greet("Ada")
'hi Ada'
```

A line that opens a block, such as `def f(n);` or `if x;`, continues the entry with a `...`
prompt, and a blank line ends it. Every entry is checked together with the earlier ones as if
they were one file, so `x = "a"` after `x = 6` is an error. An entry with an error is reported as
`<stdin>:line:col` and discarded, and one that fails at runtime prints `error: message`; the
session continues either way. Defining a function again replaces the earlier definition.
`use` reads modules relative to the current directory, and `input()` reads the line after the
entry.

Ctrl+D ends the session. stone does no line editing of its own, so run `rlwrap stone` for
arrow keys and history. When standard input is not a terminal, stone prints no banner or
prompts, so `printf 'x = 6\nx * 7\n' | stone` prints just `42`.

## `stone self-update [--check] [--version TAG]`

Replaces the installed binary with a newer GitHub release, after verifying its checksum. With
`--check`, it only reports whether a newer release exists, and `--version` installs a specific
release, such as `v0.1.0`. Release binaries include this command. A `stone` built with `cargo`
explains how to upgrade instead, unless it was built with `--features self-update`.

## Other options

- `stone --version` prints the version.
- `stone --help` and `stone COMMAND --help` describe the commands and options.

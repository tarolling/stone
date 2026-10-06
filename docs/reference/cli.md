# Command line

```text
stone [FILE [ARGS...]] [COMMAND]
```

Every command reads one `.st` file, then checks it before doing anything else. If the checker
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

## `stone build FILE [-o OUTPUT]`

Compiles the program to a native executable at `OUTPUT` (default `build/out`, relative to the
current directory), writing the assembly next to it as `OUTPUT.s`. This needs x86-64 Linux with
`gcc` on `PATH`, which assembles and links the output.

```sh
stone build examples/basics.st -o build/basics
./build/basics
```

The executable reads its own arguments and standard input the same way, so `./build/sum 1 2 3`
behaves like `stone run sum.st 1 2 3`.

## `stone check FILE`

Reports every syntax and type error without running the program, and exits with status 1 if
there are any. Warnings alone print but leave the status at 0.

```sh
stone check examples/basics.st
```

## `stone self-update [--check] [--version TAG]`

Replaces the installed binary with a newer GitHub release, after verifying its checksum. With
`--check`, it only reports whether a newer release exists, and `--version` installs a specific
release, such as `v0.1.0`. Release binaries include this command. A `stone` built with `cargo`
explains how to upgrade instead, unless it was built with `--features self-update`.

## Other options

- `stone --version` prints the version.
- `stone --help` and `stone COMMAND --help` describe the commands and options.

<!-- markdownlint-configure-file { "MD024": { "siblings_only": true } } -->

# Changelog

All notable changes to stone: the language, the `stone` binary, its interpreter, and its
compiler. The VS Code extension and the `stone-lsp` language server it ships have their own
changelog in `editors/vscode/CHANGELOG.md`. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and versions follow
[semantic versioning](https://semver.org/).

## [Unreleased]

## [0.1.3] - 2026-10-09

### Added

- An interactive session: `stone` with no file reads entries one at a time, runs each as soon as
  it is complete, and prints the value of an expression on its own. Variables, functions, and
  modules from earlier entries stay available, and a function can be defined again.
- `stone build` compiles for arm64 Linux as well as x86-64, targeting the host by default.
  `--target x86_64` or `--target aarch64` (also `x64` and `arm64`) cross-compiles, using
  `x86_64-linux-gnu-gcc` or `aarch64-linux-gnu-gcc` to assemble and link. Both backends print the
  same output and runtime errors as `stone run`.
- The builtin `os` module, which needs no file: `os.env`, `os.has_env`, `os.platform`, `os.arch`,
  `os.hostname`, `os.cpu_count`, `os.pid`, `os.cwd`, `os.exit`, `os.time`, and `os.clock`. Use it
  as `use os`, `use os.env`, or `use os as system`.
- `stone uninstall` deletes the `stone` binary after asking `[y/N]`, or right away with `--yes`
  (required without a terminal). A `stone` installed with `cargo install` is left alone, with a
  pointer to `cargo uninstall stone`.

### Changed

- `stone self-update` is now `stone update`, with the same `--check` and `--version` options.
- A program can no longer have a module of its own named `os`. An `os.st` in the program's
  directory is the error `'os' is a builtin module, so rename os.st`.

## [0.1.2] - 2026-10-06

### Added

- Modules. A program's entry file can `use` other `.st` files, found from its directory, so
  `use geometry.shapes` loads `geometry/shapes.st` and `shapes.area()` calls its `area`.
  `use geometry.shapes.area` binds a single function, `as` renames either, and only `pub def`
  functions can be used from another file. Library modules hold only `use` and `def`, so using one
  runs nothing, and modules may use each other in a cycle. Comment lines at the top of a module's
  file, followed by a blank line, document it.
- `stone run`, `stone build`, and `stone check` read and check every module the entry file uses,
  and report errors in any of them with that file's name.

### Changed

- Compiled programs free strings and lists when they are no longer used, through reference
  counting, instead of keeping every allocation until the program exits.

### Removed

- The undocumented `del` statement, which only unbound a name in the interpreter.

## [0.1.1] - 2026-10-05

### Added

- The `%` and `**` operators on ints and floats. `%` takes the dividend's sign, like `/`, and
  `**` takes an int exponent and groups right to left. `x % 0` is `division by zero`, and an int
  raised to a negative power is the runtime error `negative exponent`.
- Input and output: `input()` reads a line from standard input, `eof()` says whether it has
  ended, and `args()` returns the program's command-line arguments, as `stone run file.st a b` or
  a built `./file a b`.
- `str(x)` converts an int, float, or bool to a string, and `int(s)` and `float(s)` parse one,
  stopping the program with Python's error message if the text is not a number.
- The string methods `s.strip()` and `s.split()` (on whitespace) or `s.split(sep)`.
- A logo.

### Changed

- Comments start with `//` instead of `#`.
- `len` and `append` are methods: write `xs.len()` and `xs.append(x)` instead of `len(xs)` and
  `append(xs, x)`. `s.len()` works on strings too.

## [0.1.0] - 2026-10-04

### Added

- The first release of stone, a statically checked language with Python-like syntax that
  minimizes Shift-key use: blocks open with `;`, functions return with `ret`, and loops skip
  ahead with `cont`.
- Types `int`, `float`, `bool`, `str`, `none`, and `list[T]`, all inferred, with builtins
  `print`, `len`, `range`, `append`, `int`, and `float`.
- `stone run` interprets a program, and `stone build` compiles it to a native x86-64 Linux
  executable through an IR and a linear-scan register allocator, with `gcc` assembling and
  linking it. Both print the same output, including float formatting and runtime error messages.
- `stone check` reports every syntax and type error without running the program, as
  `file:line:col: error: message` with the source line and a caret.
- `stone self-update` replaces the binary with a newer release after verifying its checksum.
- Prebuilt binaries for x86-64 and arm64 Linux and macOS, and an `install.sh` that installs the
  right one into `~/.local/bin`.
- Documentation, with a tutorial and a language reference, at
  [tarolling.github.io/stone](https://tarolling.github.io/stone/).

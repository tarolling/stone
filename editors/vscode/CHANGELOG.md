<!-- markdownlint-configure-file { "MD024": { "siblings_only": true } } -->

# Changelog

All notable changes to the stone extension for VS Code and the `stone-lsp` language server it
bundles, which share a version. Changes to the language itself are in
[stone's changelog](https://github.com/tarolling/stone/blob/main/CHANGELOG.md). The format
follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and versions follow
[semantic versioning](https://semver.org/).

## [Unreleased]

## [0.3.4] - 2026-10-24

### Added

- Function values: hover shows a value's function type, such as `fn(int) -> int`, and a function
  named as a value hovers, goes to its definition, and is found by references and rename like a
  call of it. Completion after a list and a `.` offers `map`, `filter`, `any`, `all`,
  `sort_by_key`, `min_by_key`, and `max_by_key`.
- The `net` module: going to the definition of `net` or one of its functions, such as
  `net.connect`, opens the module's stone source, which the server writes beside the prelude,
  and its names cannot be renamed. Completion after a `socket` or `listener` and a `.` offers
  their methods, and `socket` and `listener` are highlighted as types in signatures.
- `bytes`: bytes literals such as `b"\x00"` and the type `bytes` are highlighted, and completion
  after bytes and a `.` offers their methods, as it does the new socket methods `send_bytes`,
  `read_bytes`, `read_exact`, `read_all_bytes`, and `set_timeout`.

## [0.3.3] - 2026-10-21

### Added

- Values in strings are highlighted as code, as `name` in `"hello {name}"`, with `{{` and `}}`
  shown as escapes, and hover, go to definition, and rename work on names inside them.
- Completion offers `display` on every value, and `{` closes itself like `(` and `[`.

## [0.3.2] - 2026-10-19

### Added

- Results in `stone-lsp`: completion offers `ok` and `err` as `result[T, E]`, and a result
  value's methods after a `.`; completion after a `str` offers `parse_int` and `parse_float`.
- Highlighting of `ok`, `err`, `result[...]` types, and the `?` operator.

## [0.3.1] - 2026-10-17

### Added

- Traits in `stone-lsp`: hover shows `trait Shape` with its methods, a trait's method as
  `fn Shape.area(self) -> float`, and a bounded type parameter as `(type parameter) T: Shape`; a
  generic function's signature shows its bounds, as in `fn total[T: Shape](xs: list[T])`. A trait
  is outlined with its methods, a call through a bound leads to the trait's method, and
  completion after a value of a bounded type parameter offers its traits' methods, and after a
  struct value the default methods of the traits it implements.
- Highlighting of `trait`, `Self`, and the prelude's `less`, `equal`, and `greater`.

## [0.3.0] - 2026-10-17

### Added

- Generic functions in `stone-lsp`: hover shows `fn first[T](xs: list[T]) -> T`, a type parameter
  hovers as `(type parameter) T`, renames everywhere in its function, and is outlined under it.
- Generic structs and enums in `stone-lsp`: hover shows `struct Pair[A, B]` and `enum Tree[T]`
  with their fields and variants, and a value's type with its types filled in, as
  `Pair[int, str]`.

### Changed

- `some`, `none`, and `option` lead to the prelude that defines them, which `stone-lsp` writes as
  `prelude.stn` beside its builtins reference. Hover shows `some(T)` and `enum option[T]` with its
  variants, and they cannot be renamed.
- Typing `.` after a value opens completion right away, without Ctrl+Space, and a list, string,
  map, or option offers only the builtin methods of its type.
- Blocks open with `:`, as stone now writes them: a line ending in `:` indents the next line and
  highlights as the start of a block.

## [0.2.7] - 2026-10-15

### Added

- Highlighting of `impl`, of the struct's name after it, and of `self`.
- Hover, go to definition, references, and rename for methods and associated functions through
  `stone-lsp`, shown as `fn Point.shift(self, dx: float)`. It outlines a struct with its methods
  after its fields, offers a variable's methods after its fields following a `.`, and a struct's
  associated functions after its name and a `.`.

## [0.2.6] - 2026-10-13

### Added

- Highlighting of `struct` and of a struct's name where it is defined.
- Hover, go to definition, references, and rename for structs and their fields through
  `stone-lsp`, including the field names in `Point(x=1.0, y=2.0)`. It outlines a struct with its
  fields, and offers a variable's fields after its name and a `.`.

## [0.2.5] - 2026-10-12

### Added

- Highlighting of `enum`, `match`, and `case`, of an enum's name where it is defined, and of
  `option[T]` in signatures.
- Hover, go to definition, references, and rename for enums and their variants through
  `stone-lsp`, which outlines an enum with its variants and offers the variants after an enum's
  name and a `.`, and the option methods after any other value.

### Changed

- `none` is no longer highlighted as a type in signatures, since it is now an option's value.

## [0.2.4] - 2026-10-11

### Added

- Highlighting of tuple types in signatures, such as `tuple[int, list[tuple[str, float]]]`, and
  hover, diagnostics, and completion for tuples through `stone-lsp`.

## [0.2.3] - 2026-10-11

### Added

- Highlighting of escape sequences in strings, such as `\n` and `\"`, so an escaped quote no
  longer ends the string, and of unknown escapes as errors.
- Highlighting of the augmented assignment operators `+=`, `-=`, `*=`, `/=`, `%=`, and `**=`,
  and diagnostics, hover, and rename for them through `stone-lsp`.
- Completion and hover for the string methods `join`, `find`, `startswith`, and `endswith`.

## [0.2.2] - 2026-10-11

### Changed

- The extension opens `.stn` files, stone's new extension, instead of `.st` files, and a
  program's entry file is the nearest `main.stn`.
- Highlighting, hover, and completion follow stone's new `fn` keyword, which replaces `def`.
- Highlighting of the types in function signatures, such as `list[int]` in
  `fn total(xs: list[int]) -> int;`, which stone now requires. Hover shows a function that returns
  `none` without `-> none`, as it is written.

## [0.2.1] - 2026-10-10

### Added

- Support for stone's new builtin `math`, `random`, and `time` modules, which works as it does
  for `os`: hover, completion after `use` and after the module's name, and go to definition in
  the builtins reference.
- Support for stone's new list features: completion after `.` offers `pop`, `insert`, and
  `extend`, hover and go to definition document them, and the editor understands slices such as
  `xs[1:]` and arithmetic on lists such as `a * 2.0 + b`, reporting their errors and the warning
  for `[0] * n`, which multiplies the element instead of repeating it.

## [0.2.0] - 2026-10-10

### Changed

- `stone-lsp` is built on stone 0.2.0, whose lists are values, so the editor reports the
  language's new rules: a warning when a function changes a list parameter it never reads
  again, since the caller cannot see the change, and errors for `append` used as a value
  (`ys = xs.append(1)`) and for a function that changes a global. Hover on `append` explains
  that it changes only the variable it is called on.

## [0.1.6] - 2026-10-09

### Added

- Support for the builtin `os` module: hover shows each function's documentation, completion
  offers `os` after `use` and its functions after `os.`, and go to definition jumps to the
  function's entry in the builtins reference. The same works through `use os as system` and
  `use os.env as getenv`.

## [0.1.5] - 2026-10-06

### Added

- Modules. A file is checked as part of its program, whose entry file is the nearest `main.st`
  in its directory or one above it, so errors, hover, and completion know about every module it
  uses. Go to definition, references, and rename work across files, and hover on a module's name
  shows the comment at the top of its file.
- Highlighting for `use`, `pub`, and `as`.

### Changed

- Editing one file updates the errors shown in every other open file of its program.

### Removed

- `del`, which stone no longer has.

## [0.1.4] - 2026-10-05

### Added

- An icon for the extension and for `.st` files.
- Hover shows the `//` comment lines directly above a function or variable as its
  documentation.
- Hover, completion after a `.`, and go to definition for the builtin methods `len`, `append`,
  `strip`, and `split`, and for the new builtins `str`, `input`, `eof`, and `args`.
- Highlighting for `%` and `**`.

### Changed

- Comments start with `//` instead of `#`, for highlighting and for the Toggle Line Comment
  command, following stone 0.1.1.

## [0.1.3] - 2026-10-04

### Changed

- The display name is "stone language" again.

## [0.1.2] - 2026-10-04

### Changed

- The extension's ID is `tarolling.stone-lang`, and its display name is "stone". Published to
  Open VSX only.

## [0.1.1] - 2026-10-04

### Changed

- The display name is "stone language". Published to Open VSX only.

## [0.1.0] - 2026-10-04

### Added

- The first release: syntax highlighting, errors as you type, the inferred type or
  documentation on hover, go to definition, find references, rename, the document outline, and
  completion for `.st` files.
- `stone-lsp` comes bundled for x86-64 and arm64 Linux and macOS. On other platforms the
  extension runs `stone-lsp` from PATH.
- The `stone.server.path` setting runs another `stone-lsp`, such as a local build.
- Published to Open VSX only, as `tarolling.stone`. The first release on the VS Code
  Marketplace is 0.1.3.

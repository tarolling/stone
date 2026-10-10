<!-- markdownlint-configure-file { "MD024": { "siblings_only": true } } -->

# Changelog

All notable changes to the stone extension for VS Code and the `stone-lsp` language server it
bundles, which share a version. Changes to the language itself are in
[stone's changelog](https://github.com/tarolling/stone/blob/main/CHANGELOG.md). The format
follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and versions follow
[semantic versioning](https://semver.org/).

## [Unreleased]

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

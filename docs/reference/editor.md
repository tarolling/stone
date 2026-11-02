# Editor support

`stone-lsp` is a language server for stone, and the "stone" VS Code extension runs it. Any
editor that speaks the Language Server Protocol can use `stone-lsp` too: start it with no
arguments, speaking LSP over stdin and stdout, for files ending in `.stn`.

## Features

- **Diagnostics as you type**, from the same checker `stone check` uses. A syntax error only
  affects the statement it is in, so the rest of the file keeps working while you type.
  Unused variables, parameters, functions, methods, types, and imports get a warning and are
  shown faded, as `stone check` reports them.
- **Hover** shows a variable's or expression's inferred type, a function's signature such as
  `fn add(a: int, b: int) -> int`, or a builtin's documentation. A function or variable also
  shows the `//` comment lines directly above its definition.
- **Go to definition**, **find references**, and **rename** for variables, parameters, and
  functions. Going to the definition of a builtin like `print` opens a generated `builtins.stn`
  that documents every builtin.
- **Outline** of functions and variables, with each function's parameters and locals.
- **Completion** of names visible at the cursor, builtins, and keywords.
- **Formatting**: Format Document (and format on save) rewrites the file as `stone fmt` does.
- **Packages** (see {doc}`../guides/packages`) are read from the package store and from
  directories, as the compiler reads them, but never downloaded: a package that is not
  downloaded yet is an error at each `use` of it and in `stone.toml`, with the hint to run
  `stone fetch`, and the errors clear once it has run. Go to definition opens a package's files,
  and a package's names cannot be renamed. A file inside a package is checked as part of it,
  with the package's own dependencies.

The VS Code extension also provides syntax highlighting, comment toggling, bracket matching, and
indentation after a line ending in `:`.

## Installing

Install "stone" by tarolling from the VS Code Marketplace, or from Open VSX in editors such as
VSCodium. On Linux and macOS (x86-64 or arm64) the extension bundles `stone-lsp`. On other
platforms, or for other editors, install the server with

```sh
cargo install --git https://github.com/tarolling/stone stone-lsp
```

## Settings

- `stone.server.path`: the `stone-lsp` executable to run. When empty (the default), the
  extension runs its bundled server, or `stone-lsp` on `PATH` if it has none.

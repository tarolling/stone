# Editor support

`stone-lsp` is a language server for stone, and the "stone" VS Code extension runs it. Any
editor that speaks the Language Server Protocol can use `stone-lsp` too: start it with no
arguments, speaking LSP over stdin and stdout, for files ending in `.st`.

## Features

- **Diagnostics as you type**, from the same checker `stone check` uses. A syntax error only
  affects the statement it is in, so the rest of the file keeps working while you type.
- **Hover** shows a variable's or expression's inferred type, a function's signature such as
  `def add(a: int, b: int) -> int`, or a builtin's documentation. A function or variable also
  shows the `//` comment lines directly above its definition.
- **Go to definition**, **find references**, and **rename** for variables, parameters, and
  functions. Going to the definition of a builtin like `print` opens a generated `builtins.st`
  that documents every builtin.
- **Outline** of functions and variables, with each function's parameters and locals.
- **Completion** of names visible at the cursor, builtins, and keywords.

The VS Code extension also provides syntax highlighting, comment toggling, bracket matching, and
indentation after a line ending in `;`.

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

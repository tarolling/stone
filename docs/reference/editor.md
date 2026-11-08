# Editor support

`stone-lsp` is a language server for stone, and the "stone" VS Code extension runs it. Any
editor that speaks the Language Server Protocol can use `stone-lsp` too: start it with no
arguments, speaking LSP over stdin and stdout, for files ending in `.stn`. With `--mcp` it
serves coding agents instead (see [Coding agents](#coding-agents-mcp)).

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
VSCodium. On Linux and macOS (x86-64 or arm64) and x86-64 Windows the extension bundles
`stone-lsp`. On other
platforms, or for other editors, install the server with

```sh
cargo install --git https://github.com/tarolling/stone stone-lsp
```

## Settings

- `stone.server.path`: the `stone-lsp` executable to run. When empty (the default), the
  extension runs its bundled server, or `stone-lsp` on `PATH` if it has none.

## Coding agents (MCP)

`stone-lsp --mcp` serves the same analysis to coding agents over the
[Model Context Protocol](https://modelcontextprotocol.io), as Pylance does for Python. Its tools
read programs from disk and analyze each file as part of its program, as the editor does:

| tool | does |
| --- | --- |
| `stone_guide` | returns the guide to writing stone for agents, which the server asks agents to read first |
| `stone_diagnostics` | checks a file, or every `.stn` file under a directory, and returns errors and warnings as `stone check` writes them |
| `stone_check_snippet` | checks a program given as text |
| `stone_run_snippet` | runs a program given as text with the interpreter, with optional input and arguments, returning its stdout, stderr, and how it ended; a step limit stops a loop that never ends |
| `stone_hover` | describes what is at a position, as hover does |
| `stone_definition` | says where the symbol at a position is defined |
| `stone_references` | lists every use of the symbol at a position |
| `stone_symbols` | outlines a file |
| `stone_rename` | renames the symbol at a position in every file of its program, writing the files |
| `stone_docs` | looks up a builtin function, method, type, module, or module function |

A position is a 1-based `line` plus either `symbol`, the name as it appears on that line, or a
1-based `column`. Relative paths start from the directory the server runs in, and answers show
paths relative to it.

The VS Code extension offers the server to VS Code's agents on VS Code 1.101 and later, so its
tools appear in Copilot's agent mode with nothing to set up. For Claude Code, add it from the
project's directory with

```sh
claude mcp add stone -- stone-lsp --mcp
```

using `stone-lsp` from `PATH` (see above for installing it), or the full path to the server the
extension bundles. Any other agent that speaks MCP over stdio runs the same command.

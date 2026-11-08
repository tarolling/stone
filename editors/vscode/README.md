# stone for VS Code

Language support for stone `.stn` files, backed by the `stone-lsp` language server:

- syntax highlighting, comment toggling, bracket matching, and indentation after a line ending in `:`
- errors as you type, from the same checker `stone check` uses
- hover to see signatures and inferred types, such as `fn add(a: int, b: int) -> int`
- go to definition, find references, and rename. Going to the definition of a builtin like `print`
  opens a generated `builtins.stn` that documents the builtins
- an outline of functions and variables, and completion of names, builtins, and keywords
- formatting, with the same layout `stone fmt` writes
- tools for coding agents: on VS Code 1.101 and later the extension offers `stone-lsp --mcp` as
  an MCP server, so Copilot's agent mode can check files, run snippets, look up builtins, hover,
  find definitions and references, and rename

## Install

Install "stone language" by tarolling from the VS Code Marketplace, or from Open VSX in editors such as
VSCodium. On Linux and macOS (x86-64 or arm64) and x86-64 Windows the extension bundles
`stone-lsp`, so there is
nothing else to set up. On other platforms it runs `stone-lsp` from PATH, which you can install
with Rust:

```sh
cargo install --git https://github.com/tarolling/stone stone-lsp
```

## Settings

- `stone.server.path`: the `stone-lsp` executable to run. When empty (the default), the extension
  runs its bundled server, or `stone-lsp` on PATH if it has none

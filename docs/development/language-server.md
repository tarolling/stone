# Language server

`lsp/` is the `stone-lsp` crate, built on `lsp-server` and `lsp-types`. It is a separate crate so
that `stone` itself keeps a single dependency.

```sh
cargo test -p stone-lsp       # unit, in-memory server, and stdio tests
cargo install --path lsp      # install stone-lsp into ~/.cargo/bin
```

## How it works

On every change to an open document, the server reanalyzes every open document as part of the
program it belongs to: the one whose entry file is the nearest `main.stn` in its directory or
above. `Document::in_program` links that program with `stone::project::link`, reading open
documents' text over what is on disk, and runs `stone::driver::analyze_linked`. A file the
program does not use yet is checked on its own, from the program's root. Requests are answered
from the resulting `checker::Analysis`: its diagnostics, the type of every expression, and every
symbol with all its references in every file. The analysis recovers from syntax errors statement
by statement, so features keep working around a typo, and it hides type errors until the syntax
and `use` errors are fixed.

A program's root is the nearest directory holding a `main.stn` or a package's `stone.toml`, and
its entry file that `main.stn` or, for a library, its `lib.stn`. Packages need nothing of their
own: `project::link` reads manifests through the same `Sources`, so the editor sees the program's
dependencies, from directories and from the package store, without downloading. Linking reports a
package that cannot be read once, in the manifest, and notes an error at each `use` of it in
`Linked::unavailable`, which only the editor shows. `features::package_diagnostics` gives the
errors of every `stone.toml` and `stone.lock`, which the server publishes to those files' URIs,
and when the client allows it the server registers a watcher for both, so editing a manifest or
running `stone fetch` reanalyzes every open document. Hover, completion, and diagnostics show a
dependency's linked names as the program writes them (`project::shown_names`, `geo.shapes.area`
for `geo.1.shapes.area`), and a symbol in a dependency, or in a downloaded package opened on its
own, cannot be renamed.

| file | role |
| --- | --- |
| `lsp/src/lib.rs` | the initialize handshake, open documents, diagnostics, and request dispatch |
| `lsp/src/features.rs` | one pure function per request over a `Document` |
| `lsp/src/line_index.rs` | converts stone's 1-based char positions to LSP's UTF-16 or UTF-8 positions |
| `lsp/src/uri.rs` | converts between `file://` URIs and paths |
| `lsp/src/mcp.rs`, `lsp/src/mcp/tools.rs` | `stone-lsp --mcp`, the MCP server for coding agents |

To add a feature, write it in `features.rs` with unit tests in `lsp/src/features/tests.rs`, then
wire it into `Server::handle_request` and `capabilities`.

## The MCP server

`stone-lsp --mcp` (`lsp/src/mcp.rs`) serves the Model Context Protocol over stdin and stdout:
newline-delimited JSON-RPC 2.0, answering `initialize`, `ping`, `tools/list`, and `tools/call`
with no other dependency than `serde_json`. Each tool in `lsp/src/mcp/tools.rs` reads the file it
is given from disk into a `Document::in_program`, as the language server does for an open
document, calls the same `features` function, and writes the answer as text: positions as
`file:line:col: <line>` and diagnostics as `stone check` renders them. Snippets run on a thread
of their own with a step limit (`Context::fuel`), and an answer stops waiting after
`Context::timeout`. Tests in `lsp/src/mcp/tests.rs` drive `serve` and each tool over temporary
directories, and `lsp/tests/stdio.rs` runs the binary with `--mcp`. To try it by hand:

```sh
cargo build -p stone-lsp
printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | target/debug/stone-lsp --mcp
```

The repository's `.mcp.json` runs `target/debug/stone-lsp --mcp` for Claude Code sessions in the
repository, so build it first.

## The VS Code extension

`editors/vscode/` holds the extension. To work on it:

1. Install a server with `cargo install --path lsp`, or build one with
   `cargo build -p stone-lsp` and set `stone.server.path` to the absolute path of
   `target/debug/stone-lsp`.
2. Install its dependencies with `npm install` in `editors/vscode`.
3. Open `editors/vscode` in VS Code and press F5 to launch a window with the extension loaded,
   or package and install it:

   ```sh
   npx @vscode/vsce package
   code --install-extension stone-lang-<version>.vsix
   ```

`npm test` runs the extension's tests, which cover how `server.js` picks a server: the
`stone.server.path` setting, then a bundled `server/stone-lsp`, then `stone-lsp` on `PATH`. They
also cover `mcp.js`, which offers `<server> --mcp` to VS Code's agents through
`vscode.lm.registerMcpServerDefinitionProvider` (VS Code 1.101 and later; older versions skip it)
and the `mcpServerDefinitionProviders` contribution.

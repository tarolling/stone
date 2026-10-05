# Language server

`lsp/` is the `stone-lsp` crate, built on `lsp-server` and `lsp-types`. It is a separate crate so
that `stone` itself keeps a single dependency.

```sh
cargo test -p stone-lsp       # unit, in-memory server, and stdio tests
cargo install --path lsp      # install stone-lsp into ~/.cargo/bin
```

## How it works

On every change to an open document, the server reruns `stone::driver::analyze` and answers
requests from the resulting `checker::Analysis`: its diagnostics, the type of every expression,
and every symbol with all its references. `analyze` recovers from syntax errors statement by
statement, so features keep working around a typo, and it hides type errors until the syntax
errors are fixed.

| file | role |
| --- | --- |
| `lsp/src/lib.rs` | the initialize handshake, open documents, diagnostics, and request dispatch |
| `lsp/src/features.rs` | one pure function per request over a `Document` |
| `lsp/src/line_index.rs` | converts stone's 1-based char positions to LSP's UTF-16 or UTF-8 positions |

To add a feature, write it in `features.rs` with unit tests in `lsp/src/features/tests.rs`, then
wire it into `Server::handle_request` and `capabilities`.

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
   code --install-extension stone-0.1.0.vsix
   ```

`npm test` runs the extension's tests, which cover how `server.js` picks a server: the
`stone.server.path` setting, then a bundled `server/stone-lsp`, then `stone-lsp` on `PATH`.

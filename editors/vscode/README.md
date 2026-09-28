# stone for VS Code

Language support for stone `.st` files, backed by the `stone-lsp` language server:

- syntax highlighting, comment toggling, bracket matching, and indentation after a line ending in `;`
- errors as you type, from the same checker `stone check` uses
- hover to see inferred types, such as `def add(a: int, b: int) -> int`
- go to definition, find references, and rename
- an outline of functions and variables, and completion of names, builtins, and keywords

## Setup

1. Install the server from the repository root, which puts `stone-lsp` in `~/.cargo/bin`:

   ```sh
   cargo install --path lsp
   ```

   To use a local build instead, run `cargo build -p stone-lsp` and set `stone.server.path` to the
   absolute path of `target/debug/stone-lsp`.

2. Install the extension's dependencies:

   ```sh
   cd editors/vscode
   npm install
   ```

3. Either open `editors/vscode` in VS Code and press F5 to launch a window with the extension
   loaded, or package and install it:

   ```sh
   npx @vscode/vsce package
   code --install-extension stone-0.1.0.vsix
   ```

## Settings

- `stone.server.path`: the `stone-lsp` executable to run, `stone-lsp` on PATH by default

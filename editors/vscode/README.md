# stone for VS Code

Language support for stone `.st` files, backed by the `stone-lsp` language server:

- syntax highlighting, comment toggling, bracket matching, and indentation after a line ending in `;`
- errors as you type, from the same checker `stone check` uses
- hover to see inferred types, such as `def add(a: int, b: int) -> int`
- go to definition, find references, and rename. Going to the definition of a builtin like `print`
  opens a generated `builtins.st` that documents the builtins
- an outline of functions and variables, and completion of names, builtins, and keywords

## Install

Install "stone" by tarolling from the VS Code Marketplace, or from Open VSX in editors such as
VSCodium. On Linux and macOS (x86-64 or arm64) the extension bundles `stone-lsp`, so there is
nothing else to set up. On other platforms it runs `stone-lsp` from PATH, which you can install
with step 1 below.

## Development setup

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

- `stone.server.path`: the `stone-lsp` executable to run. When empty (the default), the extension
  runs its bundled server, or `stone-lsp` on PATH if it has none

## Tests

`npm test` runs the unit tests in `test/` with Node's built-in test runner.

## Releasing

Bump `version` in `package.json`, then push a matching tag:

```sh
git tag vscode-v0.1.1 && git push origin vscode-v0.1.1
```

`.github/workflows/vscode.yml` builds `stone-lsp` for each platform, packages a `.vsix` per
platform plus a universal one, and publishes them to the VS Code Marketplace and Open VSX. Running
the workflow by hand from the Actions tab packages the extensions without publishing them.

Publishing needs the repository secret `OVSX_PAT` for Open VSX, and `AZURE_CLIENT_ID` and
`AZURE_TENANT_ID` for a Microsoft Entra ID app that is a member of the `tarolling` Marketplace
publisher, since global Azure DevOps PATs stop working on December 1, 2026.

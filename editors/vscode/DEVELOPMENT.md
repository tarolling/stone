# Developing the stone extension

This file is left out of the packaged extension, so it does not show on the Marketplace page.

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
   code --install-extension stone-lang-0.1.3.vsix
   ```

## Tests

`npm test` runs the unit tests in `test/` with Node's built-in test runner.

## Releasing

Bump the version from the repository root, which sets `package.json`, `package-lock.json`, and
`stone-lsp`'s version in `lsp/Cargo.toml` and `Cargo.lock` together, then commit and push the
matching tag it prints:

```sh
sh scripts/bump-version.sh vscode patch   # or minor, major, or a version such as 1.0.0
git tag vscode-v0.1.5 && git push origin HEAD vscode-v0.1.5
```

`.github/workflows/vscode.yml` builds `stone-lsp` for each platform, packages a `.vsix` per
platform plus a universal one, and publishes them to the VS Code Marketplace and Open VSX. Running
the workflow by hand from the Actions tab packages the extensions without publishing them.

Publishing needs the repository secret `OVSX_PAT` for Open VSX, and `AZURE_CLIENT_ID` and
`AZURE_TENANT_ID` for a Microsoft Entra ID app that is a member of the `tarolling` Marketplace
publisher, since global Azure DevOps PATs stop working on December 1, 2026.

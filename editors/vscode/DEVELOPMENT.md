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

List each change under `## [Unreleased]` in `CHANGELOG.md` as it lands. The version in
`package.json` is always the next one to release, since it is bumped right after each release.
To release it, push its tag, then bump from the repository root, which dates the release's notes
in `CHANGELOG.md` and sets `package.json`, `package-lock.json`, and `stone-lsp`'s version in
`lsp/Cargo.toml` and `Cargo.lock` together, and commit what it prints:

```sh
git tag vscode-v0.1.6 && git push origin vscode-v0.1.6
sh scripts/bump-version.sh vscode patch   # or minor, major, or a version such as 1.0.0
```

`.github/workflows/vscode.yml` builds `stone-lsp` for each platform, packages a `.vsix` per
platform plus a universal one, and publishes them to the VS Code Marketplace and Open VSX, which
show `CHANGELOG.md`. It then creates a GitHub release with every `.vsix` attached and the
version's changelog section (or `[Unreleased]`, if it has none yet) as its notes, never marked
Latest so that `install.sh` keeps finding stone's releases. Running the workflow by hand from the
Actions tab packages the extensions without publishing them.

Publishing needs the repository secret `OVSX_PAT` for Open VSX, and `AZURE_CLIENT_ID` and
`AZURE_TENANT_ID` for a Microsoft Entra ID app that is a member of the `tarolling` Marketplace
publisher, since global Azure DevOps PATs stop working on December 1, 2026.

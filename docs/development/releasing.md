# Releasing

## stone

1. Bump `version` in `Cargo.toml` and commit.
2. Push a matching tag:

   ```sh
   git tag v0.1.1 && git push origin v0.1.1
   ```

`.github/workflows/release.yml` checks that the tag matches `Cargo.toml`, builds `stone` with the
`self-update` feature for x86-64 and arm64 Linux (musl) and macOS, smoke-tests each binary it can
run, and publishes `stone-<target>.tar.gz` plus a `.sha256` for each as a GitHub release.
Running the workflow by hand from the Actions tab builds and tests the archives without
publishing.

`install.sh` and `stone self-update` both download from these releases. To test the install
script against a local release, build first and run

```sh
cargo build
sh scripts/test-install.sh
```

which serves a fake release from a `file://` URL through `STONE_DOWNLOAD_URL`.

## The VS Code extension

1. Bump `version` in `editors/vscode/package.json` and commit.
2. Push a matching tag:

   ```sh
   git tag vscode-v0.1.1 && git push origin vscode-v0.1.1
   ```

`.github/workflows/vscode.yml` builds `stone-lsp` for each platform, packages a `.vsix` per
platform plus a universal one, and publishes them to the VS Code Marketplace and Open VSX.
Publishing needs the repository secret `OVSX_PAT` for Open VSX, and `AZURE_CLIENT_ID` and
`AZURE_TENANT_ID` for a Microsoft Entra ID app that is a member of the `tarolling` Marketplace
publisher.

## The documentation

`.github/workflows/docs.yml` publishes this site to GitHub Pages on every push to `main` that
changes the docs, and builds it without publishing on pull requests.

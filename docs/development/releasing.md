# Releasing

## Changelogs

stone's changes go in `CHANGELOG.md`, and the VS Code extension's and `stone-lsp`'s in
`editors/vscode/CHANGELOG.md`. Add each change under `## [Unreleased]` when it lands, in an
`### Added`, `### Changed`, `### Removed`, or `### Fixed` subsection.

Versions are bumped right after a release, so the version in `Cargo.toml` or `package.json` is
always the next one to release, and `[Unreleased]` holds its changes. Each release workflow
publishes a version's section, or `[Unreleased]` if it has none yet, as the release notes, and
fails before building if both are empty. The bump after a release then renames `[Unreleased]`
to `## [X.Y.Z] - YYYY-MM-DD`, dated with the tag's day, and opens a new empty one above it. It
only does that when the current version's tag exists, so bumping twice before a release leaves
the changelog alone.

`scripts/changelog.sh` does the work, and `sh scripts/test-changelog.sh` tests it and checks that
both changelogs are well formed and have notes for every release tag:

```sh
sh scripts/changelog.sh notes CHANGELOG.md 0.1.2           # prints 0.1.2's dated notes
sh scripts/changelog.sh release-notes CHANGELOG.md 0.1.3   # what the release workflow publishes
sh scripts/changelog.sh release CHANGELOG.md 0.1.3         # what the bump after it runs
```

## stone

1. Check that `[Unreleased]` in `CHANGELOG.md` lists the release's changes, and that
   `Cargo.toml` has its version.

2. Tag the release and push the tag:

   ```sh
   git tag v0.1.3 && git push origin v0.1.3
   ```

3. Bump to the next version, which edits `Cargo.toml` and stone's entry in each `Cargo.lock`,
   and dates the release's notes in `CHANGELOG.md`. Then commit it, as the script prints:

   ```sh
   sh scripts/bump-version.sh stone patch   # or minor, major, or a version such as 1.0.0
   git add Cargo.toml Cargo.lock fuzz/Cargo.lock bench/Cargo.lock CHANGELOG.md
   git commit -m "bump stone to 0.1.4" && git push origin HEAD
   ```

`.github/workflows/release.yml` checks that the tag matches `Cargo.toml`, builds `stone` with the
`self-update` feature for x86-64 and arm64 Linux (musl) and macOS, smoke-tests each binary it can
run, and publishes `stone-<target>.tar.gz` plus a `.sha256` for each as a GitHub release, with
the version's section of `CHANGELOG.md` as its notes. It is always marked Latest.
Running the workflow by hand from the Actions tab builds and tests the archives without
publishing.

`install.sh` and `stone update` both download from these releases. To test the install
script against a local release, build first and run

```sh
cargo build
sh scripts/test-install.sh
```

which serves a fake release from a `file://` URL through `STONE_DOWNLOAD_URL`.

## The VS Code extension

The same steps, with `editors/vscode/CHANGELOG.md` and `editors/vscode/package.json`:

1. Tag the release and push the tag:

   ```sh
   git tag vscode-v0.1.6 && git push origin vscode-v0.1.6
   ```

2. Bump to the next version, which edits `editors/vscode/package.json` and `package-lock.json`
   and dates the release's notes in `editors/vscode/CHANGELOG.md`. The extension ships
   `stone-lsp`, so this also sets `lsp/Cargo.toml` and its `Cargo.lock` entry to the same
   version. Commit what the script prints:

   ```sh
   sh scripts/bump-version.sh vscode patch   # or minor, major, or a version such as 1.0.0
   ```

`.github/workflows/vscode.yml` builds `stone-lsp` for each platform, packages a `.vsix` per
platform plus a universal one, and publishes them to the VS Code Marketplace and Open VSX, which
show `editors/vscode/CHANGELOG.md` on their Changelog tabs (the packaged copy has the tag's
notes under the version's heading, dated that day). Then it creates a GitHub release
titled "VS Code extension X.Y.Z" with every `.vsix` attached and the version's section of the
changelog as its notes. That release is never marked Latest, since `install.sh` and
`stone update` download from the Latest release.
Publishing needs the repository secret `OVSX_PAT` for Open VSX, and `AZURE_CLIENT_ID` and
`AZURE_TENANT_ID` for a Microsoft Entra ID app that is a member of the `tarolling` Marketplace
publisher.

## The documentation

`.github/workflows/docs.yml` publishes this site to GitHub Pages on every push to `main` that
changes the docs, and builds it without publishing on pull requests.

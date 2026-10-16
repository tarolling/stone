# Contributing to stone

Thanks for your interest in stone. This page covers what you need to send a change. The
[development guides](docs/development/) go into more detail, and the
[documentation site](https://tarolling.github.io/stone/) has the tutorial and language reference.

## Before you start

- This project follows the [Code of Conduct](CODE_OF_CONDUCT.md). By taking part, you agree to
  follow it.

- Read [the philosophy](docs/philosophy.md) before designing a language feature. It explains
  what stone is aiming for and how to rank goals when they conflict: safety, then programmer
  simplicity, then clear errors, then speed, then compiler simplicity.
- For a large change, such as new syntax or a new backend, open an issue first so the design can
  be discussed before you write the code.
- Bug reports are welcome as issues. Include the program, the command you ran (`stone run`,
  `stone build`, or `stone check`), what you expected, and what happened. Report security
  vulnerabilities privately instead, as [SECURITY.md](SECURITY.md) describes.

## Setup

You need Rust (the workspace uses edition 2024).

```sh
git clone https://github.com/tarolling/stone
cd stone
cargo build
cargo test
```

The test suite runs the programs `stone build` compiles, so it needs x86-64 or arm64 Linux (or
arm64 macOS). To also run the cross-target tests and compare the built-in assembler with GNU's,
install qemu-user and the other processor's binutils, for example
`sudo apt-get install qemu-user binutils-aarch64-linux-gnu` on x86-64 Debian or Ubuntu. Tests
that need missing tools skip themselves. See [building](docs/development/building.md) for the
everyday commands.

## Making a change

1. Write the test first, run it, and confirm it fails for the reason you expect. Then write the
   code that makes it pass.
2. Cover new language behavior with a golden program: add `name.stn` and the output it should
   print as `name.out` to `tests/programs/`. Every golden program runs under both the
   interpreter (`stone run`) and the compiler (`stone build`), which must agree, so keep `SKIPS`
   in `tests/programs.rs` empty. A program that should fail at runtime also gets a `name.err`.
   See [testing](docs/development/testing.md) for `.in`, `.args`, `.env`, multi-file programs,
   and unit tests.
3. Run `cargo fmt` and `cargo clippy --all-targets`, and fix what clippy reports.
4. Add a line describing your change under `## [Unreleased]` in [CHANGELOG.md](CHANGELOG.md)
   (or [editors/vscode/CHANGELOG.md](editors/vscode/CHANGELOG.md) for the extension and
   `stone-lsp`), in an `### Added`, `### Changed`, `### Removed`, or `### Fixed` subsection.
5. If you changed what users see, update the docs in `docs/`. Changes to syntax also update
   `docs/grammar/stone.gram` and `docs/grammar/stone.asdl`.

A few rules hold across the codebase:

- Every stage returns an error on bad input and never panics.
- The checker only accepts programs that both backends run identically.
- Runtime error messages, and the positions they point at, match between backends.
- The `stone` crate depends only on `clap` (plus the opt-in `self-update` feature). Compiled
  programs depend on no libc.

The [style guide](docs/development/style.md) lists the conventions for code, comments, docs, and
commit messages, and the checklist for adding a builtin or keyword.

## Pull requests

CI must pass before a pull request is merged. It runs, among other things:

```sh
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo clippy --all-targets --features self-update -- -D warnings
cargo test
cargo test --features self-update --bin stone
```

on x86-64 Linux, arm64 Linux, and arm64 macOS, plus the shell script tests, the VS Code
extension's `npm test`, and a short fuzzing run. Keep each pull request to one change, and
describe what it does and how you tested it.

Other areas have their own guides:

- [Fuzzing](docs/development/fuzzing.md): turn every crash a fuzzer finds into a regression test.
- [Benchmarks](bench/README.md)
- [Language server and VS Code extension](docs/development/language-server.md)
- [Releasing](docs/development/releasing.md), for maintainers

## License

stone is licensed under the [Apache License 2.0](LICENSE). By submitting a contribution, you
agree that it is licensed under the same terms.

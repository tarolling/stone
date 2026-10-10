# Building and contributing

stone is a Rust workspace (edition 2024). The `stone` crate itself depends only on `clap`,
unless built with the opt-in `self-update` feature. The workspace also holds `lsp/` (the
`stone-lsp` language server), and separate `fuzz/` and `bench/` crates.

## Setup

```sh
git clone https://github.com/tarolling/stone
cd stone
cargo build
```

`stone build` assembles and links programs itself, so it needs no other tools, but the test
suite runs what it builds, so it needs x86-64 or arm64 Linux. To also run the other processor's
programs, and to compare the built-in assembler with GNU's, install qemu-user and the GNU
binutils for both processors, which on an x86-64 Debian or Ubuntu machine is
`sudo apt-get install qemu-user binutils-aarch64-linux-gnu` (see [testing](testing.md)).

## Everyday commands

| task | command |
| --- | --- |
| interpret a file | `cargo run -- run examples/basics.st` |
| compile a file | `cargo run -- build examples/basics.st -o build/basics` |
| check a file | `cargo run -- check examples/basics.st` |
| run every test | `cargo test` |
| run one test | `cargo test simple_functions` |
| format | `cargo fmt` |
| lint | `cargo clippy --all-targets` |
| build the docs | see below |

`build/` is gitignored scratch output. In a debug build, setting `STONE_DEBUG=1` prints token
dumps, parser traces, and each function's IR and register assignment to stderr:

```sh
STONE_DEBUG=1 cargo run -- build examples/basics.st
```

## What CI runs

Every push and pull request runs, and must pass:

```sh
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo clippy --all-targets --features self-update -- -D warnings
cargo test
cargo test --features self-update --bin stone
shellcheck install.sh scripts/test-install.sh
sh scripts/test-install.sh
(cd editors/vscode && npm test)
```

A separate job tests the fuzz program generator and fuzzes each crash target briefly.

## Workflow

- Practice test-driven development: write the test first, watch it fail for the expected
  reason, then make it pass. See {doc}`testing`.
- After every change, run `cargo fmt` and `cargo clippy`, and fix what clippy reports.
- Follow the {doc}`style`.

## Building the docs

This site is built with Sphinx and the Furo theme from `docs/`:

```sh
python3 -m venv .venv
.venv/bin/pip install -r docs/requirements.txt
.venv/bin/sphinx-build -W -b html docs docs/_build/html
```

Then open `docs/_build/html/index.html`. `-W` turns warnings, such as a broken link or a missing
include, into errors, as the docs workflow does. Every push to `main` that touches the docs
publishes the site to GitHub Pages.

Tutorial programs live in `docs/examples/` with their `.out` files and are pulled into pages with
`literalinclude`, so the golden tests run them under both backends. `tests/docs.rs` checks that
`reference/builtins.md` documents every builtin and `reference/syntax.md` lists every keyword.

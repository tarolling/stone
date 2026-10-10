# Installation

## Prebuilt binary

On Linux or macOS (x86-64 or arm64), the install script downloads the latest release into
`~/.local/bin` and verifies its SHA-256 checksum:

```sh
curl -fsSL https://raw.githubusercontent.com/tarolling/stone/main/install.sh | sh
```

Pass options after `sh -s --` to choose another directory or release:

```sh
curl -fsSL https://raw.githubusercontent.com/tarolling/stone/main/install.sh | sh -s -- --dir /usr/local/bin --version v0.1.0
```

| option | environment variable | default |
| --- | --- | --- |
| `--dir <path>` | `STONE_INSTALL_DIR` | `$HOME/.local/bin` |
| `--version <tag>` | `STONE_VERSION` | the latest release |

Make sure the directory is on your `PATH`, then check the install:

```sh
stone --version
```

## Upgrading

A release binary can replace itself with the newest release:

```sh
stone update            # install the newest release
stone update --check    # only report whether there is one
stone update --version v0.1.0
```

It checks the download against the release's SHA-256 checksum and makes sure the new binary
runs before swapping it in. A `stone` built with `cargo install` does not include
`stone update`; rerun `cargo install` to upgrade it instead.

## Uninstalling

`stone uninstall` deletes the `stone` binary after asking for confirmation. Pass `--yes` to skip
the question, which is required when standard input is not a terminal:

```sh
stone uninstall
stone uninstall --yes
```

A `stone` installed with `cargo install` refuses and asks you to run `cargo uninstall stone`
instead, so cargo's records stay correct.

## From source

With [Rust](https://rustup.rs) installed:

```sh
cargo install --git https://github.com/tarolling/stone stone
```

## Platform support

| command | needs |
| --- | --- |
| `stone run`, `stone check` | any platform stone builds on |
| `stone build` | x86-64 or arm64 Linux with `gcc` on `PATH` |

`stone build` writes assembly for the processor it runs on and runs `gcc` to assemble and link
it, so it does not work on macOS yet. The executables it makes are static and need no C
library, so they run on any Linux machine with the same processor. `--target` builds for the
other Linux processor with a cross compiler (see [the CLI reference](reference/cli.md)). The
interpreter runs the same programs everywhere.

## Editor support

Install "stone" by tarolling from the VS Code Marketplace, or from Open VSX in editors such as
VSCodium. On Linux and macOS (x86-64 or arm64) the extension bundles the language server. On
other platforms, install it yourself and make sure `stone-lsp` is on `PATH`:

```sh
cargo install --git https://github.com/tarolling/stone stone-lsp
```

See {doc}`reference/editor` for what the extension does.

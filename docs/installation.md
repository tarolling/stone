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

## From source

With [Rust](https://rustup.rs) installed:

```sh
cargo install --git https://github.com/tarolling/stone stone
```

## Platform support

| command | needs |
| --- | --- |
| `stone run`, `stone check` | any platform stone builds on |
| `stone build` | x86-64 Linux with `gcc` on `PATH` |

`stone build` writes x86-64 assembly and runs `gcc` to assemble and link it, so it does not work
on arm64 or macOS yet. The interpreter runs the same programs everywhere.

## Editor support

Install "stone" by tarolling from the VS Code Marketplace, or from Open VSX in editors such as
VSCodium. On Linux and macOS (x86-64 or arm64) the extension bundles the language server. On
other platforms, install it yourself and make sure `stone-lsp` is on `PATH`:

```sh
cargo install --git https://github.com/tarolling/stone stone-lsp
```

See {doc}`reference/editor` for what the extension does.

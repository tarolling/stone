# Installation

## Prebuilt binary

On Linux or macOS (x86-64 or arm64), the install script downloads the latest release into
`~/.local/bin` and verifies its SHA-256 checksum:

```sh
curl -fsSL https://raw.githubusercontent.com/tarolling/stone/main/install.sh | sh
```

Pass options after `sh -s --` to choose another directory or release:

```sh
curl -fsSL https://raw.githubusercontent.com/tarolling/stone/main/install.sh | sh -s -- --dir /usr/local/bin --version v0.2.0
```

| option | environment variable | default |
| --- | --- | --- |
| `--dir <path>` | `STONE_INSTALL_DIR` | `$HOME/.local/bin` |
| `--version <tag>` | `STONE_VERSION` | the latest release |

Make sure the directory is on your `PATH`, then check the install:

```sh
stone --version
```

On x86-64 Windows, the PowerShell script downloads the latest release into
`%LOCALAPPDATA%\Programs\stone\bin`, verifies its checksum, and adds that directory to your
user `PATH`:

```powershell
irm https://raw.githubusercontent.com/tarolling/stone/main/install.ps1 | iex
```

Piped into `iex`, it reads its options from the environment, and saved as a file it also takes
them as parameters:

| parameter | environment variable | default |
| --- | --- | --- |
| `-Dir <path>` | `STONE_INSTALL_DIR` | `%LOCALAPPDATA%\Programs\stone\bin` |
| `-Version <tag>` | `STONE_VERSION` | the latest release |
| `-NoModifyPath` | `STONE_NO_MODIFY_PATH` | add the directory to `PATH` |

## Upgrading

A release binary can replace itself with the newest release:

```sh
stone update            # install the newest release
stone update --check    # only report whether there is one
stone update --version v0.2.0
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
instead, so cargo's records stay correct. On Windows, which cannot delete a running program, the
binary is renamed to `stone.exe.old` at once and deleted as soon as stone exits.

## From source

With [Rust](https://rustup.rs) installed:

```sh
cargo install --git https://github.com/tarolling/stone stone
```

## Platform support

| command | needs |
| --- | --- |
| `stone run`, `stone check`, `stone fmt` | any platform stone builds on |
| `stone build` | any platform stone builds on; the executables it writes run on x86-64 or arm64 Linux, arm64 macOS, or x86-64 Windows |

`stone build` assembles and links programs itself, so it needs no assembler, linker, or C
compiler. Its Linux executables are static programs that need no C library, so they run on any
Linux machine with the same processor. Its macOS executables call the system only through
`libSystem`, which every Mac has, and are signed, so they run on any Apple silicon Mac with
macOS 12 or later. Its Windows executables import only from the system's `kernel32.dll` and
`bcrypt.dll`, so they run on any x86-64 Windows 10 or 11 machine, though programs that use the
`net` or `gui` module or `os.run` cannot be built for Windows yet, nor run on it. Executables
are for the machine stone runs on unless `--target` picks another (see
[the CLI reference](reference/cli.md)), and none needs anything else installed. Executables for
Intel Macs are not supported. The interpreter runs the same programs everywhere.

## Editor support

Install "stone" by tarolling from the VS Code Marketplace, or from Open VSX in editors such as
VSCodium. On Linux and macOS (x86-64 or arm64) and x86-64 Windows the extension bundles the
language server. On other platforms, install it yourself and make sure `stone-lsp` is on
`PATH`:

```sh
cargo install --git https://github.com/tarolling/stone stone-lsp
```

See {doc}`reference/editor` for what the extension does.

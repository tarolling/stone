#!/bin/sh
# Installs a prebuilt stone binary from the GitHub releases.
#
#   curl -fsSL https://raw.githubusercontent.com/tarolling/stone/main/install.sh | sh
#
# Pass options after `sh -s --`, e.g. `... | sh -s -- --version v0.1.0`.
# Everything runs inside main, called on the last line, so a partial download
# never runs half a script.
set -eu

repo="tarolling/stone"

usage() {
    cat <<EOF
Installs the stone programming language.

Usage: install.sh [--version <tag>] [--dir <path>]

Options:
  --version <tag>  release to install, such as v0.1.0 (default: latest)
  --dir <path>     directory to put stone in (default: \$HOME/.local/bin)
  -h, --help       print this help

Environment:
  STONE_VERSION       same as --version
  STONE_INSTALL_DIR   same as --dir
  STONE_DOWNLOAD_URL  base URL to download release files from
EOF
}

say() {
    echo "stone-install: $*"
}

err() {
    echo "stone-install: error: $*" >&2
    exit 1
}

# prints the release target for this machine, such as x86_64-unknown-linux-musl
detect_target() {
    os=$(uname -s)
    arch=$(uname -m)
    case "$arch" in
        x86_64 | amd64) arch=x86_64 ;;
        aarch64 | arm64) arch=aarch64 ;;
        *) arch="" ;;
    esac
    case "$os" in
        Linux) os=unknown-linux-musl ;;
        Darwin) os=apple-darwin ;;
        *) os="" ;;
    esac
    if [ -z "$arch" ] || [ -z "$os" ]; then
        err "no prebuilt stone for $(uname -s) $(uname -m); build it from source with
    cargo install --git https://github.com/$repo stone"
    fi
    echo "$arch-$os"
}

# downloads $1 to the file $2
download() {
    if command -v curl > /dev/null 2>&1; then
        curl -fsSL "$1" -o "$2"
    elif command -v wget > /dev/null 2>&1; then
        wget -q "$1" -O "$2"
    else
        err "need curl or wget to download stone"
    fi
}

# checks that the file $1 matches the checksum file $2
verify() {
    expected=$(cut -d ' ' -f 1 < "$2")
    if command -v sha256sum > /dev/null 2>&1; then
        actual=$(sha256sum "$1" | cut -d ' ' -f 1)
    elif command -v shasum > /dev/null 2>&1; then
        actual=$(shasum -a 256 "$1" | cut -d ' ' -f 1)
    else
        say "warning: no sha256sum or shasum, skipping checksum verification"
        return
    fi
    if [ "$expected" != "$actual" ]; then
        err "checksum mismatch for $(basename "$1"): expected $expected, got $actual"
    fi
}

main() {
    version="${STONE_VERSION:-latest}"
    dir="${STONE_INSTALL_DIR:-${HOME:?}/.local/bin}"
    while [ $# -gt 0 ]; do
        case "$1" in
            --version)
                [ $# -ge 2 ] || err "--version needs a value"
                version="$2"
                shift 2
                ;;
            --dir)
                [ $# -ge 2 ] || err "--dir needs a value"
                dir="$2"
                shift 2
                ;;
            -h | --help)
                usage
                exit 0
                ;;
            *)
                usage >&2
                err "unknown option '$1'"
                ;;
        esac
    done

    target=$(detect_target)
    if [ -n "${STONE_DOWNLOAD_URL:-}" ]; then
        base="$STONE_DOWNLOAD_URL"
    elif [ "$version" = latest ]; then
        base="https://github.com/$repo/releases/latest/download"
    else
        base="https://github.com/$repo/releases/download/$version"
    fi
    archive="stone-$target.tar.gz"

    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT

    say "downloading $base/$archive"
    download "$base/$archive" "$tmp/$archive" || err "could not download $base/$archive"
    download "$base/$archive.sha256" "$tmp/$archive.sha256" ||
        err "could not download $base/$archive.sha256"
    verify "$tmp/$archive" "$tmp/$archive.sha256"

    tar -xzf "$tmp/$archive" -C "$tmp"
    mkdir -p "$dir"
    cp "$tmp/stone" "$dir/stone.tmp"
    chmod 755 "$dir/stone.tmp"
    # rename into place so a running stone is never overwritten mid-write
    mv -f "$dir/stone.tmp" "$dir/stone"
    say "installed $("$dir/stone" --version) to $dir/stone"
    say "upgrade it later with 'stone update', or remove it with 'stone uninstall'"

    case ":$PATH:" in
        *":$dir:"*) ;;
        *)
            say "$dir is not on your PATH; add this to your shell profile:"
            echo "    export PATH=\"$dir:\$PATH\""
            ;;
    esac
    case "$target" in
        *linux*) ;;
        *) say "note: 'stone build' writes Linux executables, so use 'stone run' to run programs here" ;;
    esac
}

main "$@"

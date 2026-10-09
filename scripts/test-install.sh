#!/bin/sh
# Tests install.sh against a local release built from target/debug/stone.
#
# Usage, from the repository root after `cargo build`:
#
#   sh scripts/test-install.sh
#
# It packages the binary the way release.yml does, installs it through
# install.sh with STONE_DOWNLOAD_URL pointing at a file:// directory, runs an
# example with the installed binary, uninstalls one, and checks that a bad
# checksum is rejected.
set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
binary="$root/target/debug/stone"
[ -x "$binary" ] || { echo "error: $binary not found, run cargo build first" >&2; exit 1; }

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# package the binary under every target name install.sh might pick
mkdir -p "$work/package" "$work/release"
cp "$binary" "$root/LICENSE" "$root/README.md" "$work/package/"
for target in x86_64-unknown-linux-musl aarch64-unknown-linux-musl \
    x86_64-apple-darwin aarch64-apple-darwin; do
    archive="stone-$target.tar.gz"
    tar -czf "$work/release/$archive" -C "$work/package" stone LICENSE README.md
    (cd "$work/release" && sha256sum "$archive" > "$archive.sha256")
done

echo "test: installs and runs an example"
STONE_DOWNLOAD_URL="file://$work/release" STONE_INSTALL_DIR="$work/bin" \
    sh "$root/install.sh"
"$work/bin/stone" run "$root/examples/basics.st" | diff - "$root/examples/basics.out"

echo "test: --dir overrides the install directory"
STONE_DOWNLOAD_URL="file://$work/release" sh "$root/install.sh" --dir "$work/other"
"$work/other/stone" --version

echo "test: stone uninstall removes the installed binary"
"$work/other/stone" uninstall --yes
[ ! -e "$work/other/stone" ]

echo "test: rejects a checksum mismatch"
for checksum in "$work"/release/*.sha256; do
    echo "0000000000000000000000000000000000000000000000000000000000000000  x" > "$checksum"
done
if STONE_DOWNLOAD_URL="file://$work/release" STONE_INSTALL_DIR="$work/bad" \
    sh "$root/install.sh" 2> "$work/stderr"; then
    echo "error: install.sh accepted a bad checksum" >&2
    exit 1
fi
grep -q "checksum" "$work/stderr"
[ ! -e "$work/bad/stone" ]

echo "test: rejects an unknown option"
if sh "$root/install.sh" --bogus 2> /dev/null; then
    echo "error: install.sh accepted --bogus" >&2
    exit 1
fi

echo "all install tests passed"

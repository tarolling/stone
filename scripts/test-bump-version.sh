#!/bin/sh
# Tests bump-version.sh on a copy of the files it edits.
#
# Usage, from anywhere:
#
#   sh scripts/test-bump-version.sh
set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# a fresh copy of every file the script touches, with known versions
reset() {
    rm -rf "$work/repo"
    mkdir -p "$work/repo/lsp" "$work/repo/fuzz" "$work/repo/bench" "$work/repo/editors/vscode"
    for file in Cargo.toml Cargo.lock lsp/Cargo.toml fuzz/Cargo.lock bench/Cargo.lock \
        editors/vscode/package.json editors/vscode/package-lock.json; do
        cp "$root/$file" "$work/repo/$file"
    done
    STONE_ROOT="$work/repo" sh "$root/scripts/bump-version.sh" stone 1.2.3 > /dev/null
    STONE_ROOT="$work/repo" sh "$root/scripts/bump-version.sh" vscode 0.4.5 > /dev/null
}

bump() {
    STONE_ROOT="$work/repo" sh "$root/scripts/bump-version.sh" "$@"
}

# prints the version on the line after `name = "<package>"` in a Cargo.lock
locked() {
    awk -v name="name = \"$2\"" 'found { print; exit } $0 == name { found = 1 }' "$work/repo/$1"
}

expect() {
    if [ "$1" != "$2" ]; then
        echo "FAIL: expected '$2', got '$1'" >&2
        exit 1
    fi
}

echo "test: an explicit version sets stone everywhere"
reset
expect "$(grep -m1 '^version' "$work/repo/Cargo.toml")" 'version = "1.2.3"'
for lock in Cargo.lock fuzz/Cargo.lock bench/Cargo.lock; do
    expect "$(locked "$lock" stone)" 'version = "1.2.3"'
done

echo "test: patch, minor, and major bump stone"
bump stone patch > /dev/null
expect "$(grep -m1 '^version' "$work/repo/Cargo.toml")" 'version = "1.2.4"'
bump stone minor > /dev/null
expect "$(grep -m1 '^version' "$work/repo/Cargo.toml")" 'version = "1.3.0"'
bump stone major > /dev/null
expect "$(grep -m1 '^version' "$work/repo/Cargo.toml")" 'version = "2.0.0"'
expect "$(locked Cargo.lock stone)" 'version = "2.0.0"'

echo "test: stone leaves the server and extension alone"
expect "$(grep -m1 '^version' "$work/repo/lsp/Cargo.toml")" 'version = "0.4.5"'
expect "$(locked Cargo.lock stone-lsp)" 'version = "0.4.5"'

echo "test: vscode bumps the extension and the server together"
reset
bump vscode minor > /dev/null
expect "$(grep -m1 '^version' "$work/repo/lsp/Cargo.toml")" 'version = "0.5.0"'
expect "$(locked Cargo.lock stone-lsp)" 'version = "0.5.0"'
expect "$(grep -m1 '^  "version"' "$work/repo/editors/vscode/package.json")" '  "version": "0.5.0",'
expect "$(grep -c '"version": "0.5.0"' "$work/repo/editors/vscode/package-lock.json")" 2
expect "$(grep -m1 '^version' "$work/repo/Cargo.toml")" 'version = "1.2.3"'

echo "test: the edited files are still valid"
node -e 'for (const f of process.argv.slice(1)) JSON.parse(require("fs").readFileSync(f))' \
    "$work/repo/editors/vscode/package.json" "$work/repo/editors/vscode/package-lock.json"
expect "$(cd "$work/repo" && git diff --no-index --numstat "$root/Cargo.lock" Cargo.lock \
    | awk '{ print $1 + $2 }')" 4

echo "test: prints the commands to release"
reset
output=$(bump stone patch)
case $output in
    *"git tag v1.2.4"*) ;;
    *) echo "FAIL: no tag command in: $output" >&2; exit 1 ;;
esac
output=$(bump vscode patch)
case $output in
    *"git tag vscode-v0.4.6"*) ;;
    *) echo "FAIL: no tag command in: $output" >&2; exit 1 ;;
esac

echo "test: rejects bad arguments"
reset
for args in "" "stone" "lsp patch" "stone 1.2" "stone v1.2.4" "stone 1.2.3" "stone 1.2.2"; do
    # shellcheck disable=SC2086
    if bump $args > /dev/null 2>&1; then
        echo "FAIL: accepted '$args'" >&2
        exit 1
    fi
done
expect "$(grep -m1 '^version' "$work/repo/Cargo.toml")" 'version = "1.2.3"'

echo "all tests passed"

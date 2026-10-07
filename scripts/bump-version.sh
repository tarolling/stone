#!/bin/sh
# Bumps the version of stone, or of the VS Code extension and stone-lsp, in every file that
# records it, then prints the commands that commit and tag the release.
#
# Usage, from anywhere:
#
#   sh scripts/bump-version.sh stone patch     # 0.1.1 -> 0.1.2
#   sh scripts/bump-version.sh vscode minor    # 0.1.4 -> 0.2.0
#   sh scripts/bump-version.sh stone 1.0.0
#
# `stone` edits Cargo.toml and stone's entry in Cargo.lock, fuzz/Cargo.lock, and
# bench/Cargo.lock. `vscode` edits editors/vscode/package.json and package-lock.json, plus
# lsp/Cargo.toml and stone-lsp's entry in Cargo.lock, since the extension ships the server, so
# both share one version taken from package.json. A new version must be greater than the
# current one. STONE_ROOT overrides the repository root, for tests.
set -eu

usage() {
    echo "usage: sh scripts/bump-version.sh <stone|vscode> <major|minor|patch|X.Y.Z>" >&2
    exit 1
}

[ $# -eq 2 ] || usage
target=$1
bump=$2
root=${STONE_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}

# replaces the file with stdin once the command writing it has succeeded
replace() {
    tmp="$1.tmp.$$"
    cat > "$tmp"
    mv "$tmp" "$1"
}

# prints the version in the first `version = "X.Y.Z"` line of a Cargo.toml
cargo_version() {
    sed -n 's/^version = "\(.*\)"$/\1/p' "$1" | head -n 1
}

# sets the first `version = ...` line of a Cargo.toml, which is the package's
set_cargo_version() {
    awk -v version="$2" '
        !done && /^version = / { print "version = \"" version "\""; done = 1; next }
        { print }
    ' "$1" | replace "$1"
}

# sets the version on the line after `name = "<package>"` in a Cargo.lock
set_locked_version() {
    awk -v name="name = \"$2\"" -v version="$3" '
        next_is_version && /^version = / { print "version = \"" version "\""; next_is_version = 0; next }
        { next_is_version = ($0 == name); print }
    ' "$1" | replace "$1"
}

# sets the top-level "version" of package.json, and of package-lock.json and its root package
set_npm_version() {
    awk -v version="$2" '
        /^  "version": / || (in_root && /^      "version": /) {
            sub(/"version": "[^"]*"/, "\"version\": \"" version "\"")
            in_root = 0
        }
        /^    "": \{/ { in_root = 1 }
        { print }
    ' "$1" | replace "$1"
}

case $target in
    stone) current=$(cargo_version "$root/Cargo.toml") ;;
    vscode) current=$(sed -n 's/^  "version": "\(.*\)",$/\1/p' "$root/editors/vscode/package.json") ;;
    *) usage ;;
esac

major=${current%%.*}
rest=${current#*.}
minor=${rest%%.*}
patch=${rest#*.}
case $bump in
    major) new="$((major + 1)).0.0" ;;
    minor) new="$major.$((minor + 1)).0" ;;
    patch) new="$major.$minor.$((patch + 1))" ;;
    *)
        echo "$bump" | grep -Eq '^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$' || usage
        new=$bump
        ;;
esac

# a new version must sort after the current one
newer=$(printf '%s\n%s\n' "$current" "$new" | sort -t. -k1,1n -k2,2n -k3,3n | tail -n 1)
if [ "$new" = "$current" ] || [ "$newer" != "$new" ]; then
    echo "error: $new is not newer than the current $target version $current" >&2
    exit 1
fi

case $target in
    stone)
        set_cargo_version "$root/Cargo.toml" "$new"
        for lock in Cargo.lock fuzz/Cargo.lock bench/Cargo.lock; do
            set_locked_version "$root/$lock" stone "$new"
        done
        tag="v$new"
        files="Cargo.toml Cargo.lock fuzz/Cargo.lock bench/Cargo.lock"
        ;;
    vscode)
        set_npm_version "$root/editors/vscode/package.json" "$new"
        set_npm_version "$root/editors/vscode/package-lock.json" "$new"
        set_cargo_version "$root/lsp/Cargo.toml" "$new"
        set_locked_version "$root/Cargo.lock" stone-lsp "$new"
        tag="vscode-v$new"
        files="editors/vscode/package.json editors/vscode/package-lock.json lsp/Cargo.toml Cargo.lock"
        ;;
esac

echo "bumped $target from $current to $new"
echo
echo "to release it:"
echo
echo "  git add $files"
echo "  git commit -m \"bump $target to $new\""
echo "  git tag $tag && git push origin HEAD $tag"

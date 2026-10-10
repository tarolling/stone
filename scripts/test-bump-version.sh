#!/bin/sh
# Tests bump-version.sh on a copy of the files it edits, with fixture changelogs.
#
# Usage, from anywhere:
#
#   sh scripts/test-bump-version.sh
set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# a changelog with one release and nothing unreleased
changelog() {
    printf '# Changelog\n\n## [Unreleased]\n\n## [0.0.1] - 2026-01-01\n\n- first\n' > "$work/repo/$1"
}

# adds a change under Unreleased
note() {
    awk -v change="$2" '{ print } $0 == "## [Unreleased]" { print ""; print "- " change }' \
        "$work/repo/$1" > "$work/note" && mv "$work/note" "$work/repo/$1"
}

# a fresh copy of every file the script touches, with known versions, and no tags
reset() {
    rm -rf "$work/repo"
    mkdir -p "$work/repo/lsp" "$work/repo/fuzz" "$work/repo/bench" "$work/repo/editors/vscode"
    for file in Cargo.toml Cargo.lock lsp/Cargo.toml fuzz/Cargo.lock bench/Cargo.lock \
        editors/vscode/package.json editors/vscode/package-lock.json; do
        cp "$root/$file" "$work/repo/$file"
    done
    changelog CHANGELOG.md
    changelog editors/vscode/CHANGELOG.md
    bump stone 1.2.3 > /dev/null
    bump vscode 0.4.5 > /dev/null
    git -C "$work/repo" init -q
}

# commits everything and tags it on the given day, as releasing does
tag() {
    git -C "$work/repo" add -A
    GIT_COMMITTER_DATE="$2T12:00:00" git -C "$work/repo" -c user.name=test -c user.email=test@example.com \
        commit -q --allow-empty -m "release $1"
    git -C "$work/repo" tag "$1"
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

echo "test: a bump before a release leaves the changelog alone"
reset
note CHANGELOG.md "a change to stone"
cp "$work/repo/CHANGELOG.md" "$work/before.md"
bump stone patch > /dev/null
expect "$(cat "$work/repo/CHANGELOG.md")" "$(cat "$work/before.md")"

echo "test: a bump right after a release dates its notes with the tag's day"
reset
note CHANGELOG.md "a change to stone"
note editors/vscode/CHANGELOG.md "a change to the extension"
tag v1.2.3 2026-03-04
bump stone patch > /dev/null
expect "$(grep -m1 '^version' "$work/repo/Cargo.toml")" 'version = "1.2.4"'
expect "$(grep -A 4 '^## \[Unreleased\]$' "$work/repo/CHANGELOG.md")" \
    "$(printf '## [Unreleased]\n\n## [1.2.3] - 2026-03-04\n\n- a change to stone')"
expect "$(grep -c '^## \[Unreleased\]$' "$work/repo/CHANGELOG.md")" 1
expect "$(grep -c '^## \[1.2.3\]' "$work/repo/editors/vscode/CHANGELOG.md")" 0

echo "test: vscode looks for its own tag"
bump vscode patch > /dev/null
expect "$(grep -c '^## \[0.4.5\]' "$work/repo/editors/vscode/CHANGELOG.md")" 0
tag vscode-v0.4.6 2026-03-05
bump vscode patch > /dev/null
expect "$(grep -A 4 '^## \[Unreleased\]$' "$work/repo/editors/vscode/CHANGELOG.md")" \
    "$(printf '## [Unreleased]\n\n## [0.4.6] - 2026-03-05\n\n- a change to the extension')"

echo "test: a version that is already dated stays as it is"
reset
printf '# Changelog\n\n## [Unreleased]\n\n## [1.2.3] - 2026-03-01\n\n- done\n' > "$work/repo/CHANGELOG.md"
cp "$work/repo/CHANGELOG.md" "$work/before.md"
tag v1.2.3 2026-03-04
bump stone patch > /dev/null
expect "$(cat "$work/repo/CHANGELOG.md")" "$(cat "$work/before.md")"

echo "test: a release with no notes stops the bump and changes nothing"
reset
tag v1.2.3 2026-03-04
cp -R "$work/repo" "$work/before"
if bump stone patch > /dev/null 2>&1; then
    echo "FAIL: bumped past a release with no notes" >&2
    exit 1
fi
diff -r -x .git "$work/before" "$work/repo"
rm -rf "$work/before"

echo "test: the printed commands add the changelog"
reset
case $(bump stone patch) in
    *"git add Cargo.toml "*" CHANGELOG.md"*) ;;
    *) echo "FAIL: stone's git add misses CHANGELOG.md" >&2; exit 1 ;;
esac
case $(bump vscode patch) in
    *"git add editors/vscode/package.json "*" editors/vscode/CHANGELOG.md"*) ;;
    *) echo "FAIL: vscode's git add misses editors/vscode/CHANGELOG.md" >&2; exit 1 ;;
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

#!/bin/sh
# Tests changelog.sh on fixture changelogs, then checks that the real ones are well formed and
# have an entry for every release tag.
#
# Usage, from anywhere:
#
#   sh scripts/test-changelog.sh
set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
log="$work/CHANGELOG.md"

changelog() {
    sh "$root/scripts/changelog.sh" "$@"
}

expect() {
    if [ "$1" != "$2" ]; then
        echo "FAIL: expected '$2', got '$1'" >&2
        exit 1
    fi
}

# fails unless the command fails
refuse() {
    if "$@" > /dev/null 2>&1; then
        echo "FAIL: accepted '$*'" >&2
        exit 1
    fi
}

# a changelog with an unreleased change and three releases, the middle one empty
reset() {
    cat > "$log" <<'EOF'
# Changelog

All notable changes.

## [Unreleased]

### Added

- something new

## [0.2.0] - 2026-02-01

### Added

- `x % y`

### Removed

- `del`


## [0.1.1] - 2026-01-15

## [0.1.0] - 2026-01-01

A paragraph wrapped
onto two lines.

### Added

- everything, wrapped
  onto two lines
- and more
EOF
}

# what notes prints for 0.2.0
release_020=$(cat <<'EOF'
### Added

- `x % y`

### Removed

- `del`
EOF
)

echo "test: notes prints a section without its heading or surrounding blank lines"
reset
expect "$(changelog notes "$log" 0.2.0)" "$release_020"

echo "test: notes prints the last section in the file, joining wrapped lines"
expect "$(changelog notes "$log" 0.1.0)" \
    "$(printf 'A paragraph wrapped onto two lines.\n\n### Added\n\n- everything, wrapped onto two lines\n- and more')"

echo "test: notes prints unreleased changes"
expect "$(changelog notes "$log" Unreleased)" "$(printf '### Added\n\n- something new')"

echo "test: notes rejects a missing version, an empty section, or a prefix of a version"
refuse changelog notes "$log" 0.3.0
refuse changelog notes "$log" 0.1.1
refuse changelog notes "$log" 0.1
refuse changelog notes "$work/missing.md" 0.1.0
expect "$(changelog notes "$log" 0.3.0 2>&1 || true)" "error: $log has no entry for 0.3.0"

echo "test: release-notes prints a version's own section if it has one"
reset
expect "$(changelog release-notes "$log" 0.2.0)" "$release_020"

echo "test: release-notes falls back to unreleased changes"
expect "$(changelog release-notes "$log" 0.3.0)" "$(printf '### Added\n\n- something new')"
expect "$(changelog release-notes "$log" 0.1.1)" "$(printf '### Added\n\n- something new')"

echo "test: release-notes rejects a version with no notes anywhere"
changelog release "$log" 0.3.0 2026-03-01
refuse changelog release-notes "$log" 0.4.0
expect "$(changelog release-notes "$log" 0.4.0 2>&1 || true)" \
    "error: $log has no entry for 0.4.0 and nothing under [Unreleased]"
refuse changelog release-notes "$log" Unreleased

echo "test: release dates the unreleased changes and opens a new section"
reset
changelog release "$log" 0.3.0 2026-03-01
expect "$(changelog notes "$log" 0.3.0)" "$(printf '### Added\n\n- something new')"
expect "$(grep -c '^## \[Unreleased\]$' "$log")" 1
expect "$(grep -A 2 '^## \[Unreleased\]$' "$log")" "$(printf '## [Unreleased]\n\n## [0.3.0] - 2026-03-01')"
expect "$(changelog notes "$log" 0.2.0)" "$release_020"

echo "test: release defaults to today"
reset
changelog release "$log" 0.3.0
expect "$(grep -c "^## \[0.3.0\] - $(date +%Y-%m-%d)$" "$log")" 1

echo "test: release refuses an empty unreleased section and leaves the file alone"
changelog release "$log" 0.4.0 2026-04-01 2> "$work/stderr" && exit 1
expect "$(cat "$work/stderr")" "error: nothing to release, since the [Unreleased] section of $log is empty"
expect "$(grep -c '0.4.0' "$log")" 0

echo "test: release refuses a version that already has a section, or bad arguments"
reset
cp "$log" "$work/before.md"
refuse changelog release "$log" 0.2.0
refuse changelog release "$log" 0.1.1
refuse changelog release "$log" v0.3.0
refuse changelog release "$log" 0.3.0 2026-3-1
refuse changelog release "$work/missing.md" 0.3.0
refuse changelog release "$log"
refuse changelog publish "$log" 0.3.0
expect "$(cat "$log")" "$(cat "$work/before.md")"

echo "test: release refuses a changelog with no unreleased section"
grep -v '^## \[Unreleased\]$' "$work/before.md" > "$log"
refuse changelog release "$log" 0.3.0

# every heading in a real changelog is Unreleased or a dated version with notes
check_headings() {
    file="$root/$1"
    grep -q '^## \[Unreleased\]$' "$file" || { echo "FAIL: $1 has no [Unreleased] section" >&2; exit 1; }
    grep '^## ' "$file" | grep -v '^## \[Unreleased\]$' | while read -r heading; do
        echo "$heading" | grep -Eq '^## \[[0-9]+\.[0-9]+\.[0-9]+\] - [0-9]{4}-[0-9]{2}-[0-9]{2}$' ||
            { echo "FAIL: $1 has a malformed heading: $heading" >&2; exit 1; }
        version=$(echo "$heading" | sed 's/^## \[\(.*\)\].*/\1/')
        changelog notes "$file" "$version" > /dev/null
    done
}

echo "test: the real changelogs are well formed"
check_headings CHANGELOG.md
check_headings editors/vscode/CHANGELOG.md

# CI's shallow checkouts have no tags, so this only checks anything in a full clone
# checks each tag with the prefix has notes in the changelog, where the current version's may
# still be under Unreleased, since it is dated by the bump after its release
check_tags() {
    for tag in $(git -C "$root" tag -l "$1[0-9]*"); do
        version=${tag#"$1"}
        if [ "$version" = "$3" ]; then
            changelog release-notes "$root/$2" "$version" > /dev/null
        else
            changelog notes "$root/$2" "$version" > /dev/null
        fi
    done
}

echo "test: every release tag has an entry"
check_tags v CHANGELOG.md "$(sed -n 's/^version = "\(.*\)"$/\1/p' "$root/Cargo.toml" | head -n 1)"
check_tags vscode-v editors/vscode/CHANGELOG.md \
    "$(sed -n 's/^  "version": "\(.*\)",$/\1/p' "$root/editors/vscode/package.json")"

echo "all tests passed"

#!/bin/sh
# Reads and updates a changelog: CHANGELOG.md for stone, or editors/vscode/CHANGELOG.md for the
# VS Code extension. Each keeps changes not yet released under `## [Unreleased]` and each release
# under `## [X.Y.Z] - YYYY-MM-DD`, newest first.
#
# Usage, from anywhere:
#
#   sh scripts/changelog.sh notes CHANGELOG.md 0.1.2                 # prints 0.1.2's notes
#   sh scripts/changelog.sh release-notes CHANGELOG.md 0.1.3         # the notes to publish
#   sh scripts/changelog.sh release CHANGELOG.md 0.1.3               # dates Unreleased as 0.1.3
#   sh scripts/changelog.sh release CHANGELOG.md 0.1.3 2026-10-08
#
# `notes` prints a version's section without its heading and with wrapped lines joined, and fails
# if the version has no section or an empty one. `release-notes` does the same, but falls back to
# the Unreleased section, since a version is usually tagged before its notes are dated. The
# release workflows publish what it prints. `release` renames Unreleased to the version, dated
# today unless a date is given, and opens a new empty Unreleased above it. It fails without
# editing the file if Unreleased is missing or empty or the version already has a section.
# bump-version.sh runs it for the version just released, right after its tag.
set -eu

usage() {
    echo "usage: sh scripts/changelog.sh notes <file> <version>" >&2
    echo "       sh scripts/changelog.sh release-notes <file> <version>" >&2
    echo "       sh scripts/changelog.sh release <file> <version> [YYYY-MM-DD]" >&2
    exit 1
}

fail() {
    echo "error: $1" >&2
    exit 1
}

# replaces the file with stdin once the command writing it has succeeded
replace() {
    tmp="$1.tmp.$$"
    cat > "$tmp"
    mv "$tmp" "$1"
}

# prints the section under the heading `## [<name>]`, without blank lines at either end, or
# exits 2 if there is no such heading
section() {
    awk -v heading="## [$2]" '
        /^## / {
            if (inside) exit
            inside = ($0 == heading || index($0, heading " ") == 1)
            if (inside) found = 1
            next
        }
        inside { lines[++n] = $0 }
        END {
            if (!found) exit 2
            first = 1
            while (first <= n && lines[first] ~ /^[ \t]*$/) first++
            while (n >= first && lines[n] ~ /^[ \t]*$/) n--
            for (i = first; i <= n; i++) print lines[i]
        }
    ' "$1"
}

# joins each hard-wrapped line onto the one before it, since GitHub renders a release body's line
# breaks as written, so `- a\n  b` becomes `- a b`, while headings, list items, tables, and code
# blocks keep their own lines
unwrap() {
    awk '
        /^[ \t]*```/ { fenced = !fenced; flush(); print; next }
        fenced { print; next }
        /^[ \t]*$/ { flush(); print; next }
        held != "" && held !~ /^#/ && $0 !~ /^[ \t]*([-*+] |[0-9]+\. |#|\|)/ {
            sub(/^[ \t]+/, "")
            held = held " " $0
            next
        }
        { flush(); held = $0 }
        END { flush() }
        function flush() { if (held != "") print held; held = "" }
    '
}

[ $# -ge 3 ] || usage
command=$1
file=$2
version=$3
echo "$version" | grep -Eq '^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$|^Unreleased$' || usage
[ -f "$file" ] || fail "$file does not exist"

case $command in
    notes)
        [ $# -eq 3 ] || usage
        notes=$(section "$file" "$version") || fail "$file has no entry for $version"
        [ -n "$notes" ] || fail "$file has no entry for $version"
        printf '%s\n' "$notes" | unwrap
        ;;
    release-notes)
        [ $# -eq 3 ] || usage
        [ "$version" != Unreleased ] || usage
        notes=$(section "$file" "$version") || notes=
        [ -n "$notes" ] || notes=$(section "$file" Unreleased) || notes=
        [ -n "$notes" ] || fail "$file has no entry for $version and nothing under [Unreleased]"
        printf '%s\n' "$notes" | unwrap
        ;;
    release)
        [ $# -le 4 ] || usage
        [ "$version" != Unreleased ] || usage
        date=${4:-$(date +%Y-%m-%d)}
        echo "$date" | grep -Eq '^[0-9]{4}-[0-9]{2}-[0-9]{2}$' || usage
        if section "$file" "$version" > /dev/null; then
            fail "$file already has an entry for $version"
        fi
        unreleased=$(section "$file" Unreleased) || fail "$file has no [Unreleased] section"
        [ -n "$unreleased" ] || fail "nothing to release, since the [Unreleased] section of $file is empty"
        awk -v release="## [$version] - $date" '
            !done && $0 == "## [Unreleased]" { print; print ""; print release; done = 1; next }
            { print }
        ' "$file" | replace "$file"
        ;;
    *) usage ;;
esac

# stone.toml

A `stone.toml` makes a directory a package. A program's sits next to its entry file, and a
package's at the root of its repository. The guides {doc}`../guides/packages` and
{doc}`../guides/publishing` show how they are used.

```toml
# a program that uses three packages
[package]
name = "app"
version = "0.1.0"
stone = "0.4"

[dependencies]
csv = { github = "alice/stone-csv", version = "1.2.0" }
num = { gitlab = "bob/num", version = "0.3" }
geo = { path = "../geo" }
```

A manifest holds two tables, `[package]` and `[dependencies]`, both optional. Any other key is an
error, as is a key one of them does not hold, so a misspelling is caught.

## [package]

| key | value | meaning |
| --- | --- | --- |
| `name` | a stone name | The package's name, which programs use it under unless they pick another. A package that others use must have one; a program needs none. |
| `version` | a version | The package's version. A released package's must be the version of its release's tag. |
| `stone` | a version | The oldest stone the package works with. An older stone reports an error at this key. |
| `description` | a string | What the package is for. |
| `license` | a string | Its license, as an SPDX expression such as `MIT` or `Apache-2.0`. |

A **stone name** is made of ASCII letters, digits, and `_`, does not start with a digit, and is
not a keyword. A `-` is not allowed, so the package in a repository `stone-csv` is named
something like `csv` or `stone_json`.

## [dependencies]

Each key is a dependency's name, which the package's files write in `use` (`use csv`,
`use csv.encode`). Besides being a stone name, it cannot be a builtin module (`os`, `math`,
`json`, ...), `prelude`, or a builtin (`print`, `some`, ...), and it cannot also be a module or
directory of the package. Its value is an inline table that says where it comes from:

| form | where it comes from |
| --- | --- |
| `{ github = "owner/repo", version = "1.2.0" }` | releases of a repository on github.com |
| `{ github = "owner/repo", version = "1.2.0", host = "github.example.com" }` | releases on GitHub Enterprise |
| `{ gitlab = "group/project", version = "1.2.0" }` | releases of a project on gitlab.com, whose path may have subgroups, as `group/subgroup/project` |
| `{ gitlab = "group/project", version = "1.2.0", host = "gitlab.example.com" }` | releases on another GitLab |
| `{ path = "../geo" }` | the directory `../geo`, relative to this `stone.toml` |

A dependency from a host must give a `version`: the oldest version it works with. stone uses the
newest version that any package of the program asks for, so this is a minimum, never a range or
an exact pin. A directory has no version and no host, and a package from a host cannot depend on
one. `host` is a host name, such as `gitlab.example.com`, without `https://`.

## Versions

A version is `major.minor.patch`, as `1.2.0`. A missing minor or patch is 0, so `1.2` is `1.2.0`
and `1` is `1.0.0`. A pre-release follows a `-`, as `2.0.0-beta.1`, and build metadata after a `+`
is ignored. Versions in a manifest have no `v`, but release tags may: `v1.2.0` and `1.2.0` are
both version 1.2.0.

Versions with the same major number, or before 1.0 the same minor number, are compatible, and a
program uses one version of each such class: 1.2.0 and 1.5.3 are one class, and 2.0.0, 0.3.1, and
0.4.0 each another. A program can use several classes of one package at once, which are linked
apart.

## The TOML it reads

stone reads `stone.toml` and `stone.lock` itself, and reads the part of TOML they use: comments,
`[table]` and `[[array of tables]]` headers with a single name, `key = value` lines with bare or
quoted keys, and values that are strings (basic `"..."` or literal `'...'`, on one line),
integers, booleans, arrays, and inline tables (on one line). Dotted keys, multi-line strings,
floats, and dates are errors.

## stone.lock

stone writes `stone.lock` next to `stone.toml` the first time it downloads a package, and keeps
it current, so it is never edited by hand except to forget a package's hash. It has one entry per
package from a host that the program uses, sorted:

```toml
# written by stone: the hash of what each package downloaded held
[[package]]
name = "csv"
github = "alice/stone-csv"
version = "1.2.0"
hash = "sha256:4f1c..."
```

`gitlab` replaces `github` for a project on GitLab, and `host` follows for another host. `hash` is
the SHA-256 of a listing of the package's kept files, one line per file of its own SHA-256, two
spaces, and its path. A package whose files no longer match is an error. Only a program's own
`stone.lock` is read; a package's is left out of its release.

## The package store

Downloaded packages are kept in `$STONE_HOME/packages` if `STONE_HOME` is set, and otherwise in
`~/.stone/packages` (`%LOCALAPPDATA%\stone\packages` on Windows), one directory per version:

```text
~/.stone/packages/
  github.com/alice/stone-csv/1.2.0/
  gitlab.com/bob/num/0.3.0/
```

Each holds the release's `stone.toml` and `.stn` files only. Every program shares the store, and
`stone run --offline` and the editor read only from it.

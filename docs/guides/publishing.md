# Publishing a package

A package is published by making a release of a GitHub or GitLab repository. There is no registry
and nothing to upload: a program that names the repository in its `stone.toml` downloads the
release straight from the host.

## Laying out the repository

The repository's root is the package. It holds a `stone.toml` that names the package and gives
its version, and the package's modules, as any stone program's root does:

```text
stone-csv/
  stone.toml
  lib.stn
  encode.stn
  parse/
    value.stn
```

```toml
[package]
name = "csv"
version = "1.0.0"
description = "Reading and writing JSON"
license = "MIT"
```

`name` is how programs name the package by default. It is made of ASCII letters, digits, and
`_`, and does not start with a digit. Avoid the name of a builtin module or builtin, such as
`math` or `print`, which programs cannot use, so each would have to pick another name for it.
`version` is the version you are about to release. See {doc}`../reference/manifest` for every
key.

`lib.stn` is the module a program gets from `use csv`, so it is where the package's main
functions go. The other modules are named by their paths, as in any program: a program writes
`use csv.encode` for `encode.stn` and `use csv.parse.value` for `parse/value.stn`. A package
needs no `lib.stn` if it has none to offer, and then programs use its modules by name.

Only `pub` functions, structs, enums, and traits can be used from outside the package, and every
module of a package holds only `use`, functions, and types, as library modules do, so using the
package never runs any code. A package's own files use each other by their paths within the
package (`use parse.value`), and its dependencies by their names.

A downloaded package keeps only its `stone.toml` and its `.stn` files. Tests, examples, and
anything else in the repository are left out, so they cost the programs that use it nothing.

## Depending on other packages

A package names its own dependencies in its `stone.toml`, as a program does:

```toml
[dependencies]
num = { github = "bob/num", version = "0.3.0" }
```

Give the oldest version your package works with, not the newest there is: a program that needs a
newer `num` asks for it, and gets that version for every part of the program (see
{doc}`packages`). A package from a host cannot depend on a directory, since the directory is not
part of its release.

## Trying it out

Before releasing, use the package from a program through a path dependency, which reads the
directory as it is:

```toml
[dependencies]
csv = { path = "../stone-csv" }
```

`stone check` and `stone run` in that program then report errors in the package's files too, and
the editor checks the package's files as you change them. Opening a file of the package itself in
the editor checks it as part of the package, with its own dependencies.

## Releasing a version

1. Set `version` in `stone.toml` to the new version, and commit it.
2. Tag the commit with the version, as `v1.0.0` or `1.0.0`, and push the tag.
3. Make a release of the tag:

   ```sh
   gh release create v1.0.0 --notes "First release"         # GitHub
   glab release create v1.0.0 --notes "First release"       # GitLab
   ```

   or with the "Draft a new release" button on GitHub, or Deploy, Releases on GitLab.

That's all: `stone add github:you/stone-csv` now finds it. A release is found by its tag, which
must be a version, and drafts are left out. stone downloads the source archive the host makes of
the tag, so no files need to be attached to the release.

The `version` in the released `stone.toml` must be the version of the tag, or programs that use
the release fail to build. A tag such as `v1.0.0-beta.1` is a pre-release, which
`stone add` only picks when asked for exactly.

Never move a tag once it is released. Programs that used the release recorded a hash of its files
in their `stone.lock`, and stop building if the release changes, so fix a mistake with a new
version instead.

## Choosing the next version

Versions are [semantic versions](https://semver.org), `major.minor.patch`:

- **patch** (1.0.0 to 1.0.1): fixes that change nothing a program relies on.
- **minor** (1.0.1 to 1.1.0): new functions, types, or modules, with everything that worked
  before still working.
- **major** (1.1.0 to 2.0.0): anything that could break a program that uses the package, such as
  removing or renaming a `pub` function, changing a signature, or adding a field to a `pub`
  struct, which every construction of it must then name.

A new major version is a separate package to stone: a program may use 1.x and 2.x side by side,
and upgrading to 2.x is always something its author asks for. Before 1.0, a new minor version
(0.3 to 0.4) is treated as a new major one, so a package can still change freely while it is
young.

# Using packages

A package is a directory of stone modules with a `stone.toml` that names it. A program uses other
people's packages by naming them in its own `stone.toml`, next to its entry file. stone downloads
them from GitHub or GitLab releases, so there is no registry to sign up for: any repository whose
releases hold a stone package is one.

## Adding a package

In the directory of your program's `main.stn`, run

```sh
stone add github:alice/stone-json
```

This finds the newest release of `alice/stone-json`, downloads it, and writes the dependency to
`stone.toml`, making the file if there is none:

```toml
[dependencies]
json = { github = "alice/stone-json", version = "1.4.0" }
```

The key, `json`, is the name the package gives itself, and the name your files use it under.
`--name` picks another (`stone add github:alice/stone-json --name js`). The package can be named
in several ways:

| you write | you get |
| --- | --- |
| `github:alice/stone-json` | the newest release |
| `github:alice/stone-json@1.2` | the newest 1.2.x release |
| `github:alice/stone-json@1.2.3` | exactly 1.2.3 |
| `https://github.com/alice/stone-json` | the same as `github:alice/stone-json` |
| `gitlab:group/project`, `gitlab:group/subgroup/project` | a project on gitlab.com |
| `gitlab:group/project --host gitlab.example.com` | a project on your own GitLab |
| `github:github.example.com/alice/stone-json` | a repository on GitHub Enterprise |

`@latest` is the default, and it skips pre-releases such as `2.0.0-beta.1`. To use one, give it
exactly.

## Using it in code

A package's modules are named by the dependency's name, then their path in the package:

```stone
use json                // the package's lib.stn
use json.encode         // encode.stn, or the pub fn encode of lib.stn
use json.parse.value    // parse/value.stn, or the pub fn value of parse.stn

print(json.decode("[1, 2]"))
```

`use json` loads the package's `lib.stn`, the module a package keeps its main functions in. A
package's files see only their own modules and their own dependencies, never your program's, so
two packages can each have a `util.stn` without meaning the same module.

Everything else works as with your own modules: only `pub` functions and types can be used, and
unused functions are never compiled in. See {doc}`../reference/modules`.

## Packages in a directory

A dependency can also be a directory, for a package you are writing yourself or one that lives
next to your program in the same repository:

```toml
[dependencies]
geo = { path = "../geo" }
```

The path is relative to the `stone.toml`, and the directory must have a `stone.toml` of its own
that gives the package a name. stone uses the directory as it is, so a change there shows up the
next time you run. Directories have no versions and are never downloaded or locked.

## How versions are chosen

Each dependency gives the oldest version it works with, and that is all a manifest ever says
about versions. stone uses the newest of the versions that your program and the packages it uses
ask for, which is how Go chooses versions:

- Your program asks for `json` 1.2.0, and `geo` asks for `json` 1.3.0, so both get `json` 1.3.0.
- A newer 1.9.0 being released changes nothing, until something asks for it.

So the same manifests always give the same versions, on every machine and on every day, and
nothing is ever upgraded behind your back. To upgrade, ask for the newer version: run
`stone add github:alice/stone-json` again, or edit the version in `stone.toml`.

Versions are [semantic versions](https://semver.org). A new major version may break the code that
uses it, so 1.x and 2.x are treated as different packages: if one part of your program uses
`json` 1.4.0 and another `json` 2.1.0, both are linked in, each under its own name, and neither
can see the other. Before 1.0, each minor version is its own: 0.3.x and 0.4.x are different
packages too.

## stone.lock

The first time a package is downloaded, stone writes a hash of its files to `stone.lock`, next to
`stone.toml`:

```toml
# written by stone: the hash of what each package downloaded held
[[package]]
name = "json"
github = "alice/stone-json"
version = "1.4.0"
hash = "sha256:4f1c..."
```

The lockfile does not choose versions, since the manifests already do. It makes sure that a
package is the same code every time: if a release is later moved to other code, or a file in the
download is changed, the program fails to build. If you trust the change (say, the author fixed a
release on purpose), delete the package's entry from `stone.lock`, and the next build records the
new hash. Commit `stone.lock`, so everyone who builds your program checks against the same
hashes.

## Downloading

`stone run`, `stone build`, and `stone check` download whatever the program uses that is not
downloaded yet, so after cloning a program, running it is enough. The commands for managing
packages are:

- `stone fetch` downloads everything the program uses and brings `stone.lock` up to date. With
  `--locked` it fails instead if `stone.lock` would change, which is what CI should run.
- `stone tree` shows each package the program uses and the packages they use:

  ```text
  app
  └── json 1.4.0 (github:alice/stone-json)
      └── num 0.3.0 (github:bob/num)
  ```

- `stone remove json` takes a dependency out of `stone.toml`, and out of `stone.lock` with any
  package only it used.
- `--offline`, given to `run`, `build`, or `check`, uses only what is already downloaded and
  never connects to anything.

Downloaded packages are kept in one place for every program, the package store:
`~/.stone/packages`, or `%LOCALAPPDATA%\stone\packages` on Windows. Set `STONE_HOME` to keep them
in `$STONE_HOME/packages` instead. Each version of each package is downloaded once, and only its
`stone.toml` and `.stn` files are kept. Don't change files in the store, since they no longer
match `stone.lock` once changed.

For a private repository, or to avoid GitHub's limit on requests without a token, set
`GITHUB_TOKEN` (or `GITLAB_TOKEN`) to a token that can read it.

Release builds of stone can download. A build from source needs the `fetch` feature for that
(`cargo install --git https://github.com/tarolling/stone --features fetch stone`), and without it reads only packages already downloaded.

## In the editor

The language server reads packages as the compiler does, from the store and from directories, but
never downloads. If a package is not downloaded yet, each `use` of it shows the error with the
hint to run `stone fetch`, and so does the dependency in `stone.toml`. Once `stone fetch` is done,
the errors clear on their own. Go to definition opens a package's files, and a package's names
cannot be renamed from your program, since other programs use them too.

See {doc}`publishing` to make a package of your own, and {doc}`../reference/manifest` for every
key `stone.toml` can hold.

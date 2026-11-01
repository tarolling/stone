//! The package store: where packages from hosts are downloaded, one directory per version.
//!
//! The store is `$STONE_HOME/packages`, which defaults to `~/.stone/packages`, or
//! `%LOCALAPPDATA%\stone\packages` on Windows. `json` 1.4.0 from `github:alice/stone-json` is in
//! `<store>/github.com/alice/stone-json/1.4.0/`, holding only the package's `stone.toml` and
//! `.stn` files.

use std::path::{Path, PathBuf};

use super::{MANIFEST, Source, Version};
use crate::codegen::sha256::sha256;
use crate::project::Sources;

/// Returns the store's directory, from `STONE_HOME` or else the home directory, or `None` if
/// neither is known.
pub fn default_dir() -> Option<PathBuf> {
    let home = match std::env::var_os("STONE_HOME") {
        Some(home) if !home.is_empty() => PathBuf::from(home),
        _ if cfg!(windows) => PathBuf::from(std::env::var_os("LOCALAPPDATA")?).join("stone"),
        _ => PathBuf::from(std::env::var_os("HOME")?).join(".stone"),
    };
    Some(home.join("packages"))
}

/// Returns the host a source's packages come from, as `github.com`.
pub fn host(source: &Source) -> &str {
    match source {
        Source::GitHub { host, .. } => host.as_deref().unwrap_or("github.com"),
        Source::GitLab { host, .. } => host.as_deref().unwrap_or("gitlab.com"),
        Source::Path(_) => "",
    }
}

/// Returns the directory of `version` of the package from `source` in the store `store`.
///
/// For example, `json` 1.4.0 from `github:alice/json` is `<store>/github.com/alice/json/1.4.0`.
pub fn package_dir(store: &Path, source: &Source, version: &Version) -> PathBuf {
    let path = match source {
        Source::GitHub { repo, .. } => repo,
        Source::GitLab { path, .. } => path,
        Source::Path(dir) => return dir.clone(),
    };
    let mut dir = store.join(host(source).replace(':', "_"));
    dir.extend(path.split('/'));
    dir.push(version.to_string());
    dir
}

/// Returns whether a package keeps the file at `path`: its manifest and its stone files.
pub fn is_kept(path: &Path) -> bool {
    path.file_name() == Some(std::ffi::OsStr::new(MANIFEST))
        || path.extension().is_some_and(|e| e == "stn")
}

/// Returns the hash of the files a package keeps under `dir`, as `sha256:` and 64 hexadecimal
/// digits: the SHA-256 of a line `<SHA-256 of the file>  <path>` per file, sorted by its path
/// from `dir` with `/` between names, so the hash depends on nothing but those files.
pub fn content_hash(sources: &dyn Sources, dir: &Path) -> String {
    let mut files = vec![];
    let mut dirs = vec![(dir.to_path_buf(), String::new())];
    while let Some((at, relative)) = dirs.pop() {
        for entry in sources.entries(&at) {
            let Some(name) = entry.file_name().map(|n| n.to_string_lossy().into_owned()) else {
                continue;
            };
            let path = match relative.is_empty() {
                true => name,
                false => format!("{relative}/{name}"),
            };
            if sources.is_dir(&entry) {
                dirs.push((entry, path));
            } else if is_kept(&entry)
                && let Some(text) = sources.read(&entry)
            {
                files.push((path, hex(&sha256(text.as_bytes()))));
            }
        }
    }
    files.sort();
    let listing: String = files
        .iter()
        .map(|(path, hash)| format!("{hash}  {path}\n"))
        .collect();
    format!("sha256:{}", hex(&sha256(listing.as_bytes())))
}

/// Writes bytes as lowercase hexadecimal digits.
pub fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::project::MapSources;

    #[test]
    fn packages_are_stored_by_host_path_and_version() {
        let source = Source::GitLab {
            host: Some("git.corp.com:8443".into()),
            path: "group/sub/http".into(),
        };
        let dir = package_dir(Path::new("store"), &source, &Version::new(0, 4, 1));
        assert_eq!(
            dir,
            [
                "store",
                "git.corp.com_8443",
                "group",
                "sub",
                "http",
                "0.4.1"
            ]
            .iter()
            .collect::<PathBuf>()
        );
    }

    #[test]
    fn the_hash_depends_only_on_the_kept_files() {
        let mut sources = MapSources::default();
        sources.insert("p/stone.toml", "[package]\nname = \"p\"\n");
        sources.insert("p/lib.stn", "pub fn one() -> int:\n    ret 1\n");
        sources.insert("p/sub/two.stn", "pub fn two() -> int:\n    ret 2\n");
        let hash = content_hash(&sources, Path::new("p"));
        assert!(hash.starts_with("sha256:") && hash.len() == 71);

        let mut moved = sources.clone();
        moved.insert("q/stone.toml", "[package]\nname = \"p\"\n");
        moved.insert("q/lib.stn", "pub fn one() -> int:\n    ret 1\n");
        moved.insert("q/sub/two.stn", "pub fn two() -> int:\n    ret 2\n");
        moved.insert("q/README.md", "not kept\n");
        assert_eq!(content_hash(&moved, Path::new("q")), hash);

        let mut changed = MapSources::default();
        changed.insert("p/stone.toml", "[package]\nname = \"p\"\n");
        changed.insert("p/lib.stn", "pub fn one() -> int:\n    ret 2\n");
        changed.insert("p/sub/two.stn", "pub fn two() -> int:\n    ret 2\n");
        assert_ne!(content_hash(&changed, Path::new("p")), hash);
    }
}

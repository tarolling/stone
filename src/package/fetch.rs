//! Downloading packages from GitHub and GitLab releases into the store (see [`super::store`]),
//! behind the `fetch` cargo feature, which brings in an HTTPS client.
//!
//! A version of a package is a release, found by its tag (`v1.2.0` or `1.2.0`), and its files
//! come from the source archive the host makes of the tag. Only the archive's `stone.toml` and
//! `.stn` files are kept. Selecting versions never lists releases (see [`super::resolve`]), so
//! only `stone add` asks for the newest, and downloading asks once per package for the tag of a
//! version.
//!
//! `GITHUB_TOKEN` and `GITLAB_TOKEN` are sent if set, for private repositories and higher rate
//! limits, and `STONE_GITHUB_API` and `STONE_GITLAB_API` replace `https://api.github.com` and
//! `https://gitlab.com/api/v4`, for tests.

use std::collections::{HashMap, HashSet};
use std::io::Read;
use std::path::{Component, Path, PathBuf};
use std::time::Duration;

use super::store::{is_kept, package_dir};
use super::{MANIFEST, Source, Version};
use crate::project::{self, FsSources, Linked, Sources};

/// The most an archive may hold, unpacked or not.
const MAX_ARCHIVE: u64 = 64 << 20;

/// The most pages of releases read, 100 to a page.
const MAX_PAGES: usize = 10;

/// A release of a package.
#[derive(Debug, Clone, PartialEq)]
pub struct Release {
    pub tag: String,
    pub version: Version,
}

/// Talks to the hosts packages come from.
pub struct Client {
    agent: ureq::Agent,
    /// Each package's releases, once read.
    releases: HashMap<Source, Vec<Release>>,
}

impl Default for Client {
    fn default() -> Self {
        Client::new()
    }
}

impl Client {
    pub fn new() -> Client {
        let config = ureq::Agent::config_builder()
            .timeout_global(Some(Duration::from_secs(120)))
            .user_agent(format!("stone/{}", env!("CARGO_PKG_VERSION")))
            .build();
        Client {
            agent: config.into(),
            releases: HashMap::new(),
        }
    }

    /// Returns every release of the package from `source` whose tag is a version, drafts and
    /// GitLab's upcoming releases left out.
    pub fn releases(&mut self, source: &Source) -> Result<Vec<Release>, String> {
        if let Some(releases) = self.releases.get(source) {
            return Ok(releases.clone());
        }
        let mut releases = vec![];
        for page in 1..=MAX_PAGES {
            let url = match source {
                Source::GitHub { repo, .. } => {
                    format!(
                        "{}/repos/{repo}/releases?per_page=100&page={page}",
                        api(source)
                    )
                }
                Source::GitLab { path, .. } => format!(
                    "{}/projects/{}/releases?per_page=100&page={page}",
                    api(source),
                    encode(path)
                ),
                Source::Path(_) => return Ok(vec![]),
            };
            let body = self.get(source, &url)?;
            let text = String::from_utf8(body).map_err(|_| "the host's answer is not text")?;
            let (found, count) = parse_releases(&text)?;
            releases.extend(found);
            if count < 100 {
                break;
            }
        }
        self.releases.insert(source.clone(), releases.clone());
        Ok(releases)
    }

    /// Returns the release of `version`, or an error listing the versions there are.
    pub fn release(&mut self, source: &Source, version: &Version) -> Result<Release, String> {
        let releases = self.releases(source)?;
        if let Some(release) = releases.iter().find(|r| r.version == *version) {
            return Ok(release.clone());
        }
        Err(format!(
            "{source} has no release {version}{}",
            listing(&releases)
        ))
    }

    /// Returns the newest release that `wanted` matches.
    pub fn newest(
        &mut self,
        source: &Source,
        wanted: &super::edit::Wanted,
    ) -> Result<Release, String> {
        let releases = self.releases(source)?;
        releases
            .iter()
            .filter(|r| wanted.matches(&r.version))
            .max_by(|a, b| a.version.cmp(&b.version))
            .cloned()
            .ok_or_else(|| match wanted {
                super::edit::Wanted::Latest => {
                    format!("{source} has no release yet{}", listing(&releases))
                }
                super::edit::Wanted::Matching { exact, .. } => {
                    format!(
                        "{source} has no release matching {exact}{}",
                        listing(&releases)
                    )
                }
            })
    }

    /// Downloads the release `version` of the package from `source` into the store `store`,
    /// unless it is there already.
    pub fn download(
        &mut self,
        store: &Path,
        source: &Source,
        version: &Version,
    ) -> Result<PathBuf, String> {
        let dir = package_dir(store, source, version);
        if dir.join(MANIFEST).is_file() {
            return Ok(dir);
        }
        let release = self.release(source, version)?;
        let tag = encode(&release.tag);
        let url = match source {
            Source::GitHub { repo, .. } => format!("{}/repos/{repo}/tarball/{tag}", api(source)),
            Source::GitLab { path, .. } => format!(
                "{}/projects/{}/repository/archive.tar.gz?sha={tag}",
                api(source),
                encode(path)
            ),
            Source::Path(dir) => return Ok(dir.clone()),
        };
        let archive = self.get(source, &url)?;
        unpack(&archive, &dir)?;
        if !dir.join(MANIFEST).is_file() {
            let _ = std::fs::remove_dir_all(&dir);
            return Err(format!(
                "its release {} has no {MANIFEST} at the top, so it is not a stone package",
                release.tag
            ));
        }
        Ok(dir)
    }

    /// Reads `url` from the host of `source`, sending its token if one is set.
    fn get(&self, source: &Source, url: &str) -> Result<Vec<u8>, String> {
        let mut request = self.agent.get(url);
        match source {
            Source::GitHub { .. } => {
                request = request.header("Accept", "application/vnd.github+json");
                if let Some(token) = token("GITHUB_TOKEN") {
                    request = request.header("Authorization", format!("Bearer {token}"));
                }
            }
            Source::GitLab { .. } => {
                if let Some(token) = token("GITLAB_TOKEN") {
                    request = request.header("PRIVATE-TOKEN", token);
                }
            }
            Source::Path(_) => {}
        }
        let mut response = request.call().map_err(|e| match e {
            ureq::Error::StatusCode(404) => {
                let token = match source {
                    Source::GitLab { .. } => "GITLAB_TOKEN",
                    _ => "GITHUB_TOKEN",
                };
                format!(
                    "{source} was not found (if it is private, set {token} to a token that can \
                     read it)"
                )
            }
            ureq::Error::StatusCode(code @ (403 | 429)) => format!(
                "the host refused with status {code}, which may be its rate limit; setting a \
                 token raises it"
            ),
            ureq::Error::StatusCode(code) => format!("the host answered with status {code}"),
            other => other.to_string(),
        })?;
        response
            .body_mut()
            .with_config()
            .limit(MAX_ARCHIVE)
            .read_to_vec()
            .map_err(|e| e.to_string())
    }
}

/// Links the program whose entry file is `entry` again and again, downloading each package it
/// uses that is not in the store, until every one is there, and returns the last link, whose
/// diagnostics say what else is wrong. `log` is told of each download.
///
/// Only the manifests count, so the entry file is linked as if it were empty.
pub fn fetch_missing(
    entry: &Path,
    client: &mut Client,
    log: &mut dyn FnMut(String),
) -> Result<Linked, String> {
    let store = FsSources
        .store()
        .ok_or("there is no directory to keep packages in; set STONE_HOME")?;
    let mut tried = HashSet::new();
    loop {
        let linked = project::link(entry, "", &FsSources);
        let missing: Vec<(Source, Version)> = linked
            .missing
            .iter()
            .filter(|m| !tried.contains(*m))
            .cloned()
            .collect();
        if missing.is_empty() {
            return Ok(linked);
        }
        for (source, version) in missing {
            log(format!("downloading {source} {version}"));
            client
                .download(&store, &source, &version)
                .map_err(|e| format!("cannot download {source} {version}: {e}"))?;
            tried.insert((source, version));
        }
    }
}

/// Returns a source's API, as `https://api.github.com`.
fn api(source: &Source) -> String {
    let from_env = |name: &str| std::env::var(name).ok().filter(|v| !v.is_empty());
    let url = match source {
        Source::GitHub { host: None, .. } => {
            from_env("STONE_GITHUB_API").unwrap_or("https://api.github.com".to_string())
        }
        Source::GitHub {
            host: Some(host), ..
        } => format!("https://{host}/api/v3"),
        Source::GitLab { host: None, .. } => {
            from_env("STONE_GITLAB_API").unwrap_or("https://gitlab.com/api/v4".to_string())
        }
        Source::GitLab {
            host: Some(host), ..
        } => format!("https://{host}/api/v4"),
        Source::Path(_) => String::new(),
    };
    url.trim_end_matches('/').to_string()
}

/// Returns the environment variable `name` if it is set and not empty.
fn token(name: &str) -> Option<String> {
    std::env::var(name).ok().filter(|t| !t.is_empty())
}

/// Percent-encodes everything but letters, digits, and `-._~`, as a path segment or query value
/// needs, so `group/project` is `group%2Fproject`.
fn encode(text: &str) -> String {
    text.bytes()
        .map(|b| match b {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'.' | b'_' | b'~' => {
                (b as char).to_string()
            }
            b => format!("%{b:02X}"),
        })
        .collect()
}

/// Writes the versions of `releases`, newest first, to end an error, or nothing if there are none.
fn listing(releases: &[Release]) -> String {
    let mut versions: Vec<&Version> = releases.iter().map(|r| &r.version).collect();
    versions.sort_by(|a, b| b.cmp(a));
    match versions.as_slice() {
        [] => " (it has no releases whose tags are versions, as v1.0.0)".to_string(),
        versions => {
            let shown: Vec<String> = versions.iter().take(8).map(|v| v.to_string()).collect();
            let more = if versions.len() > 8 { ", ..." } else { "" };
            format!(" (its releases are {}{more})", shown.join(", "))
        }
    }
}

/// Reads a page of GitHub's or GitLab's releases, returning those whose tags are versions and
/// how many the page held.
fn parse_releases(text: &str) -> Result<(Vec<Release>, usize), String> {
    let value =
        super::json::parse(text).map_err(|e| format!("the host's answer is not JSON: {e}"))?;
    let items = value
        .as_array()
        .ok_or("the host's answer is not a list of releases")?;
    let releases = items
        .iter()
        .filter(|item| {
            let flag = |key: &str| item.get(key).and_then(|v| v.as_bool()).unwrap_or(false);
            !flag("draft") && !flag("upcoming_release")
        })
        .filter_map(|item| {
            let tag = item.get("tag_name")?.as_str()?;
            Some(Release {
                tag: tag.to_string(),
                version: Version::from_tag(tag)?,
            })
        })
        .collect();
    Ok((releases, items.len()))
}

/// Unpacks the files a package keeps from the `.tar.gz` `archive`, whose names start with one
/// directory, as hosts make them, into `dir`, which must not exist yet. The files go into a
/// directory beside it first, which is renamed, so `dir` is never half written.
pub fn unpack(archive: &[u8], dir: &Path) -> Result<(), String> {
    let parent = dir.parent().ok_or("the store has no directory")?;
    std::fs::create_dir_all(parent)
        .map_err(|e| format!("cannot make {}: {e}", parent.display()))?;
    let name = dir.file_name().unwrap_or_default().to_string_lossy();
    let temporary = parent.join(format!(".{name}.{}.partial", std::process::id()));
    let _ = std::fs::remove_dir_all(&temporary);
    let result = unpack_into(archive, &temporary).and_then(|()| {
        match std::fs::rename(&temporary, dir) {
            Ok(()) => Ok(()),
            // another stone got there first
            Err(_) if dir.join(MANIFEST).is_file() => Ok(()),
            Err(e) => Err(format!("cannot move it to {}: {e}", dir.display())),
        }
    });
    let _ = std::fs::remove_dir_all(&temporary);
    result
}

fn unpack_into(archive: &[u8], dir: &Path) -> Result<(), String> {
    std::fs::create_dir_all(dir).map_err(|e| format!("cannot make {}: {e}", dir.display()))?;
    let decoder = flate2::read::GzDecoder::new(archive).take(MAX_ARCHIVE);
    let mut archive = tar::Archive::new(decoder);
    let entries = archive.entries().map_err(|e| format!("bad archive: {e}"))?;
    for entry in entries {
        let mut entry = entry.map_err(|e| format!("bad archive: {e}"))?;
        if !entry.header().entry_type().is_file() {
            continue;
        }
        let path = entry
            .path()
            .map_err(|e| format!("bad archive: {e}"))?
            .into_owned();
        let Some(relative) = inside_top(&path) else {
            return Err(format!(
                "the archive holds '{}', which is outside its directory",
                path.display()
            ));
        };
        if relative.as_os_str().is_empty() || !is_kept(&relative) {
            continue;
        }
        let mut text = String::new();
        entry
            .read_to_string(&mut text)
            .map_err(|_| format!("'{}' is not text", relative.display()))?;
        let target = dir.join(&relative);
        if let Some(parent) = target.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| format!("cannot make {}: {e}", parent.display()))?;
        }
        std::fs::write(&target, text)
            .map_err(|e| format!("cannot write {}: {e}", target.display()))?;
    }
    Ok(())
}

/// Returns `path` without its first directory, or `None` if it leaves it, as `..` or an absolute
/// path would.
fn inside_top(path: &Path) -> Option<PathBuf> {
    let mut components = path.components();
    match components.next()? {
        Component::Normal(_) => {}
        _ => return None,
    }
    let mut relative = PathBuf::new();
    for component in components {
        match component {
            Component::Normal(name) => relative.push(name),
            Component::CurDir => {}
            _ => return None,
        }
    }
    Some(relative)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn archive(files: &[(&str, &str)]) -> Vec<u8> {
        let mut builder = tar::Builder::new(Vec::new());
        for (path, text) in files {
            let mut header = tar::Header::new_gnu();
            header.set_size(text.len() as u64);
            header.set_mode(0o644);
            header.set_entry_type(tar::EntryType::Regular);
            // set_path refuses '..', which an archive may still hold
            header.as_gnu_mut().unwrap().name[..path.len()].copy_from_slice(path.as_bytes());
            header.set_cksum();
            builder.append(&header, text.as_bytes()).unwrap();
        }
        let tar = builder.into_inner().unwrap();
        let mut encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::fast());
        std::io::Write::write_all(&mut encoder, &tar).unwrap();
        encoder.finish().unwrap()
    }

    fn scratch(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("stone-fetch-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        dir
    }

    #[test]
    fn an_archive_unpacks_only_the_files_a_package_keeps() {
        let dir = scratch("keeps");
        let data = archive(&[
            (
                "alice-json-abc123/stone.toml",
                "[package]\nname = \"json\"\n",
            ),
            (
                "alice-json-abc123/lib.stn",
                "pub fn one() -> int:\n    ret 1\n",
            ),
            (
                "alice-json-abc123/sub/two.stn",
                "pub fn two() -> int:\n    ret 2\n",
            ),
            ("alice-json-abc123/README.md", "# json\n"),
        ]);
        let target = dir.join("1.0.0");
        unpack(&data, &target).unwrap();
        let mut found: Vec<String> = vec![];
        for path in ["stone.toml", "lib.stn", "sub/two.stn", "README.md"] {
            if target.join(path).is_file() {
                found.push(path.to_string());
            }
        }
        assert_eq!(found, ["stone.toml", "lib.stn", "sub/two.stn"]);
        // nothing is left beside it
        let left: Vec<_> = std::fs::read_dir(&dir).unwrap().collect();
        assert_eq!(left.len(), 1);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn an_archive_cannot_write_outside_its_directory() {
        let dir = scratch("outside");
        let data = archive(&[("top/../../evil.stn", "x = 1\n")]);
        let error = unpack(&data, &dir.join("1.0.0")).unwrap_err();
        assert!(error.contains("outside its directory"), "{error}");
        assert!(!dir.join("1.0.0").exists());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn releases_are_read_from_either_hosts_answer() {
        let (releases, count) = parse_releases(
            r#"[{"tag_name": "v1.2.0", "draft": false, "prerelease": false},
                {"tag_name": "v1.3.0", "draft": true},
                {"tag_name": "nightly"},
                {"tag_name": "2.0.0-rc.1", "prerelease": true},
                {"tag_name": "v0.9.0", "upcoming_release": true}]"#,
        )
        .unwrap();
        assert_eq!(count, 5);
        let found: Vec<(String, String)> = releases
            .iter()
            .map(|r| (r.tag.clone(), r.version.to_string()))
            .collect();
        assert_eq!(
            found,
            [
                ("v1.2.0".to_string(), "1.2.0".to_string()),
                ("2.0.0-rc.1".to_string(), "2.0.0-rc.1".to_string())
            ]
        );
        assert!(parse_releases("{\"message\": \"Not Found\"}").is_err());
    }

    #[test]
    fn paths_and_tags_are_encoded() {
        assert_eq!(encode("group/sub/proj"), "group%2Fsub%2Fproj");
        assert_eq!(encode("v1.2.0-rc.1"), "v1.2.0-rc.1");
        assert_eq!(encode("a b"), "a%20b");
    }

    #[test]
    fn errors_list_the_releases_there_are() {
        let release = |v: &str| Release {
            tag: format!("v{v}"),
            version: Version::parse(v).unwrap(),
        };
        assert_eq!(
            listing(&[release("1.0.0"), release("1.3.0")]),
            " (its releases are 1.3.0, 1.0.0)"
        );
        assert_eq!(
            listing(&[]),
            " (it has no releases whose tags are versions, as v1.0.0)"
        );
    }
}

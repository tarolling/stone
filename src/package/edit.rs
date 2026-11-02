//! What `stone add` and `stone remove` do to a manifest: reading the package a command names, and
//! changing `[dependencies]` in place, so the rest of the file, comments included, stays as
//! written.
//!
//! For example, `stone add github:alice/json@1.2` names `alice/json` on GitHub and the newest
//! release 1.2.x, and adds `json = { github = "alice/json", version = "1.2.3" }` to the manifest.

use super::toml::{self, Value, quote};
use super::{Source, Version};
use crate::span::{FileId, Pos};

/// Which release a command asks for.
#[derive(Debug, Clone, PartialEq)]
pub enum Wanted {
    /// The newest release that is not a pre-release.
    Latest,
    /// The newest release whose version starts with these numbers, as `1.2` for the newest
    /// 1.2.x, or exactly this version, when it has three numbers.
    Matching { numbers: Vec<u64>, exact: Version },
}

impl Wanted {
    /// Reads what follows `@`: `latest` or a version, possibly short.
    ///
    /// For example, `Wanted::parse("1.2")` matches 1.2.0 and 1.2.7 but not 1.3.0.
    pub fn parse(text: &str) -> Result<Wanted, String> {
        if text == "latest" {
            return Ok(Wanted::Latest);
        }
        let exact = Version::parse(text)?;
        let written = text.split(['-', '+']).next().unwrap_or(text);
        let numbers = written.split('.').filter_map(|n| n.parse().ok()).collect();
        Ok(Wanted::Matching { numbers, exact })
    }

    /// Returns whether a release of `version` is what was asked for.
    pub fn matches(&self, version: &Version) -> bool {
        match self {
            Wanted::Latest => !version.is_pre(),
            Wanted::Matching { numbers, exact } if numbers.len() == 3 => version == exact,
            Wanted::Matching { numbers, .. } => {
                let parts = [version.major, version.minor, version.patch];
                !version.is_pre() && numbers.iter().zip(parts).all(|(n, part)| *n == part)
            }
        }
    }
}

/// Reads the package a command names: `github:owner/repo`, `github:host/owner/repo` for
/// GitHub Enterprise, `gitlab:group/project` (with `host` for another GitLab), or a link to one on
/// `github.com` or `gitlab.com`, each optionally followed by `@` and what [`Wanted::parse`] reads.
///
/// For example, `parse_spec("https://github.com/alice/json.git@latest", None)` is
/// `github:alice/json` at its latest release.
pub fn parse_spec(spec: &str, host: Option<&str>) -> Result<(Source, Wanted), String> {
    let (place, wanted) = match spec.rsplit_once('@') {
        Some((place, wanted)) => (place, Wanted::parse(wanted)?),
        None => (spec, Wanted::Latest),
    };
    let link = place
        .strip_prefix("https://")
        .or_else(|| place.strip_prefix("http://"));
    let (kind, path) = match link {
        Some(link) => {
            let link = link.trim_end_matches('/');
            let link = link.strip_suffix(".git").unwrap_or(link);
            match link.split_once('/') {
                Some(("github.com", path)) => ("github", path.to_string()),
                Some(("gitlab.com", path)) => ("gitlab", path.to_string()),
                Some((other, path)) => {
                    return Err(format!(
                        "say which host {other} is: github:{other}/{path} for GitHub Enterprise, \
                         or gitlab:{path} --host {other} for GitLab"
                    ));
                }
                None => return Err(format!("'{place}' does not name a repository")),
            }
        }
        None => match place.split_once(':') {
            Some((kind @ ("github" | "gitlab"), path)) => (kind, path.to_string()),
            _ => {
                return Err(format!(
                    "'{place}' does not name a package; write github:owner/repo or \
                     gitlab:group/project"
                ));
            }
        },
    };
    let segments: Vec<&str> = path.split('/').collect();
    let valid = segments.iter().all(|s| {
        !s.is_empty()
            && *s != "."
            && *s != ".."
            && s.bytes()
                .all(|b| b.is_ascii_alphanumeric() || b"-_.".contains(&b))
    });
    let source = match (kind, segments.as_slice()) {
        _ if !valid => return Err(format!("'{path}' is not a repository's path")),
        ("github", [_, _]) => Source::GitHub {
            host: host.map(str::to_string),
            repo: path,
        },
        ("github", [enterprise, owner, repo]) if host.is_none() => Source::GitHub {
            host: Some(enterprise.to_string()),
            repo: format!("{owner}/{repo}"),
        },
        ("github", _) => return Err("a GitHub repository is written as owner/repo".to_string()),
        (_, [_, _, ..]) => Source::GitLab {
            host: host.map(str::to_string),
            path,
        },
        _ => {
            return Err(
                "a GitLab project is written as group/project or group/subgroup/project"
                    .to_string(),
            );
        }
    };
    Ok((source, wanted))
}

/// Writes the inline table a manifest names a dependency with.
///
/// For example, `json` 1.2.0 from `github:alice/json` is
/// `{ github = "alice/json", version = "1.2.0" }`.
pub fn dependency_value(source: &Source, version: &Version) -> String {
    let (kind, host, path) = match source {
        Source::GitHub { host, repo } => ("github", host, repo.clone()),
        Source::GitLab { host, path } => ("gitlab", host, path.clone()),
        Source::Path(dir) => return format!("{{ path = {} }}", quote(&dir.display().to_string())),
    };
    let host = match host {
        Some(host) => format!(", host = {}", quote(host)),
        None => String::new(),
    };
    format!(
        "{{ {kind} = {}, version = {}{host} }}",
        quote(&path),
        quote(&version.to_string())
    )
}

/// Returns the manifest `text` with the dependency `name` set to `value`, replacing an entry of
/// that name where it is, or else adding one after the last of `[dependencies]`, which is added if
/// the manifest has none.
pub fn set_dependency(text: &str, name: &str, value: &str) -> Result<String, String> {
    let document = toml::parse(text, FileId(0)).map_err(|e| format!("stone.toml:{e}"))?;
    let line = format!("{} = {value}", bare_or_quoted(name));
    let Some(entry) = document.get("dependencies") else {
        let mut text = text.to_string();
        if !text.is_empty() && !text.ends_with('\n') {
            text.push('\n');
        }
        if !text.is_empty() {
            text.push('\n');
        }
        text.push_str(&format!("[dependencies]\n{line}\n"));
        return Ok(text);
    };
    let Value::Table(table) = &entry.item.value else {
        return Err("'dependencies' is not a table".to_string());
    };
    if let Some(existing) = table.get(name) {
        let start = offset(text, existing.key_span.start);
        let end = offset(text, existing.item.span.end);
        return Ok(format!("{}{line}{}", &text[..start], &text[end..]));
    }
    // after the last entry, or else the header
    let after = table
        .entries
        .last()
        .map_or(entry.item.span.end, |last| last.item.span.end);
    let at = match text[offset(text, after)..].find('\n') {
        Some(newline) => offset(text, after) + newline + 1,
        None => text.len(),
    };
    let mut changed = text[..at].to_string();
    if !changed.ends_with('\n') {
        changed.push('\n');
    }
    changed.push_str(&line);
    changed.push('\n');
    changed.push_str(&text[at..]);
    Ok(changed)
}

/// Returns the manifest `text` without the dependency `name`, whose lines are removed.
pub fn remove_dependency(text: &str, name: &str) -> Result<String, String> {
    let document = toml::parse(text, FileId(0)).map_err(|e| format!("stone.toml:{e}"))?;
    let entry = document
        .get("dependencies")
        .and_then(|deps| match &deps.item.value {
            Value::Table(table) => table.get(name),
            _ => None,
        })
        .ok_or_else(|| format!("there is no dependency named '{name}'"))?;
    let start = offset(text, Pos::new(entry.key_span.start.line, 1));
    let end = offset(text, entry.item.span.end);
    let end = match text[end..].find('\n') {
        Some(newline) => end + newline + 1,
        None => text.len(),
    };
    Ok(format!("{}{}", &text[..start], &text[end..]))
}

/// Writes a key bare if it can be, and quoted otherwise.
fn bare_or_quoted(key: &str) -> String {
    match key
        .bytes()
        .all(|b| b.is_ascii_alphanumeric() || b == b'_' || b == b'-')
        && !key.is_empty()
    {
        true => key.to_string(),
        false => quote(key),
    }
}

/// Returns the byte offset of `pos`, a line and a column of characters, in `text`.
fn offset(text: &str, pos: Pos) -> usize {
    let mut line = 1;
    let mut col = 1;
    for (i, c) in text.char_indices() {
        if line == pos.line && col == pos.col {
            return i;
        }
        if c == '\n' {
            if line == pos.line {
                return i;
            }
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    text.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn version(text: &str) -> Version {
        Version::parse(text).unwrap()
    }

    #[test]
    fn specs_name_a_host_a_repository_and_a_release() {
        let github = |repo: &str| Source::GitHub {
            host: None,
            repo: repo.to_string(),
        };
        assert_eq!(
            parse_spec("github:alice/json", None),
            Ok((github("alice/json"), Wanted::Latest))
        );
        assert_eq!(
            parse_spec("https://github.com/alice/json.git", None),
            Ok((github("alice/json"), Wanted::Latest))
        );
        let (source, wanted) =
            parse_spec("gitlab:group/sub/http@1.2", Some("git.corp.com")).unwrap();
        assert_eq!(source.to_string(), "gitlab:git.corp.com/group/sub/http");
        assert!(wanted.matches(&version("1.2.7")));
        assert!(!wanted.matches(&version("1.3.0")));
        assert!(!wanted.matches(&version("1.2.8-rc.1")));
        let (source, _) = parse_spec("github:ghe.corp.com/team/lib", None).unwrap();
        assert_eq!(source.to_string(), "github:ghe.corp.com/team/lib");
        let (_, wanted) = parse_spec("github:a/b@1.0.0-rc.1", None).unwrap();
        assert!(wanted.matches(&version("1.0.0-rc.1")));
        assert!(!Wanted::Latest.matches(&version("1.0.0-rc.1")));
    }

    #[test]
    fn bad_specs_say_how_to_write_one() {
        assert_eq!(
            parse_spec("alice/json", None).unwrap_err(),
            "'alice/json' does not name a package; write github:owner/repo or gitlab:group/project"
        );
        assert_eq!(
            parse_spec("https://git.corp.com/g/p", None).unwrap_err(),
            "say which host git.corp.com is: github:git.corp.com/g/p for GitHub Enterprise, or \
             gitlab:g/p --host git.corp.com for GitLab"
        );
        assert!(parse_spec("github:alice", None).is_err());
        assert!(parse_spec("github:a/../b", None).is_err());
        assert!(parse_spec("github:a/b@v1", None).is_err());
    }

    #[test]
    fn dependencies_are_written_as_inline_tables() {
        let source = Source::GitLab {
            host: Some("git.corp.com".into()),
            path: "g/p".into(),
        };
        assert_eq!(
            dependency_value(&source, &version("1.2")),
            "{ gitlab = \"g/p\", version = \"1.2.0\", host = \"git.corp.com\" }"
        );
    }

    #[test]
    fn a_dependency_is_added_after_the_others() {
        let text = "[package]\nname = \"app\"\n\n[dependencies]\n# geometry\ngeo = { path = \"../geo\" }\n\n[other]\n";
        assert_eq!(
            set_dependency(text, "json", "{ x = 1 }").unwrap(),
            "[package]\nname = \"app\"\n\n[dependencies]\n# geometry\ngeo = { path = \"../geo\" }\n\
             json = { x = 1 }\n\n[other]\n"
        );
        assert_eq!(
            set_dependency("[dependencies]\n", "json", "{ x = 1 }").unwrap(),
            "[dependencies]\njson = { x = 1 }\n"
        );
        assert_eq!(
            set_dependency("[package]\nname = \"app\"", "json", "{ x = 1 }").unwrap(),
            "[package]\nname = \"app\"\n\n[dependencies]\njson = { x = 1 }\n"
        );
        assert_eq!(
            set_dependency("", "json", "{ x = 1 }").unwrap(),
            "[dependencies]\njson = { x = 1 }\n"
        );
    }

    #[test]
    fn a_dependency_is_replaced_where_it_is() {
        let text = "[dependencies]\njson = { x = 1 }  # keep me\ngeo = { y = 2 }\n";
        assert_eq!(
            set_dependency(text, "json", "{ x = 2 }").unwrap(),
            "[dependencies]\njson = { x = 2 }  # keep me\ngeo = { y = 2 }\n"
        );
    }

    #[test]
    fn a_dependency_is_removed_with_its_line() {
        let text = "[dependencies]\njson = { x = 1 }\ngeo = { y = 2 }\n";
        assert_eq!(
            remove_dependency(text, "json").unwrap(),
            "[dependencies]\ngeo = { y = 2 }\n"
        );
        assert_eq!(
            remove_dependency(text, "http").unwrap_err(),
            "there is no dependency named 'http'"
        );
    }
}

//! `stone.lock`: what each package from a host held when it was first downloaded.
//!
//! Minimal version selection always picks the same versions from the same manifests (see
//! [`super::resolve`]), so the lockfile does not choose versions. It records a hash of each used
//! package's files, so a release whose tag later moves to other code fails to build rather than
//! changing the program.
//!
//! ```toml
//! [[package]]
//! name = "mime"
//! github = "alice/stone-mime"
//! version = "1.4.0"
//! hash = "sha256:4f1c..."
//! ```

use super::toml::{self, Table, Value, quote};
use super::{Source, Version};
use crate::diagnostic::Diagnostic;
use crate::explain::Code;
use crate::span::{FileId, Span};

/// The comment that starts every lockfile stone writes.
const HEADER: &str = "# written by stone: the hash of what each package downloaded held\n";

/// What a `stone.lock` says.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Lock {
    pub entries: Vec<Locked>,
}

/// One package of a lockfile.
#[derive(Debug, Clone, PartialEq)]
pub struct Locked {
    pub name: String,
    pub source: Source,
    pub version: Version,
    /// The hash of its files, as `sha256:` and hexadecimal digits (see
    /// [`super::store::content_hash`]).
    pub hash: String,
    /// The span of the hash, for errors, or empty for an entry not read from a file.
    pub span: Span,
}

impl Lock {
    /// Reads the lockfile `source` in `file`, returning what it could read and every error.
    pub fn parse(source: &str, file: FileId) -> (Lock, Vec<Diagnostic>) {
        let mut lock = Lock::default();
        let document = match toml::parse(source, file) {
            Ok(document) => document,
            Err(error) => return (lock, vec![error]),
        };
        let mut errors = vec![];
        for entry in &document.entries {
            match (&*entry.key, &entry.item.value) {
                ("package", Value::Array(items)) => {
                    for item in items {
                        match &item.value {
                            Value::Table(table) => match locked(table) {
                                Ok(locked) => lock.entries.push(locked),
                                Err(message) => {
                                    errors.push(Code::Manifest.error(item.span, message));
                                }
                            },
                            _ => errors.push(Code::Manifest.error(item.span, "expected a table")),
                        }
                    }
                }
                _ => errors.push(Code::Manifest.error(
                    entry.key_span,
                    "a lockfile holds only [[package]] tables, which stone writes",
                )),
            }
        }
        (lock, errors)
    }

    /// Returns the entry for `version` of the package from `source`, if there is one.
    pub fn find(&self, source: &Source, version: &Version) -> Option<&Locked> {
        self.entries
            .iter()
            .find(|e| e.source == *source && e.version == *version)
    }

    /// Writes the lockfile, its entries sorted by name, source, and version, so the same
    /// packages always give the same text.
    pub fn write(&self) -> String {
        let mut entries: Vec<&Locked> = self.entries.iter().collect();
        entries.sort_by(|a, b| {
            (&a.name, a.source.to_string(), &a.version).cmp(&(
                &b.name,
                b.source.to_string(),
                &b.version,
            ))
        });
        let mut text = HEADER.to_string();
        for entry in entries {
            text.push_str("\n[[package]]\n");
            text.push_str(&format!("name = {}\n", quote(&entry.name)));
            let (kind, host, path) = match &entry.source {
                Source::GitHub { host, repo } => ("github", host, repo),
                Source::GitLab { host, path } => ("gitlab", host, path),
                Source::Path(_) => unreachable!("a directory is never locked"),
            };
            text.push_str(&format!("{kind} = {}\n", quote(path)));
            if let Some(host) = host {
                text.push_str(&format!("host = {}\n", quote(host)));
            }
            text.push_str(&format!(
                "version = {}\n",
                quote(&entry.version.to_string())
            ));
            text.push_str(&format!("hash = {}\n", quote(&entry.hash)));
        }
        text
    }
}

/// Reads one `[[package]]` table.
fn locked(table: &Table) -> Result<Locked, String> {
    let text = |key: &str| match table.get(key).map(|e| &e.item.value) {
        Some(Value::Str(text)) => Ok(Some(text.clone())),
        Some(_) => Err(format!("'{key}' is a string")),
        None => Ok(None),
    };
    let required = |key: &str| text(key)?.ok_or_else(|| format!("a package names its {key}"));
    let host = text("host")?;
    let source = match (text("github")?, text("gitlab")?) {
        (Some(repo), None) => Source::GitHub { host, repo },
        (None, Some(path)) => Source::GitLab { host, path },
        _ => return Err("a package comes from one of github and gitlab".to_string()),
    };
    let hash = required("hash")?;
    if !is_hash(&hash) {
        return Err(format!(
            "'{hash}' is not a hash, written as 'sha256:' and 64 digits"
        ));
    }
    Ok(Locked {
        name: required("name")?,
        source,
        version: Version::parse(&required("version")?)?,
        hash,
        span: table.get("hash").map_or(Span::default(), |e| e.item.span),
    })
}

/// Returns whether `text` is a hash as lockfiles write them.
fn is_hash(text: &str) -> bool {
    text.strip_prefix("sha256:")
        .is_some_and(|hex| hex.len() == 64 && hex.bytes().all(|b| b.is_ascii_hexdigit()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(name: &str, source: Source, version: &str) -> Locked {
        Locked {
            name: name.to_string(),
            source,
            version: Version::parse(version).unwrap(),
            hash: format!("sha256:{}", "ab".repeat(32)),
            span: Span::default(),
        }
    }

    #[test]
    fn a_lock_reads_back_what_it_writes() {
        let lock = Lock {
            entries: vec![
                entry(
                    "mime",
                    Source::GitHub {
                        host: None,
                        repo: "alice/mime".into(),
                    },
                    "1.4.0",
                ),
                entry(
                    "http",
                    Source::GitLab {
                        host: Some("git.corp.com".into()),
                        path: "group/http".into(),
                    },
                    "0.4.1",
                ),
            ],
        };
        let text = lock.write();
        assert!(text.starts_with(HEADER));
        // sorted by name
        assert!(text.find("http").unwrap() < text.find("mime").unwrap());
        let (read, errors) = Lock::parse(&text, FileId(1));
        assert_eq!(errors, []);
        let without_spans = |lock: &Lock| {
            let mut entries: Vec<(String, String)> = lock
                .entries
                .iter()
                .map(|e| {
                    (
                        e.name.clone(),
                        format!("{} {} {}", e.source, e.version, e.hash),
                    )
                })
                .collect();
            entries.sort();
            entries
        };
        assert_eq!(without_spans(&read), without_spans(&lock));
        assert_eq!(read.write(), text);
    }

    #[test]
    fn bad_entries_are_errors() {
        let message = |text: &str| {
            let (_, errors) = Lock::parse(text, FileId(1));
            errors[0].message.clone()
        };
        assert_eq!(
            message(
                "[[package]]\nname = \"a\"\ngithub = \"o/a\"\nversion = \"1.0\"\nhash = \"x\"\n"
            ),
            "'x' is not a hash, written as 'sha256:' and 64 digits"
        );
        assert_eq!(
            message("[package]\n"),
            "a lockfile holds only [[package]] tables, which stone writes"
        );
        assert_eq!(
            message("[[package]]\nname = \"a\"\nversion = \"1.0\"\n"),
            "a package comes from one of github and gitlab"
        );
    }
}

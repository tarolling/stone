//! Packages: programs and libraries that depend on others, named in a `stone.toml` manifest.
//!
//! A package is a directory with a `stone.toml`. A program's manifest sits next to its entry
//! file, and each of its `[dependencies]` names a package by the name its files use it under:
//!
//! ```toml
//! [package]
//! name = "app"
//! version = "0.1.0"
//!
//! [dependencies]
//! geo = { path = "../geo" }
//! json = { github = "alice/stone-json", version = "1.2.0" }
//! ```
//!
//! With that, `use geo.shapes` loads `shapes.stn` from the `geo` package's directory, and
//! `use geo` loads its `lib.stn`. [`crate::project`] links a package's files like any other
//! modules, under names that start with the package's own name and its versions' compatibility
//! class (see [`Version::class`]), so two packages never collide.

pub mod lock;
pub mod resolve;
pub mod store;
pub mod toml;
pub mod version;

use std::fmt;
use std::path::{Path, PathBuf};

use crate::diagnostic::Diagnostic;
use crate::span::{FileId, Span};
use crate::token::RESERVED_KEYWORDS;
use toml::{Entry, Table, Value};
pub use version::Version;

/// The manifest's file name, in a package's directory.
pub const MANIFEST: &str = "stone.toml";

/// The lockfile's file name, next to the manifest.
pub const LOCK: &str = "stone.lock";

/// The file a package's `use` of the package's own name loads, as `use geo` loads `geo`'s.
pub const LIB: &str = "lib";

/// What a `stone.toml` says.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Manifest {
    /// The package's name, which a library must have.
    pub name: Option<(String, Span)>,
    pub version: Option<(Version, Span)>,
    /// The oldest stone the package works with.
    pub stone: Option<(Version, Span)>,
    pub dependencies: Vec<Dependency>,
}

/// One entry of `[dependencies]`.
#[derive(Debug, Clone, PartialEq)]
pub struct Dependency {
    /// The name the package's files use it under, which is the entry's key.
    pub name: String,
    /// The span of the key.
    pub span: Span,
    pub source: Source,
    /// The oldest version to use, for a package from a host.
    pub version: Option<(Version, Span)>,
}

/// Where a dependency comes from.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Source {
    /// A repository on GitHub, as `owner/repo`, whose releases are its versions.
    GitHub { host: Option<String>, repo: String },
    /// A project on GitLab, as `group/project` or `group/subgroup/project`.
    GitLab { host: Option<String>, path: String },
    /// A directory, relative to the manifest's.
    Path(PathBuf),
}

impl fmt::Display for Source {
    /// Writes the source as `stone add` reads it, such as `github:alice/json`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Source::GitHub { host: None, repo } => write!(f, "github:{repo}"),
            Source::GitHub {
                host: Some(host),
                repo,
            } => write!(f, "github:{host}/{repo}"),
            Source::GitLab { host: None, path } => write!(f, "gitlab:{path}"),
            Source::GitLab {
                host: Some(host),
                path,
            } => write!(f, "gitlab:{host}/{path}"),
            Source::Path(path) => write!(f, "path:{}", path.display()),
        }
    }
}

impl Manifest {
    /// Reads the manifest `source` in `file`, returning what it could read and every error.
    ///
    /// For example, a manifest holding `[dependencies]\ngeo = { path = "../geo" }\n` has one
    /// dependency, `geo`, from the directory `../geo`.
    pub fn parse(source: &str, file: FileId) -> (Manifest, Vec<Diagnostic>) {
        let mut manifest = Manifest::default();
        let mut errors = vec![];
        let document = match toml::parse(source, file) {
            Ok(document) => document,
            Err(error) => return (manifest, vec![error]),
        };
        for entry in &document.entries {
            match (entry.key.as_str(), &entry.item.value) {
                ("package", Value::Table(table)) => manifest.package(table, &mut errors),
                ("dependencies", Value::Table(table)) => {
                    for entry in &table.entries {
                        if let Some(dependency) = dependency(entry, &mut errors) {
                            manifest.dependencies.push(dependency);
                        }
                    }
                }
                ("package" | "dependencies", _) => errors.push(Diagnostic::error(
                    entry.key_span,
                    format!("'{}' is a table, written as '[{}]'", entry.key, entry.key),
                )),
                (key, _) => errors.push(Diagnostic::error(
                    entry.key_span,
                    format!("unknown key '{key}'; a manifest holds [package] and [dependencies]"),
                )),
            }
        }
        (manifest, errors)
    }

    fn package(&mut self, table: &Table, errors: &mut Vec<Diagnostic>) {
        for entry in &table.entries {
            let span = entry.item.span;
            match entry.key.as_str() {
                "name" => {
                    if let Some(name) = string(entry, errors) {
                        match identifier_error(&name, "a package's name") {
                            Some(message) => errors.push(Diagnostic::error(span, message)),
                            None => self.name = Some((name, span)),
                        }
                    }
                }
                "version" => self.version = version(entry, errors),
                "stone" => {
                    self.stone = version(entry, errors);
                    if let Some((needed, span)) = &self.stone
                        && let Ok(running) = Version::parse(env!("CARGO_PKG_VERSION"))
                        && *needed > running
                    {
                        errors.push(Diagnostic::error(
                            *span,
                            format!("this package needs stone {needed} or newer, and this is stone {running}"),
                        ));
                    }
                }
                "description" | "license" => {
                    string(entry, errors);
                }
                key => errors.push(Diagnostic::error(
                    entry.key_span,
                    format!(
                        "unknown key '{key}' in [package], which holds name, version, stone, \
                         description, and license"
                    ),
                )),
            }
        }
    }
}

/// Reads one entry of `[dependencies]`.
fn dependency(entry: &Entry, errors: &mut Vec<Diagnostic>) -> Option<Dependency> {
    let Value::Table(table) = &entry.item.value else {
        errors.push(Diagnostic::error(
            entry.item.span,
            format!(
                "write the dependency as {{ github = \"owner/repo\", version = \"1.0.0\" }}, \
                 {{ gitlab = \"group/project\", version = \"1.0.0\" }}, or {{ path = \"../{}\" }}",
                entry.key
            ),
        ));
        return None;
    };
    if let Some(message) = dependency_name_error(&entry.key) {
        errors.push(Diagnostic::error(entry.key_span, message));
        return None;
    }
    let mut sources = vec![];
    let mut host = None;
    let mut version_found = None;
    let before = errors.len();
    for field in &table.entries {
        match field.key.as_str() {
            "github" | "gitlab" | "path" => {
                if let Some(text) = string(field, errors) {
                    sources.push((field.key.clone(), text, field.item.span));
                }
            }
            "host" => {
                if let Some(text) = string(field, errors) {
                    let valid = !text.is_empty()
                        && text
                            .bytes()
                            .all(|b| b.is_ascii_alphanumeric() || b"-.:".contains(&b));
                    match valid {
                        true => host = Some((text, field.key_span)),
                        false => errors.push(Diagnostic::error(
                            field.item.span,
                            "a host is a name such as 'gitlab.example.com', without 'https://'",
                        )),
                    }
                }
            }
            "version" => version_found = Some((version(field, errors), field.key_span)),
            key => errors.push(Diagnostic::error(
                field.key_span,
                format!(
                    "unknown key '{key}' in a dependency, which holds github, gitlab, or path, \
                     and version and host"
                ),
            )),
        }
    }
    if errors.len() > before {
        return None;
    }
    let (kind, text, span) = match sources.as_slice() {
        [one] => one.clone(),
        [] => {
            errors.push(Diagnostic::error(
                entry.item.span,
                "a dependency names where it comes from with github, gitlab, or path",
            ));
            return None;
        }
        [_, (_, _, span), ..] => {
            errors.push(Diagnostic::error(
                *span,
                "a dependency comes from one place, so give one of github, gitlab, and path",
            ));
            return None;
        }
    };
    let source = match kind.as_str() {
        "github" => {
            let segments: Vec<&str> = text.split('/').collect();
            if segments.len() != 2 || !segments.iter().all(|s| is_repository_name(s)) {
                errors.push(Diagnostic::error(
                    span,
                    "a GitHub repository is written as 'owner/repo'",
                ));
                return None;
            }
            Source::GitHub {
                host: host.as_ref().map(|(h, _)| h.clone()),
                repo: text,
            }
        }
        "gitlab" => {
            let segments: Vec<&str> = text.split('/').collect();
            if segments.len() < 2 || !segments.iter().all(|s| is_repository_name(s)) {
                errors.push(Diagnostic::error(
                    span,
                    "a GitLab project is written as 'group/project' or 'group/subgroup/project'",
                ));
                return None;
            }
            Source::GitLab {
                host: host.as_ref().map(|(h, _)| h.clone()),
                path: text,
            }
        }
        _ => {
            if let Some((_, span)) = host {
                errors.push(Diagnostic::error(span, "a path dependency has no host"));
                return None;
            }
            if let Some((_, span)) = version_found {
                errors.push(Diagnostic::error(
                    span,
                    "a path dependency uses the directory as it is, so it has no version",
                ));
                return None;
            }
            if text.is_empty() {
                errors.push(Diagnostic::error(span, "a path names a directory"));
                return None;
            }
            Source::Path(PathBuf::from(text))
        }
    };
    let version = match version_found {
        Some((version, _)) => version,
        None if !matches!(source, Source::Path(_)) => {
            errors.push(Diagnostic::error(
                entry.item.span,
                "a dependency from a host gives the oldest version it works with, as \
                 version = \"1.0.0\"",
            ));
            return None;
        }
        None => None,
    };
    Some(Dependency {
        name: entry.key.clone(),
        span: entry.key_span,
        source,
        version,
    })
}

/// Reads an entry's string, reporting any other value.
fn string(entry: &Entry, errors: &mut Vec<Diagnostic>) -> Option<String> {
    match &entry.item.value {
        Value::Str(text) => Some(text.clone()),
        other => {
            errors.push(Diagnostic::error(
                entry.item.span,
                format!("'{}' is a string, not {}", entry.key, other.kind()),
            ));
            None
        }
    }
}

/// Reads an entry's version, reporting anything else.
fn version(entry: &Entry, errors: &mut Vec<Diagnostic>) -> Option<(Version, Span)> {
    let text = string(entry, errors)?;
    match Version::parse(&text) {
        Ok(version) => Some((version, entry.item.span)),
        Err(message) => {
            errors.push(Diagnostic::error(entry.item.span, message));
            None
        }
    }
}

/// Returns why `name` cannot be a package's name, called `what` in the message, if it cannot:
/// it must be a stone name, since modules are named after it.
///
/// For example, `identifier_error("my-geo", "a package's name")` says to use `_`.
pub fn identifier_error(name: &str, what: &str) -> Option<String> {
    let mut chars = name.chars();
    let valid = chars
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_');
    if !valid {
        let hint = match name.contains('-') {
            true => format!(", so write '{}'", name.replace('-', "_")),
            false => String::new(),
        };
        return Some(format!(
            "'{name}' cannot be {what}, which is a name of letters, digits, and '_'{hint}"
        ));
    }
    if RESERVED_KEYWORDS.contains(&name) {
        return Some(format!("'{name}' is a keyword, so it cannot be {what}"));
    }
    None
}

/// Returns why a dependency cannot be named `name`, if it cannot: it must be a stone name that no
/// builtin module, the prelude, or a builtin function takes.
///
/// For example, `dependency_name_error("os")` says `os` is a builtin module.
pub fn dependency_name_error(name: &str) -> Option<String> {
    if let Some(message) = identifier_error(name, "a dependency's name") {
        return Some(message);
    }
    let builtin_module = crate::stdlib::module(name).is_some()
        || crate::stdlib::source_module(name).is_some()
        || crate::stdlib::is_internal_module(name);
    if builtin_module {
        return Some(format!(
            "'{name}' is a builtin module, so give the dependency another name"
        ));
    }
    if name == crate::project::PRELUDE_MODULE {
        return Some(format!(
            "'{name}' is the name of stone's prelude, so give the dependency another name"
        ));
    }
    if crate::stdlib::is_reserved(name) {
        return Some(format!(
            "'{name}' is a builtin, so give the dependency another name"
        ));
    }
    None
}

/// Returns whether `name` may be a segment of a repository's path on a host.
fn is_repository_name(name: &str) -> bool {
    !name.is_empty()
        && name != "."
        && name != ".."
        && name
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || b"-_.".contains(&b))
}

/// Joins `path` onto `dir` and removes `.` and `..` components without reading the filesystem,
/// so the same directory reached two ways is one package, in memory as on disk.
///
/// For example, `join_normal("app", "../geo")` is `geo`, and `join_normal("", "../geo")` is
/// `../geo`.
pub fn join_normal(dir: &Path, path: &Path) -> PathBuf {
    use std::path::Component;
    let joined = dir.join(path);
    let mut parts: Vec<Component> = vec![];
    for component in joined.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => match parts.last() {
                Some(Component::Normal(_)) => {
                    parts.pop();
                }
                Some(Component::RootDir | Component::Prefix(_)) => {}
                _ => parts.push(component),
            },
            other => parts.push(other),
        }
    }
    parts.iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read(source: &str) -> Manifest {
        let (manifest, errors) = Manifest::parse(source, FileId(1));
        assert_eq!(errors, [], "should have no errors");
        manifest
    }

    fn errors(source: &str) -> Vec<(String, usize, usize)> {
        Manifest::parse(source, FileId(1))
            .1
            .into_iter()
            .map(|e| (e.message, e.span.start.line, e.span.start.col))
            .collect()
    }

    fn message(source: &str) -> String {
        let errors = errors(source);
        assert_eq!(errors.len(), 1, "one error: {errors:?}");
        errors[0].0.clone()
    }

    #[test]
    fn a_manifest_names_its_package_and_dependencies() {
        let manifest = read(
            "[package]\nname = \"app\"\nversion = \"0.1\"\n\n[dependencies]\n\
             geo = { path = \"../geo\" }\n\
             json = { github = \"alice/stone-json\", version = \"1.2.0\" }\n\
             http = { gitlab = \"group/sub/http\", version = \"0.4\", host = \"git.corp.com\" }\n",
        );
        assert_eq!(manifest.name.unwrap().0, "app");
        assert_eq!(manifest.version.unwrap().0, Version::new(0, 1, 0));
        let deps: Vec<(String, String, Option<String>)> = manifest
            .dependencies
            .iter()
            .map(|d| {
                (
                    d.name.clone(),
                    d.source.to_string(),
                    d.version.as_ref().map(|(v, _)| v.to_string()),
                )
            })
            .collect();
        assert_eq!(
            deps,
            [
                ("geo".into(), "path:../geo".into(), None),
                (
                    "json".into(),
                    "github:alice/stone-json".into(),
                    Some("1.2.0".into())
                ),
                (
                    "http".into(),
                    "gitlab:git.corp.com/group/sub/http".into(),
                    Some("0.4.0".into())
                ),
            ]
        );
    }

    #[test]
    fn an_empty_manifest_is_valid() {
        assert_eq!(read(""), Manifest::default());
    }

    #[test]
    fn manifest_errors_point_at_their_keys_and_values() {
        assert_eq!(
            errors("[package]\nname = \"my-geo\"\n"),
            [(
                "'my-geo' cannot be a package's name, which is a name of letters, digits, and \
                 '_', so write 'my_geo'"
                    .to_string(),
                2,
                8
            )]
        );
        assert_eq!(
            errors("[dependecies]\n"),
            [(
                "unknown key 'dependecies'; a manifest holds [package] and [dependencies]"
                    .to_string(),
                1,
                2
            )]
        );
        assert_eq!(
            message("[package]\nversion = \"v1.0\"\n"),
            "'v1.0' is not a version; write it without the 'v', as '1.0'"
        );
        assert_eq!(
            message("[package]\nname = \"fn\"\n"),
            "'fn' is a keyword, so it cannot be a package's name"
        );
        assert_eq!(
            message("[package]\nauthor = \"me\"\n"),
            "unknown key 'author' in [package], which holds name, version, stone, description, \
             and license"
        );
    }

    #[test]
    fn dependency_errors_explain_the_forms() {
        assert_eq!(
            message("[dependencies]\njson = \"1.0\"\n"),
            "write the dependency as { github = \"owner/repo\", version = \"1.0.0\" }, \
             { gitlab = \"group/project\", version = \"1.0.0\" }, or { path = \"../json\" }"
        );
        assert_eq!(
            message("[dependencies]\njson = { github = \"alice/json\" }\n"),
            "a dependency from a host gives the oldest version it works with, as \
             version = \"1.0.0\""
        );
        assert_eq!(
            message("[dependencies]\njson = { github = \"json\", version = \"1\" }\n"),
            "a GitHub repository is written as 'owner/repo'"
        );
        assert_eq!(
            message("[dependencies]\njson = { path = \"../json\", version = \"1\" }\n"),
            "a path dependency uses the directory as it is, so it has no version"
        );
        assert_eq!(
            message(
                "[dependencies]\njson = { path = \"../json\", github = \"a/b\", version = \"1\" }\n"
            ),
            "a dependency comes from one place, so give one of github, gitlab, and path"
        );
        assert_eq!(
            message("[dependencies]\njson = { version = \"1\" }\n"),
            "a dependency names where it comes from with github, gitlab, or path"
        );
        assert_eq!(
            message("[dependencies]\nos = { path = \"../os\" }\n"),
            "'os' is a builtin module, so give the dependency another name"
        );
        assert_eq!(
            message("[dependencies]\nprint = { path = \"../p\" }\n"),
            "'print' is a builtin, so give the dependency another name"
        );
        assert_eq!(
            message("[dependencies]\nprelude = { path = \"../p\" }\n"),
            "'prelude' is the name of stone's prelude, so give the dependency another name"
        );
    }

    #[test]
    fn a_package_may_need_a_newer_stone() {
        assert_eq!(
            message("[package]\nstone = \"999.0\"\n"),
            format!(
                "this package needs stone 999.0.0 or newer, and this is stone {}",
                env!("CARGO_PKG_VERSION")
            )
        );
        read("[package]\nstone = \"0.1\"\n");
    }

    #[test]
    fn paths_join_without_dot_components() {
        let join = |dir: &str, path: &str| join_normal(Path::new(dir), Path::new(path));
        assert_eq!(join("app", "../geo"), PathBuf::from("geo"));
        assert_eq!(join("", "../geo"), PathBuf::from("../geo"));
        assert_eq!(join("a/b", "./../../c/./d"), PathBuf::from("c/d"));
        assert_eq!(join("/x", "../../y"), PathBuf::from("/y"));
    }
}

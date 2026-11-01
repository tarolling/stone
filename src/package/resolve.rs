//! Minimal version selection, which picks the version of every package a program uses.
//!
//! Each dependency gives the oldest version of a package it works with. The version used is the
//! newest of those any package in the graph asks for, among the versions of the same
//! compatibility class (see [`Version::class`]), which are different packages. Selection never
//! asks a host which versions exist, so it never picks one that no manifest names, and the same
//! manifests always give the same versions. This is Go's algorithm: every (package, version)
//! reachable from the program is read, the versions of a package that are not chosen included,
//! since they may still raise what another package needs.
//!
//! For example, if the program needs `a` 1.0 and `b` 1.0, and `b` 1.0 needs `a` 1.2, then `a`
//! 1.2 is used. A release `a` 1.5 that nothing names is not.

use std::collections::{HashMap, HashSet, VecDeque};
use std::path::PathBuf;

use super::{Source, Version};
use crate::span::Span;

/// What a dependency names: a directory, or the oldest version of a package from a host.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Target {
    Path(PathBuf),
    Remote(Source, Version),
}

impl Target {
    /// Returns the package the target is a version of: a directory, or a source with a
    /// compatibility class, so `json` 1.x and 2.x are two packages.
    pub fn identity(&self) -> Identity {
        match self {
            Target::Path(dir) => Identity::Path(dir.clone()),
            Target::Remote(source, version) => Identity::Remote(source.clone(), version.class()),
        }
    }
}

/// A package, whatever its version.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Identity {
    Path(PathBuf),
    Remote(Source, String),
}

/// One dependency of a package, by the name its files use it under.
#[derive(Debug, Clone, PartialEq)]
pub struct Requirement {
    pub name: String,
    /// The span of the dependency's key in its manifest.
    pub span: Span,
    pub target: Target,
}

/// The versions selection chose, and what it read on the way.
#[derive(Debug, Default)]
pub struct Selection {
    /// Every target read, in the order first reached, breadth first from the program.
    pub order: Vec<Target>,
    /// Each target's requirements, for the targets whose manifests could be read.
    pub requirements: HashMap<Target, Vec<Requirement>>,
    /// The version chosen for each package from a host.
    chosen: HashMap<Identity, Version>,
}

impl Selection {
    /// Returns the version of `target`'s package that is used: `target` itself for a directory,
    /// and the chosen version for a package from a host.
    pub fn resolve(&self, target: &Target) -> Target {
        match target {
            Target::Path(_) => target.clone(),
            Target::Remote(source, version) => {
                let chosen = self.chosen.get(&target.identity()).unwrap_or(version);
                Target::Remote(source.clone(), chosen.clone())
            }
        }
    }

    /// Returns every target that is used, in the order first reached.
    pub fn used(&self) -> impl Iterator<Item = &Target> {
        self.order.iter().filter(|t| self.resolve(t) == **t)
    }
}

/// Selects the versions of every package reachable from `root`, the program's requirements,
/// asking `requirements` for each target's own, which it gives as `None` if it cannot read them,
/// as for a version that is not downloaded.
///
/// For example, with `root` needing `a` 1.0 and `b` 1.0, and `b` 1.0 needing `a` 1.2,
/// `resolve` of `a` 1.0 is `a` 1.2.
pub fn select(
    root: &[Requirement],
    mut requirements: impl FnMut(&Target) -> Option<Vec<Requirement>>,
) -> Selection {
    let mut selection = Selection::default();
    let mut seen = HashSet::new();
    let mut queue: VecDeque<Target> = root.iter().map(|r| r.target.clone()).collect();
    while let Some(target) = queue.pop_front() {
        if !seen.insert(target.clone()) {
            continue;
        }
        if let Target::Remote(_, version) = &target {
            let chosen = selection
                .chosen
                .entry(target.identity())
                .or_insert_with(|| version.clone());
            if version > chosen {
                *chosen = version.clone();
            }
        }
        selection.order.push(target.clone());
        if let Some(found) = requirements(&target) {
            queue.extend(found.iter().map(|r| r.target.clone()));
            selection.requirements.insert(target, found);
        }
    }
    selection
}

#[cfg(test)]
mod tests {
    use super::*;

    fn source(name: &str) -> Source {
        Source::GitHub {
            host: None,
            repo: format!("o/{name}"),
        }
    }

    fn remote(name: &str, version: &str) -> Target {
        Target::Remote(source(name), Version::parse(version).unwrap())
    }

    fn requirement(name: &str, version: &str) -> Requirement {
        Requirement {
            name: name.to_string(),
            span: Span::default(),
            target: remote(name, version),
        }
    }

    /// A manifest of a test registry: a package's name, its version, and its dependencies with
    /// their versions.
    type Manifest<'a> = (&'a str, &'a str, &'a [(&'a str, &'a str)]);

    /// Selects from a registry of manifests, returning each used package as `name version`, in
    /// order.
    fn used(root: &[(&str, &str)], registry: &[Manifest]) -> Vec<String> {
        let root: Vec<Requirement> = root.iter().map(|(n, v)| requirement(n, v)).collect();
        let selection = select(&root, |target| {
            registry.iter().find_map(|(name, version, deps)| {
                (remote(name, version) == *target)
                    .then(|| deps.iter().map(|(n, v)| requirement(n, v)).collect())
            })
        });
        selection
            .used()
            .map(|target| match target {
                Target::Remote(Source::GitHub { repo, .. }, version) => {
                    format!("{} {version}", &repo[2..])
                }
                other => panic!("{other:?}"),
            })
            .collect()
    }

    #[test]
    fn the_newest_version_asked_for_is_used() {
        assert_eq!(
            used(
                &[("a", "1.0"), ("b", "1.0")],
                &[
                    ("a", "1.0.0", &[]),
                    ("a", "1.2.0", &[]),
                    ("a", "1.5.0", &[]),
                    ("b", "1.0.0", &[("a", "1.2")]),
                ],
            ),
            ["b 1.0.0", "a 1.2.0"]
        );
    }

    #[test]
    fn a_diamond_meets_at_the_newer_version() {
        assert_eq!(
            used(
                &[("b", "1.0"), ("c", "1.0")],
                &[
                    ("b", "1.0.0", &[("d", "1.1")]),
                    ("c", "1.0.0", &[("d", "1.3")]),
                    ("d", "1.1.0", &[]),
                    ("d", "1.3.0", &[]),
                ],
            ),
            ["b 1.0.0", "c 1.0.0", "d 1.3.0"]
        );
    }

    #[test]
    fn a_version_that_is_not_used_still_raises_others() {
        // c 1.0 is replaced by c 1.1, but what c 1.0 needs still counts, as in Go
        assert_eq!(
            used(
                &[("b", "1.0"), ("c", "1.0")],
                &[
                    ("b", "1.0.0", &[("c", "1.1")]),
                    ("c", "1.0.0", &[("d", "1.2")]),
                    ("c", "1.1.0", &[("d", "1.0")]),
                    ("d", "1.0.0", &[]),
                    ("d", "1.2.0", &[]),
                ],
            ),
            ["b 1.0.0", "c 1.1.0", "d 1.2.0"]
        );
    }

    #[test]
    fn major_versions_are_different_packages() {
        assert_eq!(
            used(
                &[("a", "1.4"), ("b", "1.0")],
                &[
                    ("a", "1.4.0", &[]),
                    ("a", "2.1.0", &[]),
                    ("b", "1.0.0", &[("a", "2.1")]),
                ],
            ),
            ["a 1.4.0", "b 1.0.0", "a 2.1.0"]
        );
        // before 1.0, each minor version is its own class
        assert_eq!(
            used(
                &[("a", "0.3"), ("a", "0.4")],
                &[("a", "0.3.0", &[]), ("a", "0.4.0", &[])]
            ),
            ["a 0.3.0", "a 0.4.0"]
        );
    }

    #[test]
    fn cycles_end() {
        assert_eq!(
            used(
                &[("a", "1.0")],
                &[
                    ("a", "1.0.0", &[("b", "1.0")]),
                    ("b", "1.0.0", &[("a", "1.1")]),
                    ("a", "1.1.0", &[("b", "1.0")]),
                ],
            ),
            ["b 1.0.0", "a 1.1.0"]
        );
    }

    #[test]
    fn a_package_that_cannot_be_read_is_still_selected() {
        let root = [requirement("a", "1.0")];
        let selection = select(&root, |_| None);
        assert_eq!(selection.used().count(), 1);
        assert!(selection.requirements.is_empty());
    }
}

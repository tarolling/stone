//! Semantic versions, as packages and their release tags are numbered.
//!
//! For example, `1.2.0` is newer than `1.1.9` and than `1.2.0-beta.1`, and is compatible with
//! `1.0.0`, while `0.4.2` is compatible with `0.4.0` but not with `0.5.0`.

use std::cmp::Ordering;
use std::fmt;

/// A version `major.minor.patch`, possibly with a pre-release such as `-beta.1`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Version {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
    /// The pre-release identifiers after `-`, such as `["beta", "1"]`, or empty for a release.
    pub pre: Vec<String>,
}

impl Version {
    pub fn new(major: u64, minor: u64, patch: u64) -> Version {
        Version {
            major,
            minor,
            patch,
            pre: vec![],
        }
    }

    /// Reads a version written as `1`, `1.2`, or `1.2.3`, optionally followed by a pre-release
    /// such as `-beta.1` and build metadata such as `+linux`, which is ignored. A missing minor or
    /// patch number is 0.
    ///
    /// For example, `Version::parse("1.2")` is `1.2.0`, and `Version::parse("v1.2")` fails, since
    /// the `v` belongs to tags, not versions.
    pub fn parse(text: &str) -> Result<Version, String> {
        let text = text.split_once('+').map_or(text, |(version, _)| version);
        let (numbers, pre) = match text.split_once('-') {
            Some((numbers, pre)) => (numbers, Some(pre)),
            None => (text, None),
        };
        if text.starts_with(['v', 'V']) {
            return Err(format!(
                "'{text}' is not a version; write it without the 'v', as '{}'",
                &text[1..]
            ));
        }
        let parts: Vec<&str> = numbers.split('.').collect();
        if parts.len() > 3 {
            return Err(format!(
                "'{text}' is not a version, which has at most three numbers"
            ));
        }
        let mut values = [0u64; 3];
        for (value, part) in values.iter_mut().zip(&parts) {
            let valid = !part.is_empty()
                && part.bytes().all(|b| b.is_ascii_digit())
                && (part.len() == 1 || !part.starts_with('0'));
            *value = match part.parse() {
                Ok(n) if valid => n,
                _ => {
                    return Err(format!(
                        "'{text}' is not a version, which is numbers separated by '.', as '1.2.0'"
                    ));
                }
            };
        }
        let pre = match pre {
            None => vec![],
            Some(pre) => {
                let identifiers: Vec<String> = pre.split('.').map(str::to_string).collect();
                let valid = identifiers.iter().all(|id| {
                    !id.is_empty() && id.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'-')
                });
                if !valid {
                    return Err(format!("'{text}' has a pre-release that is not valid"));
                }
                identifiers
            }
        };
        Ok(Version {
            major: values[0],
            minor: values[1],
            patch: values[2],
            pre,
        })
    }

    /// Reads a release tag, which is a version with or without a leading `v`.
    ///
    /// For example, `v1.2.0` and `1.2.0` are both `1.2.0`, and `release-1` is `None`.
    pub fn from_tag(tag: &str) -> Option<Version> {
        let text = tag.strip_prefix('v').unwrap_or(tag);
        // a tag names a whole version
        if text.split(['-', '+']).next()?.split('.').count() != 3 {
            return None;
        }
        Version::parse(text).ok()
    }

    /// Returns the versions this one is compatible with as a name: the major version from 1.0 on,
    /// and the major and minor before that, as Cargo treats them.
    ///
    /// For example, `1.4.2` gives `1`, and `0.4.2` gives `0.4`.
    pub fn class(&self) -> String {
        if self.major == 0 {
            format!("0.{}", self.minor)
        } else {
            self.major.to_string()
        }
    }

    /// Returns whether this is a pre-release, such as `1.0.0-rc.1`.
    pub fn is_pre(&self) -> bool {
        !self.pre.is_empty()
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)?;
        if !self.pre.is_empty() {
            write!(f, "-{}", self.pre.join("."))?;
        }
        Ok(())
    }
}

impl Ord for Version {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.major, self.minor, self.patch)
            .cmp(&(other.major, other.minor, other.patch))
            .then_with(|| compare_pre(&self.pre, &other.pre))
    }
}

impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Orders pre-releases as semver does: a release after every pre-release of it, numeric
/// identifiers by value and before others, and a longer list after its prefix.
fn compare_pre(a: &[String], b: &[String]) -> Ordering {
    match (a.is_empty(), b.is_empty()) {
        (true, true) => return Ordering::Equal,
        (true, false) => return Ordering::Greater,
        (false, true) => return Ordering::Less,
        (false, false) => {}
    }
    for (x, y) in a.iter().zip(b) {
        let order = match (x.parse::<u64>(), y.parse::<u64>()) {
            (Ok(x), Ok(y)) => x.cmp(&y),
            (Ok(_), Err(_)) => Ordering::Less,
            (Err(_), Ok(_)) => Ordering::Greater,
            (Err(_), Err(_)) => x.cmp(y),
        };
        if order != Ordering::Equal {
            return order;
        }
    }
    a.len().cmp(&b.len())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(text: &str) -> Version {
        Version::parse(text).unwrap()
    }

    #[test]
    fn short_versions_fill_in_zeros() {
        assert_eq!(v("1"), Version::new(1, 0, 0));
        assert_eq!(v("1.2"), Version::new(1, 2, 0));
        assert_eq!(v("1.2.3+build").to_string(), "1.2.3");
        assert_eq!(v("1.2.3-beta.1").to_string(), "1.2.3-beta.1");
    }

    #[test]
    fn bad_versions_are_errors() {
        assert!(
            Version::parse("v1.2")
                .unwrap_err()
                .contains("without the 'v', as '1.2'")
        );
        for bad in ["", "1.", "1.2.3.4", "01.2", "a.b", "1.2-", "1.2-be ta"] {
            assert!(Version::parse(bad).is_err(), "{bad}");
        }
    }

    #[test]
    fn versions_order_as_semver_does() {
        let mut versions = [
            v("1.0.0"),
            v("1.0.0-rc.1"),
            v("1.0.0-beta.11"),
            v("1.0.0-beta.2"),
            v("1.0.0-beta"),
            v("1.0.0-alpha.1"),
            v("0.9.10"),
            v("0.9.9"),
        ];
        versions.sort();
        let sorted: Vec<String> = versions.iter().map(Version::to_string).collect();
        assert_eq!(
            sorted,
            [
                "0.9.9",
                "0.9.10",
                "1.0.0-alpha.1",
                "1.0.0-beta",
                "1.0.0-beta.2",
                "1.0.0-beta.11",
                "1.0.0-rc.1",
                "1.0.0"
            ]
        );
    }

    #[test]
    fn classes_split_majors_and_zero_minors() {
        assert_eq!(v("1.4.2").class(), "1");
        assert_eq!(v("2.0.0").class(), "2");
        assert_eq!(v("0.4.2").class(), "0.4");
        assert_eq!(v("0.0.3").class(), "0.0");
    }

    #[test]
    fn tags_are_whole_versions_with_or_without_v() {
        assert_eq!(Version::from_tag("v1.2.0"), Some(v("1.2.0")));
        assert_eq!(Version::from_tag("1.2.0"), Some(v("1.2.0")));
        assert_eq!(Version::from_tag("v1.2.0-rc.1"), Some(v("1.2.0-rc.1")));
        assert_eq!(Version::from_tag("v1.2"), None);
        assert_eq!(Version::from_tag("release-1"), None);
    }
}

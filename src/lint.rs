//! Lints: how each kind of warning ([`Lint`]) is reported.
//!
//! A program's `stone.toml` sets a [`Level`] for each lint in a `[lints]` table, by name:
//!
//! ```toml
//! [lints]
//! unused_name = "allow"
//! lost_change = "deny"
//! ```
//!
//! The checker reports some lints as it checks types, [`crate::unused`] reports unused names, and
//! [`find`] the rest, in one walk over a checked module.
//!
//! [`apply`] then drops the warnings an allowed lint gives and makes a denied lint's warnings
//! errors, which keep their lint's id, as in `error[W0004]`. It also drops every warning in
//! stone's own files and in the packages a program depends on, which are not its author's to fix.

mod checks;

use std::collections::HashMap;

use crate::diagnostic::{Diagnostic, Severity};
use crate::explain::{Lint, WARNINGS};
use crate::project::SourceMap;
use crate::span::{FileId, Span};

pub use checks::find;

/// How a lint's warnings are reported.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    /// Not at all.
    Allow,
    /// As warnings, which let the program run, as every lint is by default.
    Warn,
    /// As errors, which keep it from running or building.
    Deny,
}

impl Level {
    /// Returns the level a `[lints]` table writes as `text`, such as `"deny"`, if it is one.
    pub fn parse(text: &str) -> Option<Level> {
        match text {
            "allow" => Some(Level::Allow),
            "warn" => Some(Level::Warn),
            "deny" => Some(Level::Deny),
            _ => None,
        }
    }

    /// Returns the level as a `[lints]` table writes it, such as `deny`.
    pub fn as_str(self) -> &'static str {
        match self {
            Level::Allow => "allow",
            Level::Warn => "warn",
            Level::Deny => "deny",
        }
    }
}

/// The level of every lint, as a `[lints]` table sets them: each lint it names, then `warnings`
/// for the rest, then [`Level::Warn`].
///
/// For example, `warnings = "deny"` with `unused_name = "warn"` denies every lint but
/// `unused_name`.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Levels {
    /// The level `warnings` sets, with the span of its key.
    pub warnings: Option<(Level, Span)>,
    /// The level of each lint the table names, with the span of its key.
    pub lints: HashMap<Lint, (Level, Span)>,
}

impl Levels {
    /// Returns the level of `lint`.
    pub fn level(&self, lint: Lint) -> Level {
        self.setting(lint).map_or(Level::Warn, |(level, _)| level)
    }

    /// Returns the name that sets the level of `lint`, its own or `warnings`, with the level, if
    /// the table names either.
    fn setting(&self, lint: Lint) -> Option<(Level, &'static str)> {
        match self.lints.get(&lint) {
            Some((level, _)) => Some((*level, lint.name())),
            None => self.warnings.map(|(level, _)| (level, WARNINGS)),
        }
    }
}

/// Returns `diagnostics` as `levels` says to report them: without the warnings of an allowed
/// lint, a denied lint's warnings as errors, and without any warning in stone's own files or in
/// the packages the program depends on, which `sources` tells.
///
/// For example, with `unused_name = "deny"`, `warning[W0001]: variable 'x' is never used`
/// becomes `error[W0001]: variable 'x' is never used`, with a note naming the setting.
pub fn apply(
    diagnostics: Vec<Diagnostic>,
    levels: &Levels,
    sources: &SourceMap,
) -> Vec<Diagnostic> {
    let foreign = |span: Span| {
        let file = sources.file(span.file);
        span.file == FileId::PRELUDE || file.is_builtin() || file.is_dependency()
    };
    diagnostics
        .into_iter()
        .filter_map(|diagnostic| {
            if diagnostic.severity != Severity::Warning {
                return Some(diagnostic);
            }
            if foreign(diagnostic.span) {
                return None;
            }
            let Some(lint) = diagnostic.code().and_then(Lint::with_id) else {
                return Some(diagnostic);
            };
            match levels.setting(lint) {
                Some((Level::Allow, _)) => None,
                Some((Level::Deny, name)) => Some(denied(
                    diagnostic,
                    format!("`{name} = \"deny\"` in stone.toml makes this an error"),
                )),
                Some((Level::Warn, _)) | None => Some(diagnostic),
            }
        })
        .collect()
}

/// Makes every warning of `diagnostics` an error, as `stone check --deny warnings` does.
pub fn deny_warnings(diagnostics: Vec<Diagnostic>) -> Vec<Diagnostic> {
    diagnostics
        .into_iter()
        .map(|diagnostic| match diagnostic.severity {
            Severity::Warning => denied(
                diagnostic,
                "`--deny warnings` makes every warning an error".to_string(),
            ),
            Severity::Error => diagnostic,
        })
        .collect()
}

/// Returns the warning `diagnostic` as an error, with a note saying what made it one.
fn denied(diagnostic: Diagnostic, note: String) -> Diagnostic {
    Diagnostic {
        severity: Severity::Error,
        ..diagnostic
    }
    .with_note(note)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::project::{MapSources, link};
    use std::path::Path;

    /// Checks the program `main.stn` with a `stone.toml` holding `manifest`, returning each
    /// diagnostic rendered without its excerpt, as `main.stn:1:1: warning[W0001]: ...`.
    fn checked(manifest: &str, main: &str) -> Vec<String> {
        let mut sources = MapSources::default();
        sources.insert("stone.toml", manifest);
        let (map, diagnostics) =
            crate::driver::check_program(Path::new("main.stn"), main, &sources);
        diagnostics
            .iter()
            .map(|d| map.render(d).lines().next().unwrap_or_default().to_string())
            .collect()
    }

    #[test]
    fn an_allowed_lint_reports_nothing() {
        let source = "x = 1\n";
        assert_eq!(
            checked("", source),
            ["main.stn:1:1: warning[W0001]: variable 'x' is never used; remove it or name it '_x'"]
        );
        assert_eq!(
            checked("[lints]\nunused_name = \"allow\"\n", source),
            Vec::<String>::new()
        );
        assert_eq!(
            checked("[lints]\nwarnings = \"allow\"\n", source),
            Vec::<String>::new()
        );
        assert_eq!(
            checked(
                "[lints]\nwarnings = \"allow\"\nunused_name = \"warn\"\n",
                source
            )
            .len(),
            1
        );
    }

    #[test]
    fn a_denied_lint_is_an_error_that_keeps_its_id() {
        let mut sources = MapSources::default();
        sources.insert("stone.toml", "[lints]\nunused_name = \"deny\"\n");
        let (map, diagnostics) =
            crate::driver::check_program(Path::new("main.stn"), "x = 1\n", &sources);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Severity::Error);
        let rendered = map.render(&diagnostics[0]);
        assert!(
            rendered.starts_with("main.stn:1:1: error[W0001]: variable 'x' is never used")
                && rendered
                    .contains("= note: `unused_name = \"deny\"` in stone.toml makes this an error"),
            "{rendered}"
        );
    }

    #[test]
    fn a_denied_lint_keeps_a_program_from_running() {
        let mut sources = MapSources::default();
        sources.insert("stone.toml", "[lints]\nlost_change = \"deny\"\n");
        let source = "fn push(xs: list[int]):\n    xs.append(1)\n\npush([1])\n";
        let (_, checked) = crate::driver::load(Path::new("main.stn"), source, &sources);
        let errors = checked.expect_err("a denied lint is an error").0;
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code(), Some("W0004"));
        let (_, checked) =
            crate::driver::load(Path::new("main.stn"), source, &MapSources::default());
        let checked = checked.expect("a warning lets it run");
        assert_eq!(checked.warnings.len(), 1);
    }

    #[test]
    fn denying_warnings_makes_each_an_error() {
        let linked = link(Path::new("main.stn"), "x = 1\n", &MapSources::default());
        let warning = Lint::UnusedName.warning(Span::empty(crate::span::Pos::new(1, 1)), "unused");
        let denied = deny_warnings(apply(vec![warning], &Levels::default(), &linked.sources));
        assert_eq!(denied[0].severity, Severity::Error);
        assert_eq!(
            denied[0].notes(),
            ["`--deny warnings` makes every warning an error"]
        );
    }
}

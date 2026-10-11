//! Diagnostics: errors and warnings about a program, each pointing at a span of its source.
//!
//! Every stage reports problems as a [`Diagnostic`], so the CLI and the language server show them
//! the same way. For example, parsing `if x` reports `expected ';', found end of line` at the end
//! of the line.

use std::error::Error;
use std::fmt::Display;

use crate::span::Span;

/// How serious a [`Diagnostic`] is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

impl Severity {
    pub fn as_str(&self) -> &str {
        match self {
            Severity::Error => "error",
            Severity::Warning => "warning",
        }
    }
}

/// A problem found in a program, with a readable message and the span it applies to.
///
/// For example, `Diagnostic::error(span, "expected ')'")` reports a missing parenthesis.
#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub severity: Severity,
    pub span: Span,
    pub message: String,
}

impl Diagnostic {
    pub fn error(span: Span, message: impl Into<String>) -> Self {
        Diagnostic {
            severity: Severity::Error,
            span,
            message: message.into(),
        }
    }

    /// Makes a warning, which points out a likely mistake but still lets the program run.
    pub fn warning(span: Span, message: impl Into<String>) -> Self {
        Diagnostic {
            severity: Severity::Warning,
            span,
            message: message.into(),
        }
    }

    /// Formats the diagnostic for a terminal as `file:line:col: severity: message`, followed by the
    /// offending source line and a caret under the start of the span.
    ///
    /// For example, rendering `expected ';'` for `if x` in `main.stn` gives:
    ///
    /// ```text
    /// main.stn:1:5: error: expected ';', found end of line
    ///   |
    /// 1 | if x
    ///   |     ^
    /// ```
    pub fn render(&self, file: &str, source: &str) -> String {
        format!(
            "{}{}: {}\n{}",
            location(file, self.span),
            self.severity.as_str(),
            self.message,
            excerpt(source, self.span)
        )
    }
}

/// Returns where `span` starts, as the `file:line:col: ` that begins a rendered diagnostic.
///
/// For example, a span starting at line 3, column 7 of `main.stn` gives `main.stn:3:7: `.
pub fn location(file: &str, span: Span) -> String {
    format!("{file}:{}:{}: ", span.start.line, span.start.col)
}

/// Returns the lines that follow a rendered diagnostic's message: the source line `span` starts
/// on, with carets under the span, or nothing if `source` has no such line.
///
/// For example, the span of `1 / 0` in `print(1 / 0)` gives:
///
/// ```text
///   |
/// 1 | print(1 / 0)
///   |       ^^^^^
/// ```
pub fn excerpt(source: &str, span: Span) -> String {
    let Span { start, end, .. } = span;
    let Some(text) = source.lines().nth(start.line.saturating_sub(1)) else {
        return String::new();
    };
    let gutter = " ".repeat(start.line.to_string().len());
    let width = if end.line == start.line && end.col > start.col {
        end.col - start.col
    } else {
        1
    };
    let indent = " ".repeat(start.col.saturating_sub(1));
    format!(
        "{gutter} |\n{} | {text}\n{gutter} | {indent}{}\n",
        start.line,
        "^".repeat(width)
    )
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}: {}: {}",
            self.span.start.line,
            self.span.start.col,
            self.severity.as_str(),
            self.message
        )
    }
}

impl Error for Diagnostic {}

/// One or more diagnostics returned together as an error, such as every type error in a program.
///
/// For example, a failed parse returns `Diagnostics(vec![syntax_error])`.
#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostics(pub Vec<Diagnostic>);

impl Display for Diagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lines: Vec<String> = self.0.iter().map(Diagnostic::to_string).collect();
        write!(f, "{}", lines.join("\n"))
    }
}

impl Error for Diagnostics {}

impl From<Diagnostic> for Diagnostics {
    fn from(diagnostic: Diagnostic) -> Self {
        Diagnostics(vec![diagnostic])
    }
}

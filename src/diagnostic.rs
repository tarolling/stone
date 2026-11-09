//! Diagnostics: errors and warnings about a program, each pointing at a span of its source.
//!
//! Every stage reports problems as a [`Diagnostic`], so the CLI and the language server show them
//! the same way. For example, parsing `if x` reports `expected ':', found end of line` at the end
//! of the line.
//!
//! A diagnostic may say more than its message, as rustc's do: [`Label`]s point at other places
//! that explain it ("expected because of this parameter"), notes explain the rule involved, and
//! [`Help`] suggests a fix, possibly as code ([`Suggestion`]).

use std::error::Error;
use std::fmt::Display;

use crate::span::{FileId, Span};

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

/// A place other than a diagnostic's own span that explains it, with a short text written beside
/// the place.
///
/// For example, a type mismatch in an argument labels the parameter's type with
/// `expected because of this parameter`.
#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    pub span: Span,
    pub text: String,
}

/// A fix written as code: `replacement` takes the place of the text `span` covers, which may be
/// empty to insert it.
///
/// For example, replacing the span of `x` in `x + 1.5` with `float(x)` converts it.
#[derive(Debug, Clone, PartialEq)]
pub struct Suggestion {
    pub span: Span,
    pub replacement: String,
}

/// Advice on how to fix a problem, possibly with the fix itself as a [`Suggestion`].
#[derive(Debug, Clone, PartialEq)]
pub struct Help {
    pub text: String,
    pub suggestion: Option<Suggestion>,
}

/// A problem found in a program, with a readable message and the span it applies to.
///
/// For example, `Diagnostic::error(span, "expected ')'")` reports a missing parenthesis, and
/// `.with_label(other, "opened here")` points at the parenthesis it would close.
#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub severity: Severity,
    pub span: Span,
    pub message: String,
    /// What the diagnostic says beyond its message, if anything, boxed so that a plain one stays
    /// small, as every `Result` holding one is.
    pub details: Option<Box<Details>>,
}

/// What a [`Diagnostic`] may say beyond its message.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Details {
    /// The kind of error, such as `E0003`, which `stone explain` describes at length.
    pub code: Option<&'static str>,
    /// Other places that explain the problem, each shown with its text.
    pub labels: Vec<Label>,
    /// Explanations of the rule involved, shown after the source as `= note: ...`.
    pub notes: Vec<String>,
    /// Advice on fixing the problem, shown after the notes.
    pub help: Vec<Help>,
}

impl Diagnostic {
    pub fn error(span: Span, message: impl Into<String>) -> Self {
        Diagnostic::new(Severity::Error, span, message)
    }

    /// Makes a warning, which points out a likely mistake but still lets the program run.
    pub fn warning(span: Span, message: impl Into<String>) -> Self {
        Diagnostic::new(Severity::Warning, span, message)
    }

    fn new(severity: Severity, span: Span, message: impl Into<String>) -> Self {
        Diagnostic {
            severity,
            span,
            message: message.into(),
            details: None,
        }
    }

    /// Returns the details, made empty if there were none, to add to.
    fn details_mut(&mut self) -> &mut Details {
        self.details.get_or_insert_default()
    }

    /// Returns the code of the diagnostic's kind, such as `E0003`, if it has one.
    pub fn code(&self) -> Option<&'static str> {
        self.details.as_ref().and_then(|d| d.code)
    }

    /// Returns the other places that explain the problem.
    pub fn labels(&self) -> &[Label] {
        self.details.as_ref().map_or(&[], |d| &d.labels)
    }

    /// Returns the notes, which explain the rule involved.
    pub fn notes(&self) -> &[String] {
        self.details.as_ref().map_or(&[], |d| &d.notes)
    }

    /// Returns the advice on fixing the problem.
    pub fn help(&self) -> &[Help] {
        self.details.as_ref().map_or(&[], |d| &d.help)
    }

    /// Adds a label at `span`, another place that explains the problem.
    ///
    /// For example, `.with_label(param, "expected because of this parameter")`.
    pub fn with_label(mut self, span: Span, text: impl Into<String>) -> Self {
        self.details_mut().labels.push(Label {
            span,
            text: text.into(),
        });
        self
    }

    /// Adds a note, which explains the rule involved.
    pub fn with_note(mut self, text: impl Into<String>) -> Self {
        self.details_mut().notes.push(text.into());
        self
    }

    /// Adds advice on fixing the problem, written in words.
    pub fn with_help(mut self, text: impl Into<String>) -> Self {
        self.details_mut().help.push(Help {
            text: text.into(),
            suggestion: None,
        });
        self
    }

    /// Adds advice on fixing the problem as code: `replacement` in place of `span`.
    ///
    /// For example, `.with_suggestion("convert it", x, "float(x)")`.
    pub fn with_suggestion(
        mut self,
        text: impl Into<String>,
        span: Span,
        replacement: impl Into<String>,
    ) -> Self {
        self.details_mut().help.push(Help {
            text: text.into(),
            suggestion: Some(Suggestion {
                span,
                replacement: replacement.into(),
            }),
        });
        self
    }

    /// Gives the diagnostic the code of its kind, such as `E0003`.
    pub fn with_code(mut self, code: &'static str) -> Self {
        self.details_mut().code = Some(code);
        self
    }

    /// Returns the diagnostic with `f` applied to every text it shows: its message, its labels'
    /// and notes, and its help and suggested code, as an editor does to write a dependency's
    /// linked names as the program writes them.
    pub fn map_texts(&self, f: impl Fn(&str) -> String) -> Diagnostic {
        let details = self.details.as_ref().map(|details| {
            Box::new(Details {
                code: details.code,
                labels: details
                    .labels
                    .iter()
                    .map(|label| Label {
                        span: label.span,
                        text: f(&label.text),
                    })
                    .collect(),
                notes: details.notes.iter().map(|note| f(note)).collect(),
                help: details
                    .help
                    .iter()
                    .map(|help| Help {
                        text: f(&help.text),
                        suggestion: help.suggestion.as_ref().map(|s| Suggestion {
                            span: s.span,
                            replacement: f(&s.replacement),
                        }),
                    })
                    .collect(),
            })
        });
        Diagnostic {
            severity: self.severity,
            span: self.span,
            message: f(&self.message),
            details,
        }
    }

    /// Returns what a rendered diagnostic writes before its message: the severity, then the code
    /// in brackets if it has one, such as `error[E0003]`.
    pub fn heading(&self) -> String {
        match self.code() {
            Some(code) => format!("{}[{code}]", self.severity.as_str()),
            None => self.severity.as_str().to_string(),
        }
    }

    /// Formats the diagnostic for a terminal as `file:line:col: severity: message`, followed by the
    /// offending source line and a caret under the start of the span, for a diagnostic whose
    /// spans are all in one file.
    ///
    /// For example, rendering `expected ':'` for `if x` in `main.stn` gives:
    ///
    /// ```text
    /// main.stn:1:5: error: expected ':', found end of line
    ///   |
    /// 1 | if x
    ///   |     ^
    /// ```
    pub fn render(&self, file: &str, source: &str) -> String {
        self.render_in(|_| (file.to_string(), source))
    }

    /// Formats the diagnostic for a terminal as rustc does, with `files` giving the path and
    /// source of each file a span is in.
    ///
    /// After the `file:line:col: severity: message` line come the lines of the source its span
    /// and labels are on, each in line order with markers under it (`^` for the diagnostic's
    /// own span, `-` with its text for a label), then a `::: file:line:col` block for each other
    /// file a label is in, then `= note:` and `= help:` lines, then each suggestion as the line
    /// it would change, rewritten, with `~` (or `+` for an insertion) under the new text. For
    /// example, an argument of the wrong type gives:
    ///
    /// ```text
    /// main.stn:3:3: error: expected int, found str
    ///   |
    /// 1 | fn f(a: int):
    ///   |         --- expected because of this parameter
    /// 2 |     print(a)
    /// 3 | f("x")
    ///   |   ^^^
    /// ```
    pub fn render_in<'a>(&self, files: impl Fn(FileId) -> (String, &'a str)) -> String {
        let (path, _) = files(self.span.file);
        let mut out = format!(
            "{}{}: {}\n",
            location(&path, self.span),
            self.heading(),
            self.message
        );

        // every place marked, the diagnostic's own first, grouped by file in order of appearance
        let marks: Vec<Mark> = std::iter::once(Mark {
            span: self.span,
            text: "",
            primary: true,
        })
        .chain(self.labels().iter().map(|label| Mark {
            span: label.span,
            text: &label.text,
            primary: false,
        }))
        .collect();
        let mut order: Vec<FileId> = Vec::new();
        for mark in &marks {
            if !order.contains(&mark.span.file) {
                order.push(mark.span.file);
            }
        }
        let suggestions: Vec<(&Help, &Suggestion)> = self
            .help()
            .iter()
            .filter_map(|help| Some((help, help.suggestion.as_ref()?)))
            .collect();

        // one gutter width for every line number written
        let highest = marks
            .iter()
            .map(|mark| mark.span.start.line)
            .chain(
                suggestions
                    .iter()
                    .map(|(_, s)| s.span.start.line + s.replacement.matches('\n').count()),
            )
            .max()
            .unwrap_or(1);
        let gutter = " ".repeat(highest.to_string().len());
        let width = gutter.len();

        let mut wrote_source = false;
        for (index, file) in order.iter().enumerate() {
            let (path, source) = files(*file);
            let lines: Vec<&str> = source.lines().collect();
            let mut numbers: Vec<usize> = marks
                .iter()
                .filter(|mark| mark.span.file == *file)
                .map(|mark| mark.span.start.line)
                .filter(|line| *line >= 1 && *line <= lines.len())
                .collect();
            numbers.sort_unstable();
            numbers.dedup();
            if numbers.is_empty() {
                continue;
            }
            if index > 0 {
                let first = marks
                    .iter()
                    .find(|mark| mark.span.file == *file)
                    .map_or(self.span, |mark| mark.span);
                if wrote_source {
                    out.push_str(&format!("{gutter} |\n"));
                }
                let at = location(&path, first);
                out.push_str(&format!("{gutter}::: {}\n", at.trim_end_matches(": ")));
            }
            out.push_str(&format!("{gutter} |\n"));
            let mut previous: Option<usize> = None;
            for number in numbers {
                match previous {
                    Some(before) if number == before + 2 => {
                        let skipped = before + 1;
                        out.push_str(&format!("{skipped:<width$} | {}\n", lines[skipped - 1]));
                    }
                    Some(before) if number > before + 2 => out.push_str("...\n"),
                    _ => {}
                }
                out.push_str(&format!("{number:<width$} | {}\n", lines[number - 1]));
                let on_line: Vec<&Mark> = marks
                    .iter()
                    .filter(|mark| mark.span.file == *file && mark.span.start.line == number)
                    .collect();
                for marker in marker_lines(&on_line) {
                    out.push_str(&format!("{gutter} | {marker}\n"));
                }
                previous = Some(number);
            }
            wrote_source = true;
        }

        let words: Vec<(&str, &str)> = self
            .notes()
            .iter()
            .map(|note| ("note", note.as_str()))
            .chain(
                self.help()
                    .iter()
                    .filter(|help| help.suggestion.is_none())
                    .map(|help| ("help", help.text.as_str())),
            )
            .collect();
        if !words.is_empty() && wrote_source {
            out.push_str(&format!("{gutter} |\n"));
        }
        for (kind, text) in words {
            let indent = " ".repeat(width + kind.len() + 5);
            let mut text_lines = text.lines();
            out.push_str(&format!(
                "{gutter} = {kind}: {}\n",
                text_lines.next().unwrap_or("")
            ));
            for line in text_lines {
                out.push_str(&format!("{indent}{line}\n"));
            }
        }

        for (help, suggestion) in suggestions {
            let (path, source) = files(suggestion.span.file);
            let Some(changed) = apply(source, suggestion) else {
                continue;
            };
            if wrote_source {
                out.push_str(&format!("{gutter} |\n"));
            }
            out.push_str(&format!("help: {}\n", help.text));
            if suggestion.span.file != self.span.file {
                let at = location(&path, suggestion.span);
                out.push_str(&format!("{gutter}::: {}\n", at.trim_end_matches(": ")));
            }
            out.push_str(&format!("{gutter} |\n"));
            let start = suggestion.span.start;
            for (offset, line) in changed.iter().enumerate() {
                let number = start.line + offset;
                out.push_str(&format!("{number:<width$} | {line}\n"));
            }
            let shown = suggestion
                .replacement
                .split('\n')
                .next()
                .unwrap_or("")
                .chars()
                .count();
            if shown > 0 {
                let marker = if suggestion.span.is_empty() { '+' } else { '~' };
                out.push_str(&format!(
                    "{gutter} | {}{}\n",
                    " ".repeat(start.col.saturating_sub(1)),
                    marker.to_string().repeat(shown)
                ));
            }
            wrote_source = true;
        }
        out
    }
}

/// A place a rendered diagnostic marks under its line: the diagnostic's own span, or a label.
struct Mark<'a> {
    span: Span,
    text: &'a str,
    primary: bool,
}

impl Mark<'_> {
    /// Returns the column, counted from 0, the mark starts at.
    fn column(&self) -> usize {
        self.span.start.col.saturating_sub(1)
    }

    /// Returns how many characters the mark covers: its span on one line, or one character for
    /// an empty span or one that ends on a later line.
    fn width(&self) -> usize {
        let Span { start, end, .. } = self.span;
        if end.line == start.line && end.col > start.col {
            end.col - start.col
        } else {
            1
        }
    }
}

/// Returns the lines written under a source line for the marks on it, as rustc writes them:
/// `^` under the diagnostic's own span and `-` under each label, the rightmost label's text
/// beside its markers, and each other label's text on a line of its own below, hung from its
/// column by `|`.
///
/// For example, a label `int` on `a` and `str` on `b`, with the diagnostic at `+`, in
/// `y = a + b`, gives `    - ^ - str`, `    |`, and `    int`.
fn marker_lines(marks: &[&Mark]) -> Vec<String> {
    let mut markers: Vec<char> = Vec::new();
    for primary in [false, true] {
        for mark in marks.iter().filter(|mark| mark.primary == primary) {
            let end = mark.column() + mark.width();
            if markers.len() < end {
                markers.resize(end, ' ');
            }
            let symbol = if primary { '^' } else { '-' };
            for slot in &mut markers[mark.column()..end] {
                *slot = symbol;
            }
        }
    }
    let mut first: String = markers.iter().collect();
    let mut texted: Vec<&Mark> = marks
        .iter()
        .copied()
        .filter(|mark| !mark.text.is_empty())
        .collect();
    texted.sort_by_key(|mark| mark.column());
    let rightmost = marks.iter().map(|mark| mark.column()).max().unwrap_or(0);
    if let Some(last) = texted.last()
        && last.column() == rightmost
    {
        first.push(' ');
        first.push_str(last.text);
        texted.pop();
    }
    let mut lines = vec![first.trim_end().to_string()];
    while let Some(mark) = texted.pop() {
        let hang = |upto: &[&Mark]| -> Vec<char> {
            let mut line: Vec<char> = Vec::new();
            for other in upto {
                if line.len() <= other.column() {
                    line.resize(other.column() + 1, ' ');
                }
                line[other.column()] = '|';
            }
            line
        };
        let mut with = texted.clone();
        with.push(mark);
        lines.push(
            hang(&with)
                .iter()
                .collect::<String>()
                .trim_end()
                .to_string(),
        );
        let mut text = hang(&texted);
        if text.len() < mark.column() {
            text.resize(mark.column(), ' ');
        }
        text.truncate(mark.column());
        let mut text: String = text.iter().collect();
        text.push_str(mark.text);
        lines.push(text);
    }
    lines
}

/// Returns the source lines `suggestion` changes, with its replacement written in, or nothing if
/// they are not in `source`.
///
/// For example, replacing `x` in `y = x + 1.5` with `float(x)` gives `["y = float(x) + 1.5"]`.
fn apply(source: &str, suggestion: &Suggestion) -> Option<Vec<String>> {
    let Span { start, end, .. } = suggestion.span;
    let lines: Vec<&str> = source.lines().collect();
    let first: Vec<char> = lines.get(start.line.checked_sub(1)?)?.chars().collect();
    let last: Vec<char> = lines.get(end.line.checked_sub(1)?)?.chars().collect();
    let before: String = first[..start.col.saturating_sub(1).min(first.len())]
        .iter()
        .collect();
    let after: String = last[end.col.saturating_sub(1).min(last.len())..]
        .iter()
        .collect();
    let changed = format!("{before}{}{after}", suggestion.replacement);
    Some(changed.split('\n').map(str::to_string).collect())
}

/// Returns how far apart two names are: how many characters must be added, removed, or
/// replaced, or neighbors swapped, to turn one into the other (the optimal string alignment
/// distance), which counts the common slips of typing.
///
/// For example, `distance("pritn", "print")` and `distance("pirnt", "print")` are 1.
pub fn distance(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let mut d = vec![vec![0usize; b.len() + 1]; a.len() + 1];
    for (i, row) in d.iter_mut().enumerate() {
        row[0] = i;
    }
    for (j, cell) in d[0].iter_mut().enumerate() {
        *cell = j;
    }
    for i in 1..=a.len() {
        for j in 1..=b.len() {
            let cost = usize::from(a[i - 1] != b[j - 1]);
            let mut best = (d[i - 1][j] + 1)
                .min(d[i][j - 1] + 1)
                .min(d[i - 1][j - 1] + cost);
            if i > 1 && j > 1 && a[i - 1] == b[j - 2] && a[i - 2] == b[j - 1] {
                best = best.min(d[i - 2][j - 2] + 1);
            }
            d[i][j] = best;
        }
    }
    d[a.len()][b.len()]
}

/// Returns the name among `candidates` closest to `name`, a misspelling of it, for a "did you
/// mean" help: one within a third of its length (at least 1) of it and not `name` itself, the
/// first in order of the candidates' text among equally close ones.
///
/// For example, `similar("lenght", ["length", "width"])` is `Some("length")`, and
/// `similar("x", ["width"])` is `None`.
pub fn similar<'a>(name: &str, candidates: impl IntoIterator<Item = &'a str>) -> Option<&'a str> {
    let most = (name.chars().count() / 3).max(1);
    candidates
        .into_iter()
        .filter(|candidate| *candidate != name)
        .map(|candidate| (distance(name, candidate), candidate))
        .filter(|(far, _)| *far <= most)
        .min()
        .map(|(_, candidate)| candidate)
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
            self.heading(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Pos;

    /// Returns the span of line `line`, columns `start` up to `end`, in the entry file.
    fn at(line: usize, start: usize, end: usize) -> Span {
        Span::new(Pos::new(line, start), Pos::new(line, end))
    }

    #[test]
    fn a_plain_diagnostic_renders_its_line_and_carets() {
        let source = "x = 1\nprint(1 / 0)\n";
        let d = Diagnostic::error(at(2, 7, 12), "division by zero");
        let expected =
            "main.stn:2:7: error: division by zero\n  |\n2 | print(1 / 0)\n  |       ^^^^^\n";
        assert_eq!(d.render("main.stn", source), expected);
        assert_eq!(
            d.render("main.stn", source),
            format!(
                "{}error: division by zero\n{}",
                location("main.stn", d.span),
                excerpt(source, d.span)
            )
        );
    }

    #[test]
    fn a_diagnostic_off_the_end_of_its_source_renders_only_its_message() {
        let d = Diagnostic::error(at(9, 1, 2), "unexpected end of file");
        assert_eq!(
            d.render("main.stn", "x = 1\n"),
            "main.stn:9:1: error: unexpected end of file\n"
        );
    }

    #[test]
    fn a_label_on_an_earlier_line_comes_first_with_the_line_between() {
        let source = "fn f(a: int):\n    print(a)\nf(\"x\")\n";
        let d = Diagnostic::error(at(3, 3, 6), "expected int, found str")
            .with_label(at(1, 9, 12), "expected because of this parameter");
        assert_eq!(
            d.render("main.stn", source),
            "main.stn:3:3: error: expected int, found str
  |
1 | fn f(a: int):
  |         --- expected because of this parameter
2 |     print(a)
3 | f(\"x\")
  |   ^^^
"
        );
    }

    #[test]
    fn lines_far_apart_are_separated_by_dots() {
        let source = "x = 1\na\nb\nc\nx = \"s\"\n";
        let d = Diagnostic::error(at(5, 5, 8), "expected int, found str")
            .with_label(at(1, 1, 2), "first assigned here");
        assert_eq!(
            d.render("main.stn", source),
            "main.stn:5:5: error: expected int, found str
  |
1 | x = 1
  | - first assigned here
...
5 | x = \"s\"
  |     ^^^
"
        );
    }

    #[test]
    fn labels_on_the_diagnostics_line_hang_their_texts() {
        let d = Diagnostic::error(at(1, 7, 8), "cannot add int and str")
            .with_label(at(1, 5, 6), "int")
            .with_label(at(1, 9, 10), "str");
        assert_eq!(
            d.render("main.stn", "y = a + b\n"),
            "main.stn:1:7: error: cannot add int and str
  |
1 | y = a + b
  |     - ^ - str
  |     |
  |     int
"
        );
    }

    #[test]
    fn a_label_left_of_the_diagnostic_hangs_its_text() {
        let d = Diagnostic::error(at(1, 9, 10), "unexpected value").with_label(at(1, 5, 6), "here");
        assert_eq!(
            d.render("main.stn", "y = a + b\n"),
            "main.stn:1:9: error: unexpected value
  |
1 | y = a + b
  |     -   ^
  |     |
  |     here
"
        );
    }

    #[test]
    fn line_numbers_share_one_gutter() {
        let source = "x = 1\n".repeat(11);
        let d = Diagnostic::error(at(11, 1, 2), "bad").with_label(at(1, 1, 2), "first");
        assert_eq!(
            d.render("main.stn", &source),
            "main.stn:11:1: error: bad
   |
1  | x = 1
   | - first
...
11 | x = 1
   | ^
"
        );
    }

    #[test]
    fn notes_and_help_follow_the_source() {
        let d = Diagnostic::error(at(1, 1, 2), "cannot change a global")
            .with_note("a function changes a value only by returning it")
            .with_help("return it instead:\nxs = f(xs)");
        assert_eq!(
            d.render("main.stn", "x\n"),
            "main.stn:1:1: error: cannot change a global
  |
1 | x
  | ^
  |
  = note: a function changes a value only by returning it
  = help: return it instead:
          xs = f(xs)
"
        );
    }

    #[test]
    fn a_suggestion_shows_the_line_rewritten() {
        let d = Diagnostic::error(at(1, 5, 6), "expected float, found int").with_suggestion(
            "convert it",
            at(1, 5, 6),
            "float(x)",
        );
        assert_eq!(
            d.render("main.stn", "y = x + 1.5\n"),
            "main.stn:1:5: error: expected float, found int
  |
1 | y = x + 1.5
  |     ^
  |
help: convert it
  |
1 | y = float(x) + 1.5
  |     ~~~~~~~~
"
        );
    }

    #[test]
    fn an_inserting_suggestion_marks_its_text_with_plus() {
        let span = Span::empty(Pos::new(1, 13));
        let d = Diagnostic::error(at(1, 4, 5), "missing return type")
            .with_suggestion("write it", span, " -> int");
        assert_eq!(
            d.render("main.stn", "fn f(a: int):\n"),
            "main.stn:1:4: error: missing return type
  |
1 | fn f(a: int):
  |    ^
  |
help: write it
  |
1 | fn f(a: int) -> int:
  |             +++++++
"
        );
    }

    #[test]
    fn a_label_in_another_file_gets_a_block_of_its_own() {
        let main = "geo.f(\"x\")\n";
        let geo = "pub fn f(a: int):\n";
        let other = FileId(1);
        let d = Diagnostic::error(at(1, 7, 10), "expected int, found str")
            .with_label(at(1, 13, 16).in_file(other), "expected because of this");
        let rendered = d.render_in(|file| {
            if file == other {
                ("geo.stn".to_string(), geo)
            } else {
                ("main.stn".to_string(), main)
            }
        });
        assert_eq!(
            rendered,
            "main.stn:1:7: error: expected int, found str
  |
1 | geo.f(\"x\")
  |       ^^^
  |
 ::: geo.stn:1:13
  |
1 | pub fn f(a: int):
  |             --- expected because of this
"
        );
    }

    #[test]
    fn similar_finds_the_closest_misspelling() {
        assert_eq!(distance("pritn", "print"), 1);
        assert_eq!(distance("kitten", "sitting"), 3);
        assert_eq!(similar("lenght", ["width", "length"]), Some("length"));
        assert_eq!(similar("totl", ["total", "tot"]), Some("tot"));
        assert_eq!(similar("x", ["width"]), None);
        assert_eq!(similar("x", ["x"]), None);
    }

    #[test]
    fn a_code_follows_the_severity() {
        let d = Diagnostic::error(at(1, 1, 2), "bad").with_code("E0001");
        assert!(
            d.render("main.stn", "x\n")
                .starts_with("main.stn:1:1: error[E0001]: bad\n")
        );
        assert_eq!(d.to_string(), "1:1: error[E0001]: bad");
    }
}

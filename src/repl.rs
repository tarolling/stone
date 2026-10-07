//! The interactive session `stone` runs when given no file.
//!
//! Each entry is checked together with every earlier entry, as if they were one file, and then
//! only the new entry runs, so variables and functions carry over. For example, entering `x = 1`
//! and then `x + 1` prints `2`.

use crate::ast::{Mod, Stmt, StmtKind};
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::driver;
use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::project::{SourceMap, Sources};
use crate::span::{FileId, Pos, Span};
use crate::token::TokenType;
use std::io::Write;
use std::path::Path;

/// The name errors in the current entry show as their file.
const ENTRY: &str = "<stdin>";

/// The name errors in earlier entries show as their file, with lines counted from the first
/// entry the session kept.
const HISTORY: &str = "<session>";

/// Returns whether `buffer` is the start of an entry that needs more lines, which is when it
/// opens a block and has not yet ended with a blank line, as in Python.
///
/// For example, `needs_more("if x;\n")` is true, while `needs_more("x = 1\n")` and
/// `needs_more("if x;\n    y = 1\n\n")` are false.
pub fn needs_more(buffer: &str) -> bool {
    if buffer
        .lines()
        .last()
        .is_none_or(|line| line.trim().is_empty())
    {
        return false;
    }
    // a lex error is reported once the entry is submitted
    let Ok(tokens) = Lexer::new(buffer).lex() else {
        return false;
    };
    tokens
        .windows(2)
        .any(|pair| pair[0].r#type == TokenType::Semi && pair[1].r#type == TokenType::Newline)
}

/// One top-level statement of an entry the session kept, as source text ending in a newline.
struct Chunk {
    text: String,
    /// The function it defines, if it is a `def`, so a later `def` of the same name replaces it.
    defines: Option<String>,
}

/// Every entry the session has kept, which each new entry is checked together with.
#[derive(Default)]
struct Session {
    chunks: Vec<Chunk>,
}

/// An entry that passed the checker together with the session, ready to run.
struct Prepared {
    /// The linked program of the session and the entry.
    module: Mod,
    /// How many lines of the program come before the entry.
    offset: usize,
    /// The entry's statements, to add to the session.
    chunks: Vec<Chunk>,
}

impl Prepared {
    /// Returns the statements to run: the entry's own, plus the functions of every module the
    /// session uses, which are only defined again.
    fn statements(&self) -> Vec<Stmt> {
        let Mod::Module { body } = &self.module;
        body.iter()
            .filter(|stmt| stmt.span.file != FileId(0) || stmt.span.start.line > self.offset)
            .cloned()
            .collect()
    }
}

impl Session {
    /// Checks `entry` together with every kept entry, leaving out functions it defines again,
    /// and returns it ready to run, or every error rendered for a terminal.
    ///
    /// For example, after `x = 1`, preparing `x + "a"` fails with an error at `<stdin>:1:1`.
    fn prepare(&self, entry: &str, sources: &dyn Sources) -> Result<Prepared, String> {
        let chunks = split(entry);
        let history: String = self
            .chunks
            .iter()
            .filter(|old| {
                old.defines
                    .as_ref()
                    .is_none_or(|name| !chunks.iter().any(|new| new.defines.as_ref() == Some(name)))
            })
            .map(|old| old.text.as_str())
            .collect();
        let offset = history.lines().count();
        let program = format!("{history}{entry}");
        match driver::load(Path::new(ENTRY), &program, sources) {
            (_, Ok(module)) => Ok(Prepared {
                module,
                offset,
                chunks,
            }),
            (files, Err(Diagnostics(diagnostics))) => Err(diagnostics
                .iter()
                .map(|d| render(d, &files, &history, entry, offset))
                .collect()),
        }
    }

    /// Keeps a prepared entry, replacing any function it defines again.
    fn commit(&mut self, prepared: Prepared) {
        self.chunks.retain(|old| {
            old.defines.as_ref().is_none_or(|name| {
                !prepared
                    .chunks
                    .iter()
                    .any(|new| new.defines.as_ref() == Some(name))
            })
        });
        self.chunks.extend(prepared.chunks);
    }
}

/// Splits an entry into one chunk per top-level statement, each running from the line the
/// statement starts on to the line before the next one.
///
/// For example, `x = 1\ndef f(); ret x\n` gives `x = 1\n` and `def f(); ret x\n`, the second
/// defining `f`. An entry that does not parse gives no chunks, since it is never kept.
fn split(entry: &str) -> Vec<Chunk> {
    let Ok(tokens) = Lexer::new(entry).lex() else {
        return vec![];
    };
    let (Mod::Module { body }, _) = Parser::new(&tokens).parse_recovering();
    let lines: Vec<&str> = entry.lines().collect();
    let starts: Vec<usize> = body.iter().map(|stmt| stmt.span.start.line - 1).collect();
    body.iter()
        .enumerate()
        .map(|(i, stmt)| {
            let end = starts.get(i + 1).copied().unwrap_or(lines.len());
            let text = lines[starts[i]..end]
                .iter()
                .map(|line| format!("{line}\n"))
                .collect();
            let defines = match &stmt.kind {
                StmtKind::FunctionDef { name, .. } => Some(name.clone()),
                _ => None,
            };
            Chunk { text, defines }
        })
        .collect()
}

/// Renders a diagnostic for a terminal. One in the entry names `<stdin>` with lines counted from
/// the entry's first, one in an earlier entry names `<session>`, and one in a module names the
/// module's file.
fn render(
    diagnostic: &Diagnostic,
    files: &SourceMap,
    history: &str,
    entry: &str,
    offset: usize,
) -> String {
    let span = diagnostic.span;
    if span.file != FileId(0) {
        return files.render(diagnostic);
    }
    if span.start.line <= offset {
        return diagnostic.render(HISTORY, history);
    }
    let shift = |pos: Pos| Pos::new(pos.line.saturating_sub(offset), pos.col);
    let shifted = Diagnostic {
        span: Span {
            start: shift(span.start),
            end: shift(span.end),
            ..span
        },
        ..diagnostic.clone()
    };
    shifted.render(ENTRY, entry)
}

/// Runs an interactive session over the interpreter's input until it ends, printing each
/// expression statement's value unless it is `none`, and every error to `err`. With `prompts`,
/// writes `>>> ` before each entry and `... ` before each further line of one to `err`.
///
/// Modules named by `use` are read from `sources`, relative to the current directory.
pub fn run(
    interpreter: &mut Interpreter,
    err: &mut dyn Write,
    prompts: bool,
    sources: &dyn Sources,
) -> std::io::Result<()> {
    let mut session = Session::default();
    let mut buffer = String::new();
    loop {
        if prompts {
            interpreter.output().flush()?;
            write!(err, "{}", if buffer.is_empty() { ">>> " } else { "... " })?;
            err.flush()?;
        }
        let mut bytes = Vec::new();
        if interpreter.input().read_until(b'\n', &mut bytes)? == 0 {
            if prompts {
                writeln!(err)?;
            }
            if !buffer.is_empty() {
                submit(&mut session, &buffer, interpreter, err, sources)?;
            }
            return interpreter.output().flush();
        }
        let line = String::from_utf8_lossy(&bytes);
        if buffer.is_empty() && line.trim().is_empty() {
            continue;
        }
        buffer += line.trim_end_matches('\n');
        buffer.push('\n');
        if !needs_more(&buffer) {
            submit(&mut session, &buffer, interpreter, err, sources)?;
            buffer.clear();
        }
    }
}

/// Checks and runs one entry, writing any error to `err`.
fn submit(
    session: &mut Session,
    entry: &str,
    interpreter: &mut Interpreter,
    err: &mut dyn Write,
    sources: &dyn Sources,
) -> std::io::Result<()> {
    let prepared = match session.prepare(entry, sources) {
        Ok(prepared) => prepared,
        Err(rendered) => return err.write_all(rendered.as_bytes()),
    };
    let statements = prepared.statements();
    // kept even if it fails, since the statements before the failure have already run
    session.commit(prepared);
    let result = interpreter.run_entry(&statements);
    interpreter.output().flush()?;
    match result {
        Ok(()) => Ok(()),
        Err(e) => writeln!(err, "error: {e}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver;
    use crate::project::MapSources;

    #[test]
    fn a_simple_statement_needs_nothing_more() {
        assert!(!needs_more("x = 1\n"));
    }

    #[test]
    fn a_block_needs_more_until_a_blank_line() {
        assert!(needs_more("if x;\n"));
        assert!(needs_more("def f();\n"));
        assert!(needs_more("if x;\n    y = 1\n"));
        assert!(!needs_more("if x;\n    y = 1\n\n"));
        assert!(!needs_more("if x;\n    y = 1\n   \n"));
    }

    #[test]
    fn a_one_line_block_needs_nothing_more() {
        assert!(!needs_more("def f(); ret 1\n"));
    }

    #[test]
    fn a_semicolon_in_a_string_or_comment_opens_no_block() {
        assert!(!needs_more("print(\"a;\")\n"));
        assert!(!needs_more("x = 1 # see;\n"));
    }

    #[test]
    fn a_lex_error_needs_nothing_more() {
        assert!(!needs_more("print(\"a;\n"));
    }

    /// Runs a session over `input` with `util.st` holding `pub def one(); ret 1`, returning what
    /// it printed to stdout and to stderr.
    ///
    /// For example, `session("x = 1\nx + 1\n")` returns `("2\n", "")`.
    fn session(input: &str) -> (String, String) {
        let mut sources = MapSources::default();
        sources.insert("util.st", "pub def one(); ret 1\n");
        let (mut out, mut err) = (Vec::new(), Vec::new());
        driver::repl(&mut input.as_bytes(), &mut out, &mut err, false, &sources).unwrap();
        (
            String::from_utf8(out).unwrap(),
            String::from_utf8(err).unwrap(),
        )
    }

    #[test]
    fn variables_carry_over_between_entries() {
        assert_eq!(session("x = 1\nx + 1\n"), ("2\n".into(), "".into()));
    }

    #[test]
    fn expression_values_echo_like_list_elements() {
        assert_eq!(session("\"hi\"\n").0, "'hi'\n");
        assert_eq!(session("[1, 2]\n").0, "[1, 2]\n");
        assert_eq!(session("1.5\n").0, "1.5\n");
    }

    #[test]
    fn none_values_do_not_echo() {
        assert_eq!(session("print(1)\n"), ("1\n".into(), "".into()));
    }

    #[test]
    fn blank_lines_between_entries_are_skipped() {
        assert_eq!(session("\n  \nx = 3\n\nx\n"), ("3\n".into(), "".into()));
    }

    #[test]
    fn a_multi_line_function_ends_at_a_blank_line() {
        let input = "def double(n);\n    m = n * 2\n    ret m\n\ndouble(4)\n";
        assert_eq!(session(input), ("8\n".into(), "".into()));
    }

    #[test]
    fn a_loop_runs_once_its_block_ends() {
        let input = "for i in range(3);\n    print(i)\n\nprint(\"done\")\n";
        assert_eq!(session(input).0, "0\n1\n2\ndone\n");
    }

    #[test]
    fn a_block_at_the_end_of_the_input_still_runs() {
        assert_eq!(session("if 1;\n    print(5)\n").0, "5\n");
    }

    #[test]
    fn functions_can_be_called_from_a_later_entry() {
        assert_eq!(session("def one(); ret 1\none() + 1\n").0, "2\n");
    }

    #[test]
    fn errors_point_into_the_entry_and_the_session_continues() {
        let (out, err) = session("x = 1\ny = 2\nx + \"a\"\nx + y\n");
        assert_eq!(out, "3\n");
        assert!(err.starts_with("<stdin>:1:"), "{err}");
        assert!(err.contains("1 | x + \"a\""), "{err}");
    }

    #[test]
    fn syntax_errors_are_reported() {
        let (out, err) = session("x = \nprint(2)\n");
        assert_eq!(out, "2\n");
        assert!(err.starts_with("<stdin>:1:"), "{err}");
    }

    #[test]
    fn a_rejected_entry_is_forgotten() {
        let (out, err) = session("x = 1\nx = \"a\"\nx\n");
        assert_eq!(out, "1\n");
        assert!(err.contains("error"), "{err}");
    }

    #[test]
    fn names_from_a_rejected_entry_stay_undefined() {
        let (_, err) = session("y = 1 + \"a\"\ny\n");
        assert_eq!(err.matches("error").count(), 2, "{err}");
    }

    #[test]
    fn a_function_can_be_redefined() {
        let (out, err) = session("def f(); ret 1\nf()\ndef f(); ret 2\nf()\n");
        assert_eq!((out.as_str(), err.as_str()), ("1\n2\n", ""));
    }

    #[test]
    fn a_redefinition_that_breaks_earlier_entries_is_rejected() {
        let (out, err) = session("def f(); ret 1\nx = f() + 1\ndef f(); ret \"a\"\nf()\n");
        assert_eq!(out, "1\n");
        assert!(
            err.starts_with("<stdin>:1:14: error: 'f' returns int"),
            "{err}"
        );
    }

    #[test]
    fn errors_in_earlier_entries_name_the_session() {
        let span = Span::new(Pos::new(2, 5), Pos::new(2, 6));
        let rendered = render(
            &Diagnostic::error(span, "bad"),
            &SourceMap::default(),
            "x = 1\ny = x\n",
            "z = 1\n",
            2,
        );
        assert!(
            rendered.starts_with("<session>:2:5: error: bad\n"),
            "{rendered}"
        );
        assert!(rendered.contains("2 | y = x"), "{rendered}");
    }

    #[test]
    fn entries_split_into_statements() {
        let chunks = split("x = 1\ndef f();\n    ret x\n\nprint(1)\n");
        let texts: Vec<&str> = chunks.iter().map(|c| c.text.as_str()).collect();
        assert_eq!(texts, ["x = 1\n", "def f();\n    ret x\n\n", "print(1)\n"]);
        assert_eq!(chunks[1].defines.as_deref(), Some("f"));
        assert_eq!(chunks[0].defines, None);
    }

    #[test]
    fn runtime_errors_are_reported_and_the_session_continues() {
        let (out, err) = session("x = 4\n1 / 0\nx\n");
        assert_eq!(out, "4\n");
        assert_eq!(err, "error: division by zero\n");
    }

    #[test]
    fn input_reads_the_line_after_the_entry() {
        let (out, _) = session("name = input()\nAda\nprint(\"hi\", name)\n");
        assert_eq!(out, "hi Ada\n");
    }

    #[test]
    fn modules_can_be_used() {
        assert_eq!(session("use util\nutil.one() + 1\n").0, "2\n");
        assert_eq!(session("use util.one\none()\n").0, "1\n");
    }

    #[test]
    fn prompts_go_to_err() {
        let (mut out, mut err) = (Vec::new(), Vec::new());
        let input = "if 1;\n    print(1)\n\n";
        driver::repl(
            &mut input.as_bytes(),
            &mut out,
            &mut err,
            true,
            &MapSources::default(),
        )
        .unwrap();
        assert_eq!(out, b"1\n");
        assert_eq!(String::from_utf8(err).unwrap(), ">>> ... ... >>> \n");
    }
}

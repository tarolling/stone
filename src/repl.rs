//! The interactive session `stone` runs when given no file.
//!
//! Each entry is checked together with every earlier entry, as if they were one file, and then
//! only the new entry runs, so variables and functions carry over. For example, entering `x = 1`
//! and then `x + 1` prints `2`.

use crate::ast::{Mod, Stmt, StmtKind};
use crate::checker::Calls;
use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::driver;
use crate::interpreter::{Exit, Interpreter};
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
/// For example, `needs_more("if x:\n")` is true, while `needs_more("x = 1\n")` and
/// `needs_more("if x:\n    y = 1\n\n")` are false.
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
        .any(|pair| pair[0].r#type == TokenType::Colon && pair[1].r#type == TokenType::Newline)
}

/// One top-level statement of an entry the session kept, as source text ending in a newline.
struct Chunk {
    text: String,
    /// What it defines: a function, enum, or struct by its name, and an `impl` block each of its
    /// functions, as `S.get`, so that a later definition of any of them replaces the chunk.
    defines: Vec<String>,
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
    /// How the program's method calls were resolved, and its function values and calls of them.
    calls: Calls,
    /// How many lines of the program come before the entry.
    offset: usize,
    /// The entry's statements, to add to the session.
    chunks: Vec<Chunk>,
    /// The program's files and the kept entries before this one, for rendering runtime errors.
    files: SourceMap,
    history: String,
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
            .filter(|old| !redefined(old, &chunks))
            .map(|old| old.text.as_str())
            .collect();
        let offset = history.lines().count();
        let program = format!("{history}{entry}");
        match driver::load(Path::new(ENTRY), &program, sources) {
            // a warning would repeat with every later entry, so the session leaves them out
            (files, Ok(checked)) => Ok(Prepared {
                module: checked.module,
                calls: checked.calls,
                offset,
                chunks,
                files,
                history,
            }),
            (files, Err(Diagnostics(diagnostics))) => Err(diagnostics
                .iter()
                .map(|d| render(d, &files, &history, entry, offset))
                .collect()),
        }
    }

    /// Keeps a prepared entry, replacing any function, enum, struct, or `impl` block it defines
    /// again.
    fn commit(&mut self, prepared: Prepared) {
        self.chunks.retain(|old| !redefined(old, &prepared.chunks));
        self.chunks.extend(prepared.chunks);
    }
}

/// Returns whether one of `chunks` defines anything `old` defines, which it replaces.
fn redefined(old: &Chunk, chunks: &[Chunk]) -> bool {
    old.defines
        .iter()
        .any(|name| chunks.iter().any(|new| new.defines.contains(name)))
}

/// Splits an entry into one chunk per top-level statement, each running from the line the
/// statement starts on to the line before the next one.
///
/// For example, `x = 1\nfn f() -> int: ret x\n` gives `x = 1\n` and `fn f() -> int: ret x\n`, the
/// second defining `f`. An entry that does not parse gives no chunks, since it is never kept.
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
                StmtKind::FunctionDef { name, .. }
                | StmtKind::EnumDef { name, .. }
                | StmtKind::StructDef { name, .. }
                | StmtKind::TraitDef { name, .. } => vec![name.clone()],
                StmtKind::ImplDef {
                    name,
                    trait_name,
                    methods,
                    ..
                } => {
                    let mut defines: Vec<String> = methods
                        .iter()
                        .filter_map(|method| match &method.kind {
                            StmtKind::FunctionDef { name: method, .. } => {
                                Some(format!("{name}.{method}"))
                            }
                            _ => None,
                        })
                        .collect();
                    // a type implements a trait once, so a new block replaces the old one
                    if let Some(trait_name) = trait_name {
                        defines.push(format!("{} for {name}", trait_name.name));
                    }
                    defines
                }
                _ => vec![],
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
) -> std::io::Result<u8> {
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
            let status = if buffer.is_empty() {
                None
            } else {
                submit(&mut session, &buffer, interpreter, err, sources)?
            };
            interpreter.output().flush()?;
            return Ok(status.unwrap_or(0));
        }
        let line = String::from_utf8_lossy(&bytes);
        if buffer.is_empty() && line.trim().is_empty() {
            continue;
        }
        buffer += line.trim_end_matches('\n');
        buffer.push('\n');
        if !needs_more(&buffer) {
            if let Some(status) = submit(&mut session, &buffer, interpreter, err, sources)? {
                return Ok(status);
            }
            buffer.clear();
        }
    }
}

/// Checks and runs one entry, writing any error to `err`, and returns the exit status if it
/// called `os.exit`.
fn submit(
    session: &mut Session,
    entry: &str,
    interpreter: &mut Interpreter,
    err: &mut dyn Write,
    sources: &dyn Sources,
) -> std::io::Result<Option<u8>> {
    let mut prepared = match session.prepare(entry, sources) {
        Ok(prepared) => prepared,
        Err(rendered) => {
            err.write_all(rendered.as_bytes())?;
            return Ok(None);
        }
    };
    let statements = prepared.statements();
    let calls = std::mem::take(&mut prepared.calls);
    let Prepared {
        files,
        history,
        offset,
        ..
    } = &prepared;
    let (files, history, offset) = (files.clone(), history.clone(), *offset);
    // kept even if it fails, since the statements before the failure have already run
    session.commit(prepared);
    let result = interpreter.run_entry(&statements, calls);
    interpreter.output().flush()?;
    match result {
        Ok(()) => Ok(None),
        Err(e) => {
            if let Some(Exit(status)) = e.downcast_ref::<Exit>() {
                return Ok(Some(*status));
            }
            match e.downcast_ref::<Diagnostic>() {
                Some(diagnostic) => {
                    let rendered = render(diagnostic, &files, &history, entry, offset);
                    err.write_all(rendered.as_bytes())?;
                }
                None => writeln!(err, "error: {e}")?,
            }
            Ok(None)
        }
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
        assert!(needs_more("if x:\n"));
        assert!(!needs_more("if x;\n"));
        assert!(needs_more("fn f():\n"));
        assert!(needs_more("if x:\n    y = 1\n"));
        assert!(!needs_more("if x:\n    y = 1\n\n"));
        assert!(!needs_more("if x:\n    y = 1\n   \n"));
    }

    #[test]
    fn a_one_line_block_needs_nothing_more() {
        assert!(!needs_more("fn f() -> int: ret 1\n"));
    }

    #[test]
    fn a_semicolon_in_a_string_or_comment_opens_no_block() {
        assert!(!needs_more("print(\"a;\")\n"));
        assert!(!needs_more("x = 1 # see:\n"));
    }

    #[test]
    fn a_lex_error_needs_nothing_more() {
        assert!(!needs_more("print(\"a:\n"));
    }

    /// Runs a session over `input` with `util.stn` holding `pub fn one() -> int: ret 1`, returning
    /// what it printed to stdout and to stderr.
    ///
    /// For example, `session("x = 1\nx + 1\n")` returns `("2\n", "")`.
    fn session(input: &str) -> (String, String) {
        let mut sources = MapSources::default();
        sources.insert("util.stn", "pub fn one() -> int: ret 1\n");
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
    fn calls_without_a_value_do_not_echo() {
        assert_eq!(session("print(1)\n"), ("1\n".into(), "".into()));
    }

    #[test]
    fn options_and_enum_values_echo() {
        assert_eq!(session("none\nsome(\"a\")\n").0, "none\nsome('a')\n");
        let input = "enum Coin:\n    Heads\n    Tails\n\nc = Coin.Tails\nc\n";
        assert_eq!(session(input), ("Coin.Tails\n".into(), "".into()));
    }

    #[test]
    fn methods_carry_over_and_change_their_receivers() {
        let input = "struct S:\n    n: int\n\nimpl S:\n    fn bump(self):\n        self.n += 1\n\n\
                     s = S(n=0)\ns.bump()\ns.bump()\ns\n";
        assert_eq!(session(input), ("S(n=2)\n".into(), "".into()));
    }

    #[test]
    fn an_impl_block_defined_again_replaces_the_one_with_its_methods() {
        let s = "struct S:\n    n: int\n\n";
        let get = |n: i32| format!("impl S:\n    fn get(self) -> int:\n        ret {n}\n\n");
        let other = "impl S:\n    fn other(self) -> int:\n        ret 3\n\n";
        let input = format!(
            "{s}{}{other}{}S(n=0).get() + S(n=0).other()\n",
            get(1),
            get(2)
        );
        assert_eq!(session(&input), ("5\n".into(), "".into()));
    }

    #[test]
    fn traits_and_their_impl_blocks_carry_over_and_are_replaced() {
        let shape = "trait Shape:\n    fn area(self) -> int\n    fn twice(self) -> int:\n        \
                     ret self.area() * 2\n\nstruct Sq:\n    s: int\n\n";
        let area =
            |n: &str| format!("impl Shape for Sq:\n    fn area(self) -> int:\n        ret {n}\n\n");
        let input = format!(
            "{shape}{}fn f[T: Shape](x: T) -> int:\n    ret x.twice()\n\nf(Sq(s=3))\n{}f(Sq(s=3))\n",
            area("self.s"),
            area("self.s * self.s")
        );
        assert_eq!(session(&input), ("6\n18\n".into(), "".into()));
    }

    #[test]
    fn a_function_keeps_its_methods_when_earlier_entries_move() {
        // redefining `f` drops its first definition, so the lines of the entries after it move
        let input = "fn f() -> int:\n    ret 1\n\nstruct S:\n    n: int\n\nimpl S:\n    \
                     fn bump(self):\n        self.n += 10\n\nfn g(s: S) -> S:\n    s.bump()\n    \
                     ret s\n\nfn f() -> int:\n    ret 2\n\ng(S(n=f()))\n";
        assert_eq!(session(input), ("S(n=12)\n".into(), "".into()));
    }

    #[test]
    fn an_enum_defined_again_replaces_the_old_one() {
        let input = "enum Coin:\n    Heads\n\nenum Coin:\n    Edge\n\nCoin.Edge\n";
        assert_eq!(session(input), ("Coin.Edge\n".into(), "".into()));
    }

    #[test]
    fn struct_values_echo_and_change() {
        let input = "struct P:\n    x: int\n    s: str\n\np = P(s=\"a\", x=1)\np.x += 2\np\np.s\n";
        assert_eq!(session(input), ("P(x=3, s='a')\n'a'\n".into(), "".into()));
    }

    #[test]
    fn a_struct_defined_again_replaces_the_old_one() {
        let input = "struct P:\n    x: int\n\nstruct P:\n    y: str\n\nP(y=\"b\")\n";
        assert_eq!(session(input), ("P(y='b')\n".into(), "".into()));
    }

    #[test]
    fn blank_lines_between_entries_are_skipped() {
        assert_eq!(session("\n  \nx = 3\n\nx\n"), ("3\n".into(), "".into()));
    }

    #[test]
    fn a_multi_line_function_ends_at_a_blank_line() {
        let input = "fn double(n: int) -> int:\n    m = n * 2\n    ret m\n\ndouble(4)\n";
        assert_eq!(session(input), ("8\n".into(), "".into()));
    }

    #[test]
    fn a_loop_runs_once_its_block_ends() {
        let input = "for i in range(3):\n    print(i)\n\nprint(\"done\")\n";
        assert_eq!(session(input).0, "0\n1\n2\ndone\n");
    }

    #[test]
    fn a_block_at_the_end_of_the_input_still_runs() {
        assert_eq!(session("if true:\n    print(5)\n").0, "5\n");
    }

    #[test]
    fn functions_can_be_called_from_a_later_entry() {
        assert_eq!(session("fn one() -> int: ret 1\none() + 1\n").0, "2\n");
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
    fn a_lambda_keeps_its_body_when_a_later_one_takes_its_place() {
        // dropping the first `h` moves the last entry's lambda to the line `g`'s had
        let input = "fn h() -> int:\n    ret 1\n\ng = |x: int| x + 1\nfn h() -> int: ret 2\n\
                     y = 0\nf = |s: str| s + \"!\"\nprint(g(1), f(\"a\"))\n";
        assert_eq!(session(input), ("2 a!\n".into(), "".into()));
    }

    #[test]
    fn a_function_can_be_redefined() {
        let (out, err) = session("fn f() -> int: ret 1\nf()\nfn f() -> int: ret 2\nf()\n");
        assert_eq!((out.as_str(), err.as_str()), ("1\n2\n", ""));
    }

    #[test]
    fn an_earlier_function_calls_the_newest_definition() {
        let input =
            "fn f() -> int: ret 1\nfn g() -> int: ret f() * 10\ng()\nfn f() -> int: ret 2\ng()\n";
        assert_eq!(session(input), ("10\n20\n".into(), "".into()));
    }

    #[test]
    fn an_earlier_function_reads_globals_as_they_are_now() {
        let input = "k = 1\nfn get() -> int: ret k\nget()\nk = 5\nget()\n";
        assert_eq!(session(input), ("1\n5\n".into(), "".into()));
    }

    #[test]
    fn an_earlier_function_prints_with_a_later_display() {
        let input = "struct P:\n    x: int\n\nfn show(p: P) -> str: ret str(p)\nshow(P(x=1))\n\
                     impl display for P:\n    fn display(self) -> str:\n        ret \"p\"\n\nshow(P(x=1))\n";
        assert_eq!(session(input), ("'P(x=1)'\n'p'\n".into(), "".into()));
    }

    #[test]
    fn a_redefinition_that_breaks_earlier_entries_is_rejected() {
        let (out, err) =
            session("fn f() -> int: ret 1\nx = f() + 1\nfn f() -> str: ret \"a\"\nf()\n");
        assert_eq!(out, "1\n");
        assert!(
            err.starts_with("<session>:1:11: error: expected str, found int"),
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
        let chunks = split("x = 1\nfn f() -> int:\n    ret x\n\nprint(1)\n");
        let texts: Vec<&str> = chunks.iter().map(|c| c.text.as_str()).collect();
        assert_eq!(
            texts,
            ["x = 1\n", "fn f() -> int:\n    ret x\n\n", "print(1)\n"]
        );
        assert_eq!(chunks[1].defines, ["f"]);
        assert!(chunks[0].defines.is_empty());
        let chunks = split("impl S:\n    fn a(self):\n        ret\n    fn b(self):\n        ret\n");
        assert_eq!(chunks[0].defines, ["S.a", "S.b"]);
    }

    #[test]
    fn runtime_errors_are_reported_and_the_session_continues() {
        let (out, err) = session("x = 4\n1 / 0\nx\n");
        assert_eq!(out, "4\n");
        assert_eq!(
            err,
            "<stdin>:1:1: error: division by zero\n  |\n1 | 1 / 0\n  | ^^^^^\n"
        );
    }

    #[test]
    fn runtime_errors_in_earlier_entries_name_the_session() {
        let (_, err) = session("fn f(n: int) -> int:\n    ret 1 / n\n\nf(0)\n");
        assert!(
            err.starts_with("<session>:2:9: error: division by zero\n"),
            "{err}"
        );
    }

    #[test]
    fn input_reads_the_line_after_the_entry() {
        let (out, _) = session("name = input().unwrap()\nAda\nprint(\"hi\", name)\n");
        assert_eq!(out, "hi Ada\n");
    }

    #[test]
    fn modules_can_be_used() {
        assert_eq!(session("use util\nutil.one() + 1\n").0, "2\n");
        assert_eq!(session("use util.one\none()\n").0, "1\n");
    }

    #[test]
    fn the_os_module_can_be_used() {
        let platform = crate::stdlib::os::platform();
        assert_eq!(
            session("use os\nos.platform()\n").0,
            format!("'{platform}'\n")
        );
    }

    #[test]
    fn exit_ends_the_session() {
        assert_eq!(
            session("use os\nprint(1)\nos.exit(0)\nprint(2)\n"),
            ("1\n".into(), "".into())
        );
        let (mut out, mut err) = (Vec::new(), Vec::new());
        let input = "use os\nprint(1)\nif true:\n    os.exit(4)\n\nprint(2)\n";
        let error = driver::repl(
            &mut input.as_bytes(),
            &mut out,
            &mut err,
            false,
            &MapSources::default(),
        )
        .unwrap_err();
        assert_eq!(error.downcast_ref::<Exit>(), Some(&Exit(4)));
        assert_eq!((out, err), (b"1\n".to_vec(), vec![]));
    }

    #[test]
    fn prompts_go_to_err() {
        let (mut out, mut err) = (Vec::new(), Vec::new());
        let input = "if true:\n    print(1)\n\n";
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

//! `stone fmt`: rewrites a source file in the one standard layout.
//!
//! Every statement is one line (newlines end statements even inside brackets), so formatting
//! never moves code between lines. It indents each line by four spaces per block, decides the
//! spaces between its tokens, keeps comments, and collapses runs of blank lines to one. The
//! tokens themselves are untouched: literals keep their spelling, and parentheses stay where the
//! author put them. For example, `fn f( a:int )->int:` on one line and `\tret a*2` on the next
//! become `fn f(a: int) -> int:` and `    ret a * 2`.

use crate::diagnostic::{Diagnostic, Diagnostics};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::span::{FileId, Pos, Span};
use crate::token::{Token, TokenType};

/// How many spaces each level of blocks indents.
const INDENT: usize = 4;

/// The width a tab counts for in a line's indentation, as the lexer counts it.
const TAB_SIZE: usize = 4;

/// Formats `source`, returning it in the standard layout, or the errors that keep it from being
/// read. Code that fails to lex or parse is never formatted.
///
/// For example, `format("x=[1,2]\n")` gives `"x = [1, 2]\n"`, and `format("x = \n")` gives the
/// syntax error.
pub fn format(source: &str) -> Result<String, Diagnostics> {
    let (tokens, comments) = lex(source)?;
    // parsed as the prelude, which may also name a variant `none`, so it can be formatted too
    let prelude: Vec<Token> = tokens
        .iter()
        .map(|token| Token {
            span: token.span.in_file(FileId::PRELUDE),
            ..token.clone()
        })
        .collect();
    let (_, errors) = Parser::new(&prelude).parse_recovering();
    if !errors.is_empty() {
        return Err(Diagnostics(
            errors
                .into_iter()
                .map(|error| Diagnostic {
                    span: error.span.in_file(FileId::default()),
                    ..error
                })
                .collect(),
        ));
    }
    let formatted = Layout::new(source, &tokens, &comments).write();

    // the layout only ever changes whitespace, so anything else is a bug here
    let same = lex(&formatted).is_ok_and(|(again, again_comments)| {
        kinds(&again).eq(kinds(&tokens))
            && again_comments
                .iter()
                .map(|c| &c.1)
                .eq(comments.iter().map(|c| &c.1))
    });
    if !same {
        return Err(Diagnostics(vec![Diagnostic::error(
            Span::empty(Pos::new(1, 1)),
            "formatting would change this program, which is a bug in stone fmt; please report it",
        )]));
    }
    Ok(formatted)
}

/// Comments with their spans, as [`Lexer::comments`] gives them.
type Comments = Vec<(Span, String)>;

/// Lexes `source`, keeping its comments.
fn lex(source: &str) -> Result<(Vec<Token>, Comments), Diagnostics> {
    let mut lexer = Lexer::new(source).keeping_comments();
    let tokens = lexer.lex().map_err(|e| Diagnostics(vec![e.into()]))?;
    Ok((tokens, lexer.comments().to_vec()))
}

/// Returns the types of `tokens`, which formatting must leave as they are.
fn kinds(tokens: &[Token]) -> impl Iterator<Item = &TokenType> {
    tokens.iter().map(|token| &token.r#type)
}

/// A line of code: the tokens of one statement or block header.
struct CodeLine<'a> {
    /// The line's number in the source.
    number: usize,
    /// How many blocks it is in.
    level: usize,
    tokens: Vec<&'a Token>,
}

/// What a line of the output holds.
enum Item<'a> {
    Code(CodeLine<'a>),
    /// A comment on a line of its own, with its line number and text.
    Comment(usize, &'a str),
}

impl Item<'_> {
    fn number(&self) -> usize {
        match self {
            Item::Code(line) => line.number,
            Item::Comment(number, _) => *number,
        }
    }
}

/// A source file split into what the output writes, in order.
struct Layout<'a> {
    /// The source's lines, without their line endings.
    lines: Vec<&'a str>,
    items: Vec<Item<'a>>,
    /// The comment after the code on a line, by line number.
    trailing: Vec<(usize, &'a str)>,
}

impl<'a> Layout<'a> {
    fn new(source: &'a str, tokens: &'a [Token], comments: &'a [(Span, String)]) -> Self {
        let lines = source
            .split('\n')
            .map(|line| line.strip_suffix('\r').unwrap_or(line))
            .collect();
        let mut code: Vec<CodeLine> = vec![];
        let mut level = 0;
        let mut current: Vec<&Token> = vec![];
        for token in tokens {
            match token.r#type {
                TokenType::Indent => level += 1,
                TokenType::Dedent => level -= 1,
                TokenType::Newline | TokenType::Eof => {
                    if let Some(first) = current.first() {
                        code.push(CodeLine {
                            number: first.span.start.line,
                            level,
                            tokens: std::mem::take(&mut current),
                        });
                    }
                }
                _ => current.push(token),
            }
        }

        // comments go on their own lines or after the code of theirs
        let mut items = vec![];
        let mut trailing = vec![];
        let mut code = code.into_iter().peekable();
        for (span, text) in comments {
            let line = span.start.line;
            while let Some(next) = code.next_if(|next| next.number < line) {
                items.push(Item::Code(next));
            }
            if code.peek().is_some_and(|next| next.number == line) {
                trailing.push((line, text.as_str()));
            } else {
                items.push(Item::Comment(line, text));
            }
        }
        items.extend(code.map(Item::Code));
        Layout {
            lines,
            items,
            trailing,
        }
    }

    /// Returns the indentation width of source line `number`.
    fn width(&self, number: usize) -> usize {
        self.lines[number - 1]
            .chars()
            .map_while(|ch| match ch {
                ' ' => Some(1),
                '\t' => Some(TAB_SIZE),
                _ => None,
            })
            .sum()
    }

    /// Writes the formatted source.
    fn write(&self) -> String {
        let mut out = String::new();
        // the source widths of the blocks the last line of code is in, outermost first
        let mut widths: Vec<usize> = vec![];
        let mut previous: Option<&Item> = None;
        let mut opened = false;
        for (k, item) in self.items.iter().enumerate() {
            if let Some(previous) = previous
                && item.number() > previous.number() + 1
                && !opened
            {
                out.push('\n');
            }
            match item {
                Item::Code(line) => {
                    widths.truncate(line.level);
                    widths.push(self.width(line.number));
                    indent(&mut out, line.level);
                    out.push_str(&self.code(line));
                    if let Some((_, comment)) = self.trailing.iter().find(|t| t.0 == line.number) {
                        out.push(' ');
                        out.push_str(comment);
                    }
                    opened = self.items[k + 1..].iter().find_map(|item| match item {
                        Item::Code(next) => Some(next.level > line.level),
                        Item::Comment(..) => None,
                    }) == Some(true);
                }
                Item::Comment(number, text) => {
                    let level = if opened {
                        widths.len()
                    } else {
                        // the deepest block whose code starts at or before the comment
                        let width = self.width(*number);
                        widths.iter().rposition(|&w| w <= width).unwrap_or(0)
                    };
                    indent(&mut out, level);
                    out.push_str(text);
                }
            }
            out.push('\n');
            previous = Some(item);
        }
        out
    }

    /// Returns the text of `line`'s code, its tokens with the spaces between them decided.
    fn code(&self, line: &CodeLine) -> String {
        let header = line
            .tokens
            .iter()
            .find(|token| token.r#type != TokenType::Keyword("pub"))
            .is_some_and(|token| {
                matches!(
                    token.r#type,
                    TokenType::Keyword("fn" | "struct" | "enum" | "trait" | "impl")
                )
            });
        let mut spacer = Spacer::new(header);
        let mut out = String::new();
        for token in &line.tokens {
            if spacer.space_before(token) {
                out.push(' ');
            }
            out.push_str(&self.text(token));
        }
        out
    }

    /// Returns how `token` is written: its source text if it is a literal, so its spelling is
    /// kept, and otherwise the text its type stands for.
    fn text(&self, token: &Token) -> String {
        match &token.r#type {
            TokenType::Name(name) => name.clone(),
            TokenType::Keyword(text) | TokenType::Operator(text) => text.to_string(),
            TokenType::LParen => "(".to_string(),
            TokenType::RParen => ")".to_string(),
            TokenType::LBracket => "[".to_string(),
            TokenType::RBracket => "]".to_string(),
            TokenType::LBrace => "{".to_string(),
            TokenType::RBrace => "}".to_string(),
            TokenType::Comma => ",".to_string(),
            TokenType::Colon => ":".to_string(),
            TokenType::Dot => ".".to_string(),
            TokenType::Arrow => "->".to_string(),
            TokenType::VBar => "|".to_string(),
            _ => {
                // a literal always ends on the line it starts on
                let Span { start, end, .. } = token.span;
                self.lines[start.line - 1]
                    .chars()
                    .skip(start.col - 1)
                    .take(end.col - start.col)
                    .collect()
            }
        }
    }
}

/// Appends the indentation of `level` blocks.
fn indent(out: &mut String, level: usize) {
    out.extend(std::iter::repeat_n(' ', INDENT * level));
}

/// What a bracket or a string's value opens.
#[derive(Clone, Copy, PartialEq)]
enum Group {
    /// The line itself, outside any bracket.
    Line,
    Paren,
    /// A `[` right after a value: an index or slice, whose `:`s take no spaces.
    Index,
    /// Any other `[`: a list, or a type's or definition's parameters.
    Square,
    Brace,
    /// A `{value}` in a string.
    Hole,
}

/// A group open at some point of a line.
struct Frame {
    group: Group,
    /// Whether a lambda's `|` has opened its parameters and not closed them yet.
    in_bars: bool,
}

/// Decides, token by token, where a line's spaces go.
struct Spacer {
    frames: Vec<Frame>,
    /// Whether the line is a definition's header, whose `[`s hold type parameters.
    header: bool,
    previous: Option<Kind>,
}

/// What a token is, as far as spacing goes.
#[derive(Clone, Copy, PartialEq)]
enum Kind {
    /// A name or literal.
    Operand,
    Open,
    Close,
    Comma,
    Dot,
    Colon,
    /// A `?`, which ends an operand like a closing bracket.
    Try,
    /// An operator written between two values, or `->`.
    Binary,
    /// A `-` or `+` written before a value.
    Unary,
    /// A `=` giving a struct's field a value, as in `Point(x=1)`.
    Named,
    Keyword,
    /// The `fn` of a function type, as in `fn(int) -> int`.
    FnType,
    OpenBar,
    CloseBar,
    /// The start of a string, up to its first value.
    TextStart,
    /// The text of a string between two of its values.
    TextMiddle,
    /// The text of a string after its last value.
    TextEnd,
}

impl Kind {
    /// Whether a value ends just before what comes next, so `(` and `[` attach to it.
    fn ends_value(self) -> bool {
        matches!(
            self,
            Kind::Operand | Kind::Close | Kind::Try | Kind::TextEnd
        )
    }
}

impl Spacer {
    fn new(header: bool) -> Self {
        Spacer {
            frames: vec![Frame {
                group: Group::Line,
                in_bars: false,
            }],
            header,
            previous: None,
        }
    }

    fn top(&mut self) -> &mut Frame {
        self.frames.last_mut().expect("the line's frame stays")
    }

    /// Returns whether a space goes before `token`, the next token of the line.
    fn space_before(&mut self, token: &Token) -> bool {
        let previous = self.previous;
        let group = self.top().group;
        let kind = match &token.r#type {
            TokenType::LParen | TokenType::LBracket | TokenType::LBrace => Kind::Open,
            TokenType::RParen | TokenType::RBracket | TokenType::RBrace => Kind::Close,
            TokenType::Comma => Kind::Comma,
            TokenType::Dot => Kind::Dot,
            TokenType::Colon => Kind::Colon,
            TokenType::Operator("?") => Kind::Try,
            TokenType::Operator("=") if group == Group::Paren => Kind::Named,
            TokenType::Operator("-" | "+") if previous.is_none_or(|p| !p.ends_value()) => {
                Kind::Unary
            }
            TokenType::Operator(_) | TokenType::Arrow => Kind::Binary,
            TokenType::Keyword("true" | "false" | "none") => Kind::Operand,
            TokenType::Keyword("fn") => Kind::FnType,
            TokenType::Keyword(_) => Kind::Keyword,
            TokenType::VBar if self.top().in_bars => Kind::CloseBar,
            TokenType::VBar => Kind::OpenBar,
            TokenType::StringStart(_) => Kind::TextStart,
            TokenType::StringMiddle(_) => Kind::TextMiddle,
            TokenType::StringEnd(_) => Kind::TextEnd,
            _ => Kind::Operand,
        };
        let space = match (previous, kind) {
            (None, _) => false,
            // `{{` would be a brace in the string's text
            (Some(Kind::TextStart | Kind::TextMiddle), Kind::Open)
                if token.r#type == TokenType::LBrace =>
            {
                true
            }
            (
                Some(
                    Kind::Open
                    | Kind::TextStart
                    | Kind::TextMiddle
                    | Kind::Dot
                    | Kind::Unary
                    | Kind::OpenBar,
                ),
                _,
            ) => false,
            (
                _,
                Kind::Close
                | Kind::Comma
                | Kind::Dot
                | Kind::Colon
                | Kind::Try
                | Kind::CloseBar
                | Kind::TextMiddle
                | Kind::TextEnd,
            ) => false,
            (Some(Kind::Named), _) | (_, Kind::Named) => false,
            (Some(p), Kind::Open)
                if matches!(token.r#type, TokenType::LParen | TokenType::LBracket)
                    && (p.ends_value() || p == Kind::FnType) =>
            {
                false
            }
            (Some(Kind::Colon), _) => group != Group::Index || self.top().in_bars,
            _ => true,
        };

        // then the token opens or closes what it does
        match &token.r#type {
            TokenType::LParen => self.push(Group::Paren),
            TokenType::LBracket => {
                let index = previous.is_some_and(Kind::ends_value)
                    && !(self.header && self.frames.len() == 1);
                self.push(if index { Group::Index } else { Group::Square });
            }
            TokenType::LBrace => self.push(Group::Brace),
            TokenType::StringStart(_) => self.push(Group::Hole),
            TokenType::StringMiddle(_) => {
                self.frames.pop();
                self.push(Group::Hole);
            }
            TokenType::RParen
            | TokenType::RBracket
            | TokenType::RBrace
            | TokenType::StringEnd(_) => {
                if self.frames.len() > 1 {
                    self.frames.pop();
                }
            }
            TokenType::VBar => {
                let frame = self.top();
                frame.in_bars = !frame.in_bars;
            }
            _ => {}
        }
        self.previous = Some(kind);
        space
    }

    fn push(&mut self, group: Group) {
        self.frames.push(Frame {
            group,
            in_bars: false,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Formats `source`, checking that formatting the result changes nothing.
    fn fmt(source: &str) -> String {
        let formatted = format(source).unwrap_or_else(|e| panic!("{source:?}: {e}"));
        assert_eq!(format(&formatted).unwrap(), formatted, "not idempotent");
        formatted
    }

    /// Asserts that each line of `cases`, alone as a program, formats to its pair.
    fn lines(cases: &[(&str, &str)]) {
        for (source, expected) in cases {
            assert_eq!(fmt(&format!("{source}\n")), format!("{expected}\n"));
        }
    }

    #[test]
    fn operators_take_spaces_around_them() {
        lines(&[
            ("x=1+2*3", "x = 1 + 2 * 3"),
            ("x  =  a**2", "x = a ** 2"),
            ("x+=1", "x += 1"),
            ("b = a==1 and not c", "b = a == 1 and not c"),
            ("b = x<=y", "b = x <= y"),
            ("b = 1 in xs", "b = 1 in xs"),
            ("b = 1 not in xs", "b = 1 not in xs"),
        ]);
    }

    #[test]
    fn minus_before_a_value_takes_no_space() {
        lines(&[
            ("x = - 1", "x = -1"),
            ("x = a - -b", "x = a - -b"),
            ("x = a-b", "x = a - b"),
            ("x = f( -1 , - 2)", "x = f(-1, -2)"),
            ("x = -(a+b)", "x = -(a + b)"),
            ("x = xs[- 1]", "x = xs[-1]"),
            ("x = xs[: -1]", "x = xs[:-1]"),
            ("x = f()?-1", "x = f()? - 1"),
        ]);
    }

    #[test]
    fn brackets_and_commas() {
        lines(&[
            ("x = [ 1,2 ,3 ]", "x = [1, 2, 3]"),
            ("x = f (a) [0]", "x = f(a)[0]"),
            ("x = ( 1 , \"a\" )", "x = (1, \"a\")"),
            ("x = []", "x = []"),
            ("m = { \"a\" :1 , \"b\":2 }", "m = {\"a\": 1, \"b\": 2}"),
            ("m = { }", "m = {}"),
            ("print( xs . len ( ) )", "print(xs.len())"),
            ("x = [ (1, 2), (3, 4) ]", "x = [(1, 2), (3, 4)]"),
        ]);
    }

    #[test]
    fn keywords_take_a_space_before_brackets() {
        lines(&[
            ("ret(a, b)", "ret (a, b)"),
            ("x = not(a)", "x = not (a)"),
            (
                "for x in[1, 2]:\n    print(x)",
                "for x in [1, 2]:\n    print(x)",
            ),
        ]);
    }

    #[test]
    fn slices_are_tight_and_annotations_are_not() {
        lines(&[
            ("x = xs[1 : n]", "x = xs[1:n]"),
            ("x = xs[ : : 2]", "x = xs[::2]"),
            ("x = xs[a+1 :]", "x = xs[a + 1:]"),
            (
                "fn first[T :ord]( xs:list[ T ] )->T:\n    ret xs[0]",
                "fn first[T: ord](xs: list[T]) -> T:\n    ret xs[0]",
            ),
            (
                "pub fn f(g:fn( int )->int)->int:\n    ret g(1)",
                "pub fn f(g: fn(int) -> int) -> int:\n    ret g(1)",
            ),
            (
                "struct Pair[A,B]:\n    a:A\n    b : B",
                "struct Pair[A, B]:\n    a: A\n    b: B",
            ),
            (
                "fn total[T: Shape+Named](xs: list[T]) -> int:\n    ret 0",
                "fn total[T: Shape + Named](xs: list[T]) -> int:\n    ret 0",
            ),
        ]);
    }

    #[test]
    fn named_values_take_no_spaces() {
        lines(&[
            (
                "struct P:\n    x: float\n    y: float\np = P(x = 1.0, y= -2.0)",
                "struct P:\n    x: float\n    y: float\np = P(x=1.0, y=-2.0)",
            ),
            (
                "struct P:\n    x: float\nps = [P(x = 1.0)]",
                "struct P:\n    x: float\nps = [P(x=1.0)]",
            ),
        ]);
    }

    #[test]
    fn lambdas() {
        lines(&[
            ("ys = xs.map( | x | x*2 )", "ys = xs.map(|x| x * 2)"),
            (
                "ys = xs.map(|p : int, q:int|p+q)",
                "ys = xs.map(|p: int, q: int| p + q)",
            ),
            ("f = |x|x", "f = |x| x"),
            ("ys = xs.map(|x| f(|y| y))", "ys = xs.map(|x| f(|y| y))"),
        ]);
    }

    #[test]
    fn enums_matches_and_options() {
        lines(&[
            (
                "enum Shape:\n    Circle( float )\n    Empty\nmatch Shape.Circle(1.0):\n    case Shape.Circle( r ):\n        print(r)\n    case _ :\n        print(0)",
                "enum Shape:\n    Circle(float)\n    Empty\nmatch Shape.Circle(1.0):\n    case Shape.Circle(r):\n        print(r)\n    case _:\n        print(0)",
            ),
            (
                "fn f(x: option[int]) -> option[int]:\n    ret some(x ? + 1)",
                "fn f(x: option[int]) -> option[int]:\n    ret some(x? + 1)",
            ),
        ]);
    }

    #[test]
    fn values_in_strings_are_spaced_like_code() {
        lines(&[
            (
                r#"print("{ x+1 } and {xs[ 0 ]}!")"#,
                r#"print("{x + 1} and {xs[0]}!")"#,
            ),
            (
                r#"print("a{ {"k":1}["k"] }b")"#,
                r#"print("a{ {"k": 1}["k"]}b")"#,
            ),
            (
                r#"print("{{ not a value }}")"#,
                r#"print("{{ not a value }}")"#,
            ),
        ]);
    }

    #[test]
    fn literals_keep_their_spelling() {
        lines(&[
            ("x = 1e3", "x = 1e3"),
            ("x = 2.50", "x = 2.50"),
            (r#"s = "\x41\t\"""#, r#"s = "\x41\t\"""#),
            (r#"b = b"\xff\x00a""#, r#"b = b"\xff\x00a""#),
            ("s = \"é ü\"", "s = \"é ü\""),
        ]);
    }

    #[test]
    fn blocks_indent_four_spaces() {
        assert_eq!(
            fmt("if a:\n  if b:\n\t    x = 1\n  y = 2\nz = 3\n"),
            "if a:\n    if b:\n        x = 1\n    y = 2\nz = 3\n"
        );
        assert_eq!(fmt("if a:\r\n\tx = 1\r\n"), "if a:\n    x = 1\n");
    }

    #[test]
    fn blank_lines_collapse() {
        assert_eq!(fmt("\n\nx = 1\n\n\n\ny = 2\n\n\n"), "x = 1\n\ny = 2\n");
        assert_eq!(
            fmt("fn f():\n\n    x = 1\n\n    y = 2\n"),
            "fn f():\n    x = 1\n\n    y = 2\n"
        );
        assert_eq!(fmt("x = 1  \t\n"), "x = 1\n");
        assert_eq!(fmt(""), "");
        assert_eq!(fmt("\n\n"), "");
    }

    #[test]
    fn comments_stay_where_they_are() {
        assert_eq!(
            fmt("// top\nx=1   // one  \n\n   // stray\ny = 2\n"),
            "// top\nx = 1 // one\n\n// stray\ny = 2\n"
        );
        // a comment in a block, after a block, and before a block's first line
        assert_eq!(
            fmt("if a:\n  x = 1\n  // in\n// out\nfn f():\n// first\n  ret\n"),
            "if a:\n    x = 1\n    // in\n// out\nfn f():\n    // first\n    ret\n"
        );
        assert_eq!(
            fmt("if a:\n    if b:\n        x = 1\n      // between\n    y = 2\n"),
            "if a:\n    if b:\n        x = 1\n    // between\n    y = 2\n"
        );
        assert_eq!(fmt("// only\n"), "// only\n");
        assert_eq!(fmt("x = \"a // b\" // c\n"), "x = \"a // b\" // c\n");
    }

    #[test]
    fn code_that_does_not_parse_is_left_alone() {
        let errors = format("x = \n").unwrap_err();
        assert_eq!(errors.0[0].span.start.line, 1);
        assert!(format("x = 99999999999999999999\n").is_err());
        assert!(format("x = 1;\n").is_err());
        assert_eq!(errors.0[0].span.file, FileId::default());
    }

    #[test]
    fn the_prelude_can_be_formatted() {
        assert_eq!(
            fmt("enum option[T]:\n  none\n  some( T )\n"),
            "enum option[T]:\n    none\n    some(T)\n"
        );
    }
}

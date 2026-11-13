//! Error codes: one [`Code`] per kind of error the lexer, parser, linker, package reader, and
//! checker report, each with an [`Explanation`] that `stone explain` prints and
//! `docs/reference/errors.md` lists.
//!
//! An error with a code renders as `main.stn:1:5: error[E0006]: expected int, found str`.
//! Warnings and runtime errors have no code. Codes are numbered in the order of [`Code::ALL`]
//! and never reused, so a kind that is no longer reported keeps its number.

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// A kind of error, such as [`Code::Mismatch`] for `expected int, found str`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Code {
    Syntax,
    Literal,
    Undefined,
    NoMember,
    Mismatch,
    Arity,
    UntypedParam,
    ReturnType,
    NoValue,
    Unassigned,
    WrongKind,
    Condition,
    Uncomparable,
    Unordered,
    MapKey,
    Unchangeable,
    ChangeOutside,
    ChangeConflict,
    AssignTarget,
    Tuple,
    Duplicate,
    Reserved,
    Placement,
    NotAValue,
    CallForm,
    Construction,
    NonExhaustive,
    Pattern,
    InfiniteType,
    TypeParameter,
    Bound,
    MissingMethods,
    TraitSignature,
    ImplNotAllowed,
    Ambiguous,
    UnknownType,
    Conversion,
    Try,
    TooManyValues,
    SelfParam,
    TestGlobal,
    ChainedIn,
    FormatSpec,
    UnknownModule,
    Private,
    Unsupported,
    Manifest,
    Dependency,
    TraitValue,
    Constant,
}

/// What `stone explain` says about a [`Code`]: a title, a longer text, and a program that
/// fails with the code beside one that fixes it.
///
/// An example is a list of files, each a path and its source, whose entry is `main.stn`, so an
/// example can hold a module or a `stone.toml` too.
#[derive(Debug)]
pub struct Explanation {
    pub code: Code,
    /// The code as written, `E` and four digits, such as `E0006`.
    pub id: &'static str,
    /// A short description of the kind of error, such as `mismatched types`.
    pub title: &'static str,
    /// The explanation, in Markdown paragraphs separated by blank lines.
    pub text: &'static str,
    /// A program that fails with this code.
    pub failing: &'static [(&'static str, &'static str)],
    /// The same program, fixed.
    pub fixed: &'static [(&'static str, &'static str)],
    /// The target the examples are built for, for an error only one target reports, or `None`
    /// for `stone check` on the host.
    pub target: Option<&'static str>,
}

impl Code {
    /// Every code, in the order of their numbers.
    pub const ALL: &'static [Code] = &[
        Code::Syntax,
        Code::Literal,
        Code::Undefined,
        Code::NoMember,
        Code::Mismatch,
        Code::Arity,
        Code::UntypedParam,
        Code::ReturnType,
        Code::NoValue,
        Code::Unassigned,
        Code::WrongKind,
        Code::Condition,
        Code::Uncomparable,
        Code::Unordered,
        Code::MapKey,
        Code::Unchangeable,
        Code::ChangeOutside,
        Code::ChangeConflict,
        Code::AssignTarget,
        Code::Tuple,
        Code::Duplicate,
        Code::Reserved,
        Code::Placement,
        Code::NotAValue,
        Code::CallForm,
        Code::Construction,
        Code::NonExhaustive,
        Code::Pattern,
        Code::InfiniteType,
        Code::TypeParameter,
        Code::Bound,
        Code::MissingMethods,
        Code::TraitSignature,
        Code::ImplNotAllowed,
        Code::Ambiguous,
        Code::UnknownType,
        Code::Conversion,
        Code::Try,
        Code::TooManyValues,
        Code::SelfParam,
        Code::TestGlobal,
        Code::ChainedIn,
        Code::FormatSpec,
        Code::UnknownModule,
        Code::Private,
        Code::Unsupported,
        Code::Manifest,
        Code::Dependency,
        Code::TraitValue,
        Code::Constant,
    ];

    /// Returns the code as written, such as `E0006` for [`Code::Mismatch`].
    pub fn id(self) -> &'static str {
        self.explanation().id
    }

    /// Returns what `stone explain` says about the code.
    pub fn explanation(self) -> &'static Explanation {
        &EXPLANATIONS[self as usize]
    }

    /// Makes an error of this kind, as `Diagnostic::error` does with the code added.
    ///
    /// For example, `Code::Mismatch.error(span, "expected int, found str")` renders as
    /// `error[E0006]: expected int, found str`.
    pub fn error(self, span: Span, message: impl Into<String>) -> Diagnostic {
        Diagnostic::error(span, message).with_code(self.id())
    }
}

/// Where the published documentation lists every code, each at an anchor of its own.
pub const ERRORS_PAGE: &str = "https://tarolling.github.io/stone/reference/errors.html";

/// Returns the address of the documentation of the code `id`, for editors to link.
///
/// For example, `docs_url("E0005")` gives
/// `https://tarolling.github.io/stone/reference/errors.html#e0005`.
pub fn docs_url(id: &str) -> String {
    format!("{ERRORS_PAGE}#{}", id.to_ascii_lowercase())
}

/// Returns what `stone explain` prints for `code`: its id and title, the explanation, then the
/// failing example and the fixed one, each file indented under its name when there are several.
pub fn text(code: Code) -> String {
    let explanation = code.explanation();
    let mut out = format!(
        "{}: {}\n\n{}\n",
        explanation.id, explanation.title, explanation.text
    );
    let examples = [
        (
            format!("For example, this program fails with {}:", explanation.id),
            explanation.failing,
        ),
        ("This one is fixed:".to_string(), explanation.fixed),
    ];
    for (heading, files) in examples {
        out.push_str(&format!("\n{heading}\n"));
        for (path, source) in files {
            if files.len() > 1 {
                out.push_str(&format!("\n{path}:\n"));
            }
            out.push('\n');
            for line in source.lines() {
                match line.is_empty() {
                    true => out.push('\n'),
                    false => out.push_str(&format!("    {line}\n")),
                }
            }
        }
    }
    out
}

/// Returns `docs/reference/errors.md`, which lists every code with its explanation and
/// examples, under an anchor such as `#e0005` that editors link to.
pub fn markdown() -> String {
    let mut out = String::from(
        "# Error codes\n\n\
         Every error the checker, the linker, or the parser reports has a code, which an error \
         shows after its severity, as in `main.stn:2:5: error[E0005]: cannot assign str to 'x', \
         which is int`. `stone explain E0005` prints what a code means, with an example of the \
         error and its fix, and editors link each error to its code below. Warnings and \
         [runtime errors](runtime-errors.md) have no code.\n",
    );
    for code in Code::ALL {
        let explanation = code.explanation();
        let id = explanation.id;
        out.push_str(&format!(
            "\n({})=\n## {id}: {}\n\n{}\n",
            id.to_ascii_lowercase(),
            explanation.title,
            explanation.text
        ));
        let examples = [
            (
                format!("This program fails with {id}:"),
                explanation.failing,
            ),
            ("This one is fixed:".to_string(), explanation.fixed),
        ];
        for (heading, files) in examples {
            out.push_str(&format!("\n{heading}\n"));
            for (path, source) in files {
                let language = match path.ends_with(".stn") {
                    true => "stone",
                    false => "toml",
                };
                if files.len() > 1 {
                    out.push_str(&format!("\n`{path}`:\n"));
                }
                out.push_str(&format!("\n```{language}\n{source}```\n"));
            }
        }
        if let Some(target) = explanation.target {
            out.push_str(&format!(
                "\nOnly a build for `{target}` reports this error.\n"
            ));
        }
    }
    out
}

/// Returns the line `stone check`, `run`, and `build` print after errors that have codes,
/// pointing to `stone explain`, or `None` if none has a code.
///
/// For example, after one `E0005` it is ``For more about this error, try `stone explain E0005`.``
pub fn hint(diagnostics: &[Diagnostic]) -> Option<String> {
    let mut codes: Vec<&str> = diagnostics.iter().filter_map(Diagnostic::code).collect();
    let first = *codes.first()?;
    codes.sort_unstable();
    codes.dedup();
    Some(match codes.len() {
        1 => format!("For more about this error, try `stone explain {first}`.\n"),
        _ => format!(
            "For more about these errors, try `stone explain` with a code, as in \
             `stone explain {first}`.\n"
        ),
    })
}

/// Returns the code written as `text`, such as `E0006`, `e0006`, or `6`, if there is one.
pub fn find(text: &str) -> Option<Code> {
    let digits = text.trim();
    let digits = digits
        .strip_prefix('E')
        .or_else(|| digits.strip_prefix('e'))
        .unwrap_or(digits);
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    let number: usize = digits.parse().ok()?;
    Code::ALL.get(number.checked_sub(1)?).copied()
}

const EXPLANATIONS: &[Explanation] = &[
    Explanation {
        code: Code::Syntax,
        id: "E0001",
        title: r#"the program does not parse"#,
        text: r#"The program is not written in stone's grammar, such as a block that does not open with `:`,
a bracket that is never closed, or values separated by commas without parentheses. The error
points at the first token the parser could not use and says what it expected there."#,
        failing: &[(
            r#"main.stn"#,
            r#"x = 2
if x > 1
    print(x)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"x = 2
if x > 1:
    print(x)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Literal,
        id: "E0002",
        title: r#"a character or literal that is not valid"#,
        text: r#"The source holds text that is no token of stone: an int literal too large for an int, a
string that never ends, an escape such as `\q` that stone does not have, a character such as
`;` that stone never uses, or a bytes literal with a character outside ASCII."#,
        failing: &[(
            r#"main.stn"#,
            r#"x = 99999999999999999999
print(x)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"x = 999999999999999999
print(x)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Undefined,
        id: "E0003",
        title: r#"a name that is not defined, or not that kind of name"#,
        text: r#"A name is used that nothing defines: no variable, function, type, or trait of that name is
visible there. A misspelled name gets help naming the defined one it is closest to. The same
code covers a name of the wrong kind, such as a struct written where a trait is expected."#,
        failing: &[(
            r#"main.stn"#,
            r#"total = 3
print(totl)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"total = 3
print(total)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::NoMember,
        id: "E0004",
        title: r#"no such method, field, variant, or function"#,
        text: r#"A value, type, or module has no member of that name: a struct with no such field or method,
an enum with no such variant, or a module with no such function. Like an undefined name, a
misspelled member gets help naming the closest one."#,
        failing: &[(
            r#"main.stn"#,
            r#"struct Point:
    x: float
    y: float

p = Point(x=1.0, y=2.0)
print(p.z)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"struct Point:
    x: float
    y: float

p = Point(x=1.0, y=2.0)
print(p.x + p.y)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Mismatch,
        id: "E0005",
        title: r#"mismatched types"#,
        text: r#"A value has a type other than the one its place needs: an argument that does not fit its
parameter, a variable assigned a value of another type than before, or list elements of
different types. The error labels the place that says which type was expected, such as the
parameter's written type. An int and a float never mix: convert one with `float(n)` or
`.floor()`."#,
        failing: &[(
            r#"main.stn"#,
            r#"fn double(n: int) -> int:
    ret n * 2

print(double("4"))
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"fn double(n: int) -> int:
    ret n * 2

print(double(4))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Arity,
        id: "E0006",
        title: r#"the wrong number of arguments or types"#,
        text: r#"A call gives a function more or fewer arguments than it has parameters, or a generic type is
written with more or fewer types than it takes, as in `list[int, str]`. The error labels the
function's definition."#,
        failing: &[(
            r#"main.stn"#,
            r#"fn double(n: int) -> int:
    ret n * 2

print(double(4, 2))
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"fn double(n: int) -> int:
    ret n * 2

print(double(4))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::UntypedParam,
        id: "E0007",
        title: r#"a parameter without a type"#,
        text: r#"Every parameter of a function has a written type, so that a function's signature is a
contract that its calls are checked against. The help suggests the type the function's calls
give the parameter."#,
        failing: &[(
            r#"main.stn"#,
            r#"fn double(n) -> int:
    ret n * 2

print(double(4))
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"fn double(n: int) -> int:
    ret n * 2

print(double(4))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::ReturnType,
        id: "E0008",
        title: r#"a value that does not fit the return type"#,
        text: r#"What a function returns does not fit its signature: a function without `-> T` returns a
value, a `ret` value is not of the written return type, a `ret` without a value is in a
function that returns one, or some path through the function ends without returning. A
function that returns a value writes `-> T` after its parameters."#,
        failing: &[(
            r#"main.stn"#,
            r#"fn half(n: int):
    ret n / 2

print(half(4))
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"fn half(n: int) -> int:
    ret n / 2

print(half(4))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::NoValue,
        id: "E0009",
        title: r#"a call that returns no value used as a value"#,
        text: r#"A function without `-> T` returns no value, so its call can only be a statement of its own.
The same holds for a method that changes its receiver and returns nothing, such as `append`."#,
        failing: &[(
            r#"main.stn"#,
            r#"fn greet(name: str):
    print("hello, {name}")

x = greet("ada")
print(x)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"fn greet(name: str):
    print("hello, {name}")

greet("ada")
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Unassigned,
        id: "E0010",
        title: r#"a variable that might be used before it is assigned"#,
        text: r#"A variable is read where some path to the read skips every assignment to it, such as an
assignment inside an `if` without an `else`, or inside a loop that might not run. Assign it
on every path, for example before the `if`."#,
        failing: &[(
            r#"main.stn"#,
            r#"n = int(input().unwrap_or("0")).unwrap_or(0)
if n > 0:
    sign = "positive"
print(sign)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"n = int(input().unwrap_or("0")).unwrap_or(0)
sign = "not positive"
if n > 0:
    sign = "positive"
print(sign)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::WrongKind,
        id: "E0011",
        title: r#"a value of the wrong kind for an operation"#,
        text: r#"An operator, builtin, or method is given a kind of value it does not work on, such as `-` on
strings, `len` on an int, or `math.sqrt` of a string. The message says which kinds it takes."#,
        failing: &[(
            r#"main.stn"#,
            r#"print("stone" - "s")
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"print("stone".replace("s", ""))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Condition,
        id: "E0012",
        title: r#"a condition that is not a bool"#,
        text: r#"The condition of an `if` or `while`, and the operands of `and`, `or`, and `not`, must be
bools. stone has no truthiness: compare instead, as in `if count > 0:` or `if xs.len() > 0:`."#,
        failing: &[(
            r#"main.stn"#,
            r#"count = 3
if count:
    print("some")
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"count = 3
if count > 0:
    print("some")
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Uncomparable,
        id: "E0013",
        title: r#"values that cannot be compared with `==`"#,
        text: r#"`==` and `!=` compare values part by part, but functions and handles such as sockets have no
parts to compare, so a value that is or holds one cannot be compared. A type parameter is
compared only when it is bounded by `eq`, as in `[T: eq]`."#,
        failing: &[(
            r#"main.stn"#,
            r#"fn double(n: int) -> int:
    ret n * 2

fn triple(n: int) -> int:
    ret n * 3

print(double == triple)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"fn double(n: int) -> int:
    ret n * 2

fn triple(n: int) -> int:
    ret n * 3

print(double(1) == triple(1))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Unordered,
        id: "E0014",
        title: r#"values that cannot be ordered"#,
        text: r#"`<`, `>`, `sort`, `min`, and `max` need values that have an order: ints, floats, strings,
bytes, or a type that implements `ord` with a `cmp` method. A struct has no order until it
implements one."#,
        failing: &[(
            r#"main.stn"#,
            r#"struct Point:
    x: int
    y: int

a = Point(x=1, y=2)
b = Point(x=2, y=1)
print(a < b)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"struct Point:
    x: int
    y: int

impl ord for Point:
    fn cmp(self, other: Self) -> ordering:
        ret self.x.cmp(other.x)

a = Point(x=1, y=2)
b = Point(x=2, y=1)
print(a < b)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::MapKey,
        id: "E0015",
        title: r#"a map key or set element of a type that cannot be one"#,
        text: r#"A map's keys and a set's elements are ints, strings, or bools, which hash and compare the same
in both of stone's backends. Make a key of another value with a string, as in `"{x},{y}"`."#,
        failing: &[(
            r#"main.stn"#,
            r#"seen = {[1, 2]: true}
print(seen.len())
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"seen = {"1,2": true}
print(seen.len())
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Unchangeable,
        id: "E0016",
        title: r#"a change to a value that cannot change"#,
        text: r#"Strings, bytes, and tuples never change, and neither does a slice or a value that is not
stored anywhere, so an assignment to one of their elements is an error. Make a new value
instead, as in `name = "S" + name[1:]`."#,
        failing: &[(
            r#"main.stn"#,
            r#"name = "stone"
name[0] = "S"
print(name)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"name = "stone"
name = "S" + name[1:]
print(name)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::ChangeOutside,
        id: "E0017",
        title: r#"a change to a variable a function does not own"#,
        text: r#"stone's values are copies, so a function sees its own copy of each argument and cannot change
a global, and a lambda cannot change the variables it captures, since it holds copies of them.
To change a value in a function, pass it in and return it, as in `xs = f(xs)`, which stone
turns into a change in place."#,
        failing: &[(
            r#"main.stn"#,
            r#"scores = [1, 2]

fn add_score(n: int):
    scores.append(n)

add_score(3)
print(scores)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"fn add_score(scores: list[int], n: int) -> list[int]:
    scores.append(n)
    ret scores

scores = [1, 2]
scores = add_score(scores, 3)
print(scores)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::ChangeConflict,
        id: "E0018",
        title: r#"a value changed and used in the same statement"#,
        text: r#"A call that changes a value, such as `xs.pop()`, may be part of a larger expression, but
nothing else in that statement may use the value it changes, since which one comes first would
decide the result. Make the change in a statement of its own."#,
        failing: &[(
            r#"main.stn"#,
            r#"xs = [1, 2, 3]
print(xs.pop().unwrap() + xs.len())
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"xs = [1, 2, 3]
last = xs.pop().unwrap()
print(last + xs.len())
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::AssignTarget,
        id: "E0019",
        title: r#"an assignment to something that is not a variable or a place"#,
        text: r#"Only a variable, an element such as `xs[i]`, or a field such as `p.x` can be assigned, and a
tuple is unpacked only into names. A function, builtin, enum, or struct name cannot be
assigned."#,
        failing: &[(
            r#"main.stn"#,
            r#"fn answer() -> int:
    ret 42

answer = 41
print(answer())
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"fn answer() -> int:
    ret 42

guess = 41
print(guess, answer())
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Tuple,
        id: "E0020",
        title: r#"a tuple used as something it is not"#,
        text: r#"A tuple holds a fixed number of values of fixed types, so it cannot be iterated, sliced, or
indexed by a variable, and it unpacks only into as many names as it has values. Unpack it to
name its values, as in `n, name = pair`, or use a list."#,
        failing: &[(
            r#"main.stn"#,
            r#"pair = (1, "one")
for x in pair:
    print(x)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"pair = (1, "one")
n, name = pair
print(n, name)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Duplicate,
        id: "E0021",
        title: r#"a name defined twice"#,
        text: r#"A function, type, trait, parameter, field, variant, or method is defined twice in the same
place, so a use of the name could mean either. stone has no overloading: give each its own
name."#,
        failing: &[(
            r#"main.stn"#,
            r#"fn area(r: float) -> float:
    ret 3.14 * r * r

fn area(w: float, h: float) -> float:
    ret w * h

print(area(2.0))
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"fn circle_area(r: float) -> float:
    ret 3.14 * r * r

fn rectangle_area(w: float, h: float) -> float:
    ret w * h

print(circle_area(2.0), rectangle_area(2.0, 3.0))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Reserved,
        id: "E0022",
        title: r#"a name that belongs to stone"#,
        text: r#"A program defines something with the name of a builtin, such as `print` or `input`, a builtin
method, a builtin module, or the prelude. Those names always mean stone's own, so pick
another."#,
        failing: &[(
            r#"main.stn"#,
            r#"fn input() -> str:
    ret "ada"

print(input())
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"fn name() -> str:
    ret "ada"

print(name())
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Placement,
        id: "E0023",
        title: r#"a statement or expression where it is not allowed"#,
        text: r#"Some code can only appear in certain places: functions, types, traits, and `impl` blocks at
the top level of a file, `use` before other statements, `break` and `cont` inside a loop,
`range` as the iterable of a `for` loop, and a slice inside brackets."#,
        failing: &[(
            r#"main.stn"#,
            r#"for i in range(3):
    print(i)
break
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"for i in range(3):
    if i == 2:
        break
    print(i)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::NotAValue,
        id: "E0024",
        title: r#"a name used as a value that is not one"#,
        text: r#"An enum, a struct, a module, or a builtin is not itself a value. A value of an enum is one of
its variants, a struct's value is made with a call, and to pass a builtin around, define a
function that calls it."#,
        failing: &[(
            r#"main.stn"#,
            r#"enum Color:
    Red
    Green

c = Color
print(c)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"enum Color:
    Red
    Green

c = Color.Red
print(c, Color.Green)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::CallForm,
        id: "E0025",
        title: r#"something called the wrong way"#,
        text: r#"A method takes `self` and is called on a value, as in `c.count()`, while an associated
function has no `self` and is called on its struct, as in `Counter.new()`. A method must be
called, a constant is not a function, and only a function value can be called."#,
        failing: &[(
            r#"main.stn"#,
            r#"struct Counter:
    n: int

impl Counter:
    fn new() -> Counter:
        ret Counter(n=0)

c = Counter(n=1)
print(c.new())
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"struct Counter:
    n: int

impl Counter:
    fn new() -> Counter:
        ret Counter(n=0)

c = Counter(n=1)
print(Counter.new(), c)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Construction,
        id: "E0026",
        title: r#"a struct or variant made with the wrong values"#,
        text: r#"A struct is made with a named value for every field, in any order, as in `Point(x=1, y=2)`, and
a variant with its values in order, or by name for a variant with named values. A missing,
repeated, or unknown field is an error, and so is a variant given the wrong number of values."#,
        failing: &[(
            r#"main.stn"#,
            r#"struct Point:
    x: int
    y: int

p = Point(1, 2)
print(p)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"struct Point:
    x: int
    y: int

p = Point(x=1, y=2)
print(p)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::NonExhaustive,
        id: "E0027",
        title: r#"a `match` that does not cover every value"#,
        text: r#"A `match` must have a case for every value its subject can have, so that no value falls
through. The error names a value no case covers; add a case for it, or `case _:` for every
value the other cases leave out."#,
        failing: &[(
            r#"main.stn"#,
            r#"enum Shape:
    Circle(float)
    Square(float)

fn describe(s: Shape):
    match s:
        case Shape.Circle(r):
            print("a circle of radius {r}")

describe(Shape.Square(2.0))
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"enum Shape:
    Circle(float)
    Square(float)

fn describe(s: Shape):
    match s:
        case Shape.Circle(r):
            print("a circle of radius {r}")
        case Shape.Square(side):
            print("a square of side {side}")

describe(Shape.Square(2.0))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Pattern,
        id: "E0028",
        title: r#"a pattern that does not fit"#,
        text: r#"A `case` pattern does not fit the value it matches: a variant with the wrong number of values,
a variant of another enum, a name bound twice, or alternatives joined by `|` that bind
different names. A bare name in a pattern binds a new variable; write a variant with its
enum, as in `Shape.Circle(r)`."#,
        failing: &[(
            r#"main.stn"#,
            r#"enum Shape:
    Circle(float)
    Square(float)

s = Shape.Circle(1.0)
match s:
    case Shape.Circle(r, extra):
        print(r)
    case _:
        print("not a circle")
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"enum Shape:
    Circle(float)
    Square(float)

s = Shape.Circle(1.0)
match s:
    case Shape.Circle(r):
        print(r)
    case Shape.Square(side):
        print(side)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::InfiniteType,
        id: "E0029",
        title: r#"a type that would never end"#,
        text: r#"A struct that holds itself directly would never end, since each value would hold another. Hold
it through a list or an option, as in `next: option[Node]`. The same code covers a generic type
or function whose types would grow without end."#,
        failing: &[(
            r#"main.stn"#,
            r#"struct Node:
    value: int
    next: Node

print("never")
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"struct Node:
    value: int
    next: option[Node]

n = Node(value=1, next=none)
print(n.value)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::TypeParameter,
        id: "E0030",
        title: r#"a type parameter used where it cannot be"#,
        text: r#"A type parameter stands for any type, so it is only meaningful inside its own definition: it
cannot be the type of a global, and only a function's type parameters can have bounds for now.
A struct's or enum's type parameter takes none."#,
        failing: &[(
            r#"main.stn"#,
            r#"struct Wrapper[T: display]:
    value: T

w = Wrapper(value=1)
print(w.value)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"struct Wrapper[T]:
    value: T

w = Wrapper(value=1)
print(w.value)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Bound,
        id: "E0031",
        title: r#"a type that does not implement a trait it needs"#,
        text: r#"A generic function's bounds say which traits its type parameters must implement, and a call
gives it a type that does not implement one of them. Implement the trait for the type, or pass
a value of a type that implements it."#,
        failing: &[(
            r#"main.stn"#,
            r#"fn smallest[T: ord](a: T, b: T) -> T:
    if a < b:
        ret a
    ret b

struct Point:
    x: int

print(smallest(Point(x=1), Point(x=2)))
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"fn smallest[T: ord](a: T, b: T) -> T:
    if a < b:
        ret a
    ret b

print(smallest(3, 2))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::MissingMethods,
        id: "E0032",
        title: r#"an impl block that leaves out a trait's methods"#,
        text: r#"An `impl Trait for Type` block must implement every method of the trait that has no default
body. The error names the methods it leaves out."#,
        failing: &[(
            r#"main.stn"#,
            r#"trait Shape:
    fn area(self) -> float
    fn name(self) -> str

struct Square:
    side: float

impl Shape for Square:
    fn area(self) -> float:
        ret self.side * self.side

print(Square(side=2.0).area())
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"trait Shape:
    fn area(self) -> float
    fn name(self) -> str

struct Square:
    side: float

impl Shape for Square:
    fn area(self) -> float:
        ret self.side * self.side

    fn name(self) -> str:
        ret "square"

print(Square(side=2.0).area())
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::TraitSignature,
        id: "E0033",
        title: r#"a method that does not match its trait"#,
        text: r#"A method that implements a trait takes and returns exactly what the trait's method says, takes
`self` first, and changes `self` only if the trait's method does. The error shows the
signature to write."#,
        failing: &[(
            r#"main.stn"#,
            r#"trait Shape:
    fn area(self) -> float

struct Square:
    side: float

impl Shape for Square:
    fn area(self) -> int:
        ret 4

print(Square(side=2.0).area())
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"trait Shape:
    fn area(self) -> float

struct Square:
    side: float

impl Shape for Square:
    fn area(self) -> float:
        ret self.side * self.side

print(Square(side=2.0).area())
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::ImplNotAllowed,
        id: "E0034",
        title: r#"an impl block that is not allowed"#,
        text: r#"An `impl` block is not allowed where it would change what stone already defines or what
another file decides: a builtin type already implements `display`, `eq`, `ord`, and its
operators, two impls of one trait cannot overlap, and an impl goes in the file of its type or
of its trait."#,
        failing: &[(
            r#"main.stn"#,
            r#"impl display for int:
    fn display(self) -> str:
        ret "a number"

print(1)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"struct Money:
    cents: int

impl display for Money:
    fn display(self) -> str:
        ret "{self.cents / 100}.{self.cents % 100:02} dollars"

print(Money(cents=1234))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Ambiguous,
        id: "E0035",
        title: r#"a call that could mean more than one function"#,
        text: r#"A call or operator could reach more than one function and nothing decides which: a method that
two bounds of a type parameter both have, or a method or operator whose arguments' types are
not known yet. Rename one of the methods, or give the arguments' types."#,
        failing: &[(
            r#"main.stn"#,
            r#"trait Named:
    fn label(self) -> str

trait Titled:
    fn label(self) -> str

fn show[T: Named + Titled](x: T):
    print(x.label())
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"trait Named:
    fn label(self) -> str

trait Titled:
    fn title(self) -> str

struct Book:
    name: str

impl Named for Book:
    fn label(self) -> str:
        ret self.name

impl Titled for Book:
    fn title(self) -> str:
        ret "The {self.name}"

fn show[T: Named + Titled](x: T):
    print(x.label(), x.title())

show(Book(name="Stone"))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::UnknownType,
        id: "E0036",
        title: r#"a value whose type must be known first"#,
        text: r#"Reading a field, calling a method, or converting a value needs its type, and nothing has said
what it is by then, such as a lambda's parameter whose type no use gives. Write the type, as
in `|p: Point| p.x`."#,
        failing: &[(
            r#"main.stn"#,
            r#"struct Point:
    x: int

get_x = |p| p.x
print(get_x(Point(x=1)))
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"struct Point:
    x: int

get_x = |p: Point| p.x
print(get_x(Point(x=1)))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Conversion,
        id: "E0037",
        title: r#"a conversion that does not exist"#,
        text: r#"`T(x)` converts `x` to the type `T` only if `T` implements `from` or `try_from` for `x`'s type,
or stone has the conversion builtin, such as `float(n)` or `int(s)`. There is no conversion
from a list to an int; say what the int should be, as in `xs.len()`."#,
        failing: &[(
            r#"main.stn"#,
            r#"xs = [1, 2]
n = int(xs)
print(n)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"xs = [1, 2]
n = xs.len()
print(n)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Try,
        id: "E0038",
        title: r#"`?` where it cannot return"#,
        text: r#"`x?` gives the value of an `ok` or a `some`, or returns the `err` or `none` from the
function it is in, so that function must return a result with the same error type, or an
option. `?` cannot appear at the top level."#,
        failing: &[(
            r#"main.stn"#,
            r#"fn parse(s: str) -> int:
    ret int(s)?

print(parse("4"))
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"fn parse(s: str) -> result[int, str]:
    ret ok(int(s)? * 2)

print(parse("4").unwrap())
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::TooManyValues,
        id: "E0039",
        title: r#"a value that holds too many values"#,
        text: r#"A list element, map entry, variant, or closure holds at most 32 values, counting each value of
a tuple or struct inside it. Hold the values in a list instead."#,
        failing: &[(
            r#"main.stn"#,
            r#"rows = [(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)]
print(rows.len())
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"row = []
for i in range(33):
    row.append(i)
rows = [row]
print(rows.len())
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::SelfParam,
        id: "E0040",
        title: r#"`self` or `Self` where it does not belong"#,
        text: r#"`self` is the first parameter of a method and takes no type, since it is the value the method
is called on. `Self` names the type of a trait or an `impl` block, so it means nothing outside
one."#,
        failing: &[(
            r#"main.stn"#,
            r#"struct Counter:
    n: int

impl Counter:
    fn get(self: Counter) -> int:
        ret self.n

print(Counter(n=1).get())
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"struct Counter:
    n: int

impl Counter:
    fn get(self) -> int:
        ret self.n

print(Counter(n=1).get())
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::TestGlobal,
        id: "E0041",
        title: r#"a test that reads a global"#,
        text: r#"`stone test` runs each test without the file's top-level code, so a global a test reads,
directly or through the functions it calls, would have no value. Pass the value as an
argument instead."#,
        failing: &[(
            r#"main.stn"#,
            r#"LIMIT = 10

fn clamp(n: int) -> int:
    ret n.min(LIMIT)

test "clamp keeps small numbers":
    assert clamp(3) == 3

print(clamp(30))
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"fn clamp(n: int, limit: int) -> int:
    ret n.min(limit)

test "clamp keeps small numbers":
    assert clamp(3, 10) == 3

print(clamp(30, 10))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::ChainedIn,
        id: "E0042",
        title: r#"`in` chained with another comparison"#,
        text: r#"`in` and `not in` never chain with other comparisons, since `a in b == c` reads alike as two
different tests. Write each test on its own, joined by `and` if needed."#,
        failing: &[(
            r#"main.stn"#,
            r#"xs = [1, 2, 3]
print(1 in xs == true)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"xs = [1, 2, 3]
print(1 in xs)
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::FormatSpec,
        id: "E0043",
        title: r#"a format spec that does not fit its value"#,
        text: r#"A format spec after `:` in a string's value, as in `{x:.2f}`, follows Python's mini-language,
and its type must fit the value: `f` and `e` take numbers, `d` and `x` take ints, and `s`
takes a string. Values inside a spec are not supported yet."#,
        failing: &[(
            r#"main.stn"#,
            r#"name = "ada"
print("{name:.2f}")
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"name = "ada"
print("{name:>8}")
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::UnknownModule,
        id: "E0044",
        title: r#"a module that does not exist"#,
        text: r#"`use a.b` loads the module in `a/b.stn` under the program's root, the directory of its entry
file, or a builtin module such as `math`, or a dependency named in `stone.toml`. No file or
builtin module has that name."#,
        failing: &[(
            r#"main.stn"#,
            r#"use geometry

print(geometry.area(1.0))
"#,
        )],
        fixed: &[
            (
                r#"main.stn"#,
                r#"use geometry

print(geometry.area(1.0))
"#,
            ),
            (
                r#"geometry.stn"#,
                r#"pub fn area(r: float) -> float:
    ret 3.14 * r * r
"#,
            ),
        ],
        target: None,
    },
    Explanation {
        code: Code::Private,
        id: "E0045",
        title: r#"a name that is private to its module"#,
        text: r#"A module's functions, types, and traits are private to it unless defined with `pub`, so
another file cannot use them. Write `pub` before the definition to share it."#,
        failing: &[
            (
                r#"main.stn"#,
                r#"use geometry

print(geometry.area(1.0))
"#,
            ),
            (
                r#"geometry.stn"#,
                r#"fn area(r: float) -> float:
    ret 3.14 * r * r
"#,
            ),
        ],
        fixed: &[
            (
                r#"main.stn"#,
                r#"use geometry

print(geometry.area(1.0))
"#,
            ),
            (
                r#"geometry.stn"#,
                r#"pub fn area(r: float) -> float:
    ret 3.14 * r * r
"#,
            ),
        ],
        target: None,
    },
    Explanation {
        code: Code::Unsupported,
        id: "E0046",
        title: r#"a feature the target does not support yet"#,
        text: r#"Some functions are not supported on every target yet: Windows has no Unix sockets, so
`net.connect_unix` is an error in a build for Windows, and when running or checking on
Windows. Use TCP through `net.connect` instead."#,
        failing: &[(
            r#"main.stn"#,
            r#"use net

result = net.connect_unix("/tmp/demo.sock")
print(result.is_ok())
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"use net

result = net.connect("localhost", 8080)
print(result.is_ok())
"#,
        )],
        target: Some("x86_64-windows"),
    },
    Explanation {
        code: Code::Manifest,
        id: "E0047",
        title: r#"a mistake in `stone.toml` or `stone.lock`"#,
        text: r#"A program's `stone.toml` gives its package's name and version under `[package]` and its
dependencies under `[dependencies]`, and `stone.lock`, which stone writes, records what each
dependency held. The error points at the key or value that is not valid."#,
        failing: &[
            (
                r#"stone.toml"#,
                r#"[package]
name = "demo"
verison = "0.1.0"
"#,
            ),
            (
                r#"main.stn"#,
                r#"print("hello")
"#,
            ),
        ],
        fixed: &[
            (
                r#"stone.toml"#,
                r#"[package]
name = "demo"
version = "0.1.0"
"#,
            ),
            (
                r#"main.stn"#,
                r#"print("hello")
"#,
            ),
        ],
        target: None,
    },
    Explanation {
        code: Code::Dependency,
        id: "E0048",
        title: r#"a dependency that cannot be used"#,
        text: r#"A dependency named in `stone.toml` cannot be used: its directory or download is missing, its
own `stone.toml` has errors, its release says it is another version, or its files no longer
match `stone.lock`. Run `stone fetch` to download what is missing."#,
        failing: &[
            (
                r#"stone.toml"#,
                r#"[package]
name = "demo"
version = "0.1.0"

[dependencies]
geo = { path = "vendor/geo" }
"#,
            ),
            (
                r#"main.stn"#,
                r#"use geo

print(geo.area(1.0))
"#,
            ),
        ],
        fixed: &[
            (
                r#"stone.toml"#,
                r#"[package]
name = "demo"
version = "0.1.0"

[dependencies]
geo = { path = "vendor/geo" }
"#,
            ),
            (
                r#"main.stn"#,
                r#"use geo

print(geo.area(1.0))
"#,
            ),
            (
                r#"vendor/geo/stone.toml"#,
                r#"[package]
name = "geo"
version = "0.1.0"
"#,
            ),
            (
                r#"vendor/geo/lib.stn"#,
                r#"pub fn area(r: float) -> float:
    ret 3.14 * r * r
"#,
            ),
        ],
        target: None,
    },
    Explanation {
        code: Code::TraitValue,
        id: "E0049",
        title: r#"a value that cannot be a trait value"#,
        text: r#"A trait's name is a type of values of any type that implements the trait, as `list[Shape]`
holds circles and squares alike, each called through a table of its type's methods. That table
can only call a method whose types are the same for every type: one that takes `self` first,
never takes or returns another `Self`, and has no type parameters of its own, of a trait that has
none either. A trait such as `eq`, whose `eq` takes another `Self`, can be a bound, as in
`fn f[T: eq](a: T, b: T)`, but not a type of values.

A trait value also holds at most 32 values, counting each field of a struct, and cannot hold a
socket, a listener, or a function yet."#,
        failing: &[(
            r#"main.stn"#,
            r#"trait Grows:
    fn bigger(self, other: Self) -> bool

struct Plant:
    height: float

impl Grows for Plant:
    fn bigger(self, other: Self) -> bool:
        ret self.height > other.height

fn tallest(xs: list[Grows]) -> int:
    ret xs.len()

print(tallest([Plant(height=1.0)]))
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"trait Grows:
    fn bigger(self, other: Self) -> bool

struct Plant:
    height: float

impl Grows for Plant:
    fn bigger(self, other: Self) -> bool:
        ret self.height > other.height

fn tallest[T: Grows](xs: list[T]) -> int:
    ret xs.len()

print(tallest([Plant(height=1.0)]))
"#,
        )],
        target: None,
    },
    Explanation {
        code: Code::Constant,
        id: "E0050",
        title: r#"a constant that is not one"#,
        text: r#"A constant's value is worked out when the program is built, so it is made only of literals
(numbers, `true` and `false`, strings without values in braces, and bytes), other constants,
`math.pi`, `math.tau`, and `math.e`, the operators, and tuples of them. A call, a variable, a
list, or a map is not known until the program runs; keep such a value in a function, or in a
variable at the top level of the entry file. An operation that would fail when the program runs,
such as a division by zero, fails the build instead.

A constant is never assigned again, and no variable or parameter takes its name, so its name
always means its value."#,
        failing: &[(
            r#"main.stn"#,
            r#"fn kilobytes(n: int) -> int:
    ret n * 1024

const buffer = kilobytes(64)

print(buffer)
"#,
        )],
        fixed: &[(
            r#"main.stn"#,
            r#"const buffer = 64 * 1024

print(buffer)
"#,
        )],
        target: None,
    },
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn codes_are_numbered_in_order() {
        assert_eq!(EXPLANATIONS.len(), Code::ALL.len());
        for (i, code) in Code::ALL.iter().enumerate() {
            assert_eq!(*code as usize, i, "{code:?} is out of order");
            let explanation = code.explanation();
            assert_eq!(explanation.code, *code);
            assert_eq!(explanation.id, format!("E{:04}", i + 1));
        }
    }

    #[test]
    fn a_code_is_found_however_it_is_written() {
        assert_eq!(find("E0005"), Some(Code::Mismatch));
        assert_eq!(find("e0005"), Some(Code::Mismatch));
        assert_eq!(find("5"), Some(Code::Mismatch));
        assert_eq!(find("E0000"), None);
        assert_eq!(find("E9999"), None);
        assert_eq!(find("E"), None);
        assert_eq!(find("Ex5"), None);
    }

    /// Lists the `.rs` files under `dir`, in subdirectories too.
    fn rust_files(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
        for entry in std::fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                rust_files(&path, out);
            } else if path.extension().is_some_and(|ext| ext == "rs") {
                out.push(path);
            }
        }
    }

    #[test]
    fn every_error_before_running_is_made_with_a_code() {
        // runtime errors have no code, and a formatting bug is stone's error, not the program's
        const UNCODED: &[&str] = &["diagnostic.rs", "explain.rs", "interpreter.rs", "format.rs"];
        let mut files = vec![];
        rust_files(
            &std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src"),
            &mut files,
        );
        for path in files {
            let name = path.file_name().unwrap().to_string_lossy();
            if UNCODED.contains(&name.as_ref()) || name == "tests.rs" {
                continue;
            }
            let source = std::fs::read_to_string(&path).unwrap();
            let code = source.split("#[cfg(test)]").next().unwrap_or_default();
            for (i, line) in code.lines().enumerate() {
                let line_code = line.split("//").next().unwrap_or_default();
                assert!(
                    !line_code.contains("Diagnostic::error("),
                    "{}:{}: make this error with a code, as in `Code::Mismatch.error(span, \
                     message)`",
                    path.display(),
                    i + 1
                );
            }
        }
    }

    #[test]
    fn a_coded_error_renders_its_code() {
        let span = Span::new(crate::span::Pos::new(1, 1), crate::span::Pos::new(1, 2));
        let rendered = Code::Mismatch
            .error(span, "expected int, found str")
            .render("main.stn", "x\n");
        assert!(
            rendered.starts_with("main.stn:1:1: error[E0005]: expected int, found str\n"),
            "{rendered}"
        );
    }
}

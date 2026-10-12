"""A Pygments lexer for stone, so ```stone code blocks are highlighted.

The keywords, builtins, and methods mirror RESERVED_KEYWORDS in src/token.rs and BUILTINS and
METHODS in src/stdlib.rs.
"""

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import (
    Comment,
    Keyword,
    Name,
    Number,
    Operator,
    Punctuation,
    String,
    Text,
    Whitespace,
)

KEYWORDS = (
    "and", "as", "break", "cont", "elif", "else", "fn", "for", "if", "in", "not", "or", "pub", "ret",
    "use", "while",
)
CONSTANTS = ("true", "false", "none")
BUILTINS = ("print", "range", "int", "float", "str", "input", "eof", "args")
METHODS = (
    "len",
    "append",
    "pop",
    "insert",
    "extend",
    "strip",
    "split",
    "join",
    "find",
    "startswith",
    "endswith",
)


class StoneLexer(RegexLexer):
    """Highlights stone source, such as `fn f(a: int) -> int; ret a + 1`."""

    name = "stone"
    aliases = ["stone", "stn"]
    filenames = ["*.stn"]

    tokens = {
        "string": [
            (r'"', String.Double, "#pop"),
            (r"\\(x[0-9a-fA-F]{2}|.)", String.Escape),
            (r'[^"\\\n]+', String.Double),
            # a string that never closes ends at the line break
            (r"\n", Whitespace, "#pop"),
        ],
        "root": [
            (r"\s+", Whitespace),
            (r"//.*$", Comment.Single),
            (r'"', String.Double, "string"),
            (r"(fn)(\s+)([A-Za-z_]\w*)", bygroups(Keyword, Whitespace, Name.Function)),
            (r"(use)(\s+)([A-Za-z_][\w.]*)", bygroups(Keyword.Namespace, Whitespace, Name.Namespace)),
            (
                # a type name, maybe inside list[ or tuple[, then closing brackets or more types
                r"(->|:)(\s*)((?:(?:list|tuple)\[)*(?:int|float|bool|str|none)(?:\]|,\s*(?:(?:list|tuple)\[)*(?:int|float|bool|str|none))*)(?![\w(])",
                bygroups(Punctuation, Whitespace, Keyword.Type),
            ),
            (words(KEYWORDS, suffix=r"\b"), Keyword),
            (words(CONSTANTS, suffix=r"\b"), Keyword.Constant),
            (words(BUILTINS, suffix=r"\b"), Name.Builtin),
            (words(METHODS, prefix=r"(?<=\.)", suffix=r"\b"), Name.Builtin),
            (r"\d+\.\d*([eE][+-]?\d+)?|\d+[eE][+-]?\d+", Number.Float),
            (r"\d+", Number.Integer),
            (r"\*\*=|[-+*/%]=|==|!=|<=|>=|\*\*|[-+*/%<>=]", Operator),
            (r"[()\[\],;:.]", Punctuation),
            (r"[A-Za-z_]\w*", Name),
            (r".", Text),
        ],
    }


def setup(app):
    app.add_lexer("stone", StoneLexer)
    return {"parallel_read_safe": True, "parallel_write_safe": True}

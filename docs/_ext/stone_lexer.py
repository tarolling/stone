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

KEYWORDS = ("and", "break", "cont", "def", "elif", "else", "for", "if", "in", "not", "or", "ret", "while")
CONSTANTS = ("true", "false", "none")
BUILTINS = ("print", "range", "int", "float")
METHODS = ("len", "append")


class StoneLexer(RegexLexer):
    """Highlights stone source, such as `def f(a); ret a + 1`."""

    name = "stone"
    aliases = ["stone", "st"]
    filenames = ["*.st"]

    tokens = {
        "root": [
            (r"\s+", Whitespace),
            (r"#.*$", Comment.Single),
            (r'"[^"\n]*"', String.Double),
            (r"(def)(\s+)([A-Za-z_]\w*)", bygroups(Keyword, Whitespace, Name.Function)),
            (words(KEYWORDS, suffix=r"\b"), Keyword),
            (words(CONSTANTS, suffix=r"\b"), Keyword.Constant),
            (words(BUILTINS, suffix=r"\b"), Name.Builtin),
            (words(METHODS, prefix=r"(?<=\.)", suffix=r"\b"), Name.Builtin),
            (r"\d+\.\d*([eE][+-]?\d+)?|\d+[eE][+-]?\d+", Number.Float),
            (r"\d+", Number.Integer),
            (r"==|!=|<=|>=|\*\*|[-+*/%<>=]", Operator),
            (r"[()\[\],;.]", Punctuation),
            (r"[A-Za-z_]\w*", Name),
            (r".", Text),
        ],
    }


def setup(app):
    app.add_lexer("stone", StoneLexer)
    return {"parallel_read_safe": True, "parallel_write_safe": True}

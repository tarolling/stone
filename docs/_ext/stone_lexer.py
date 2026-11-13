"""A Pygments lexer for stone, so ```stone code blocks are highlighted.

The keywords, builtins, and methods mirror RESERVED_KEYWORDS in src/token.rs and BUILTINS and
METHODS in src/stdlib.rs.
"""

from pygments.lexer import RegexLexer, bygroups, include, words
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
    "and",
    "as",
    "assert",
    "break",
    "case",
    "cont",
    "elif",
    "else",
    "enum",
    "fn",
    "for",
    "if",
    "impl",
    "in",
    "match",
    "not",
    "or",
    "pub",
    "ret",
    "struct",
    "trait",
    "use",
    "while",
)
CONSTANTS = ("true", "false", "none")
BUILTINS = (
    "print",
    "eprint",
    "range",
    "int",
    "float",
    "str",
    "input",
    "eof",
    "args",
    "some",
    "less",
    "equal",
    "greater",
    "ok",
    "err",
)
METHODS = (
    "len",
    "append",
    "pop",
    "insert",
    "extend",
    "sort",
    "sort_by_key",
    "map",
    "filter",
    "any",
    "all",
    "min_by_key",
    "max_by_key",
    "strip",
    "split",
    "splitlines",
    "join",
    "find",
    "rfind",
    "count",
    "replace",
    "lower",
    "upper",
    "startswith",
    "endswith",
    "is_some",
    "is_none",
    "unwrap",
    "unwrap_or",
    "is_ok",
    "is_err",
    "unwrap_err",
    "contains",
    "get",
    "keys",
    "values",
    "items",
    "add",
    "remove",
    "union",
    "intersection",
    "difference",
    "is_subset",
    "is_superset",
    "is_disjoint",
    "eq",
    "cmp",
    "min",
    "max",
    "clamp",
    "abs",
    "floor",
    "display",
    "send",
    "read_line",
    "read",
    "read_all",
    "accept",
    "port",
    "close",
    "bytes",
    "text",
    "set_timeout",
    "send_bytes",
    "read_bytes",
    "read_exact",
    "read_all_bytes",
)


class StoneLexer(RegexLexer):
    """Highlights stone source, such as `fn f(a: int) -> int: ret a + 1`."""

    name = "stone"
    aliases = ["stone", "stn"]
    filenames = ["*.stn"]

    tokens = {
        "string": [
            (r'"', String.Double, "#pop"),
            (r"\\(x[0-9a-fA-F]{2}|.)", String.Escape),
            (r"\{\{|\}\}", String.Escape),
            # a value written into the string, as `{x}` in `"x is {x}"`
            (r"\{", String.Interpol, "value"),
            (r'[^"\\\n{}]+', String.Double),
            (r"\}", String.Double),
            # a string that never closes ends at the line break
            (r"\n", Whitespace, "#pop"),
        ],
        # a bytes literal, which has escapes but no values
        "bytes": [
            (r'"', String.Double, "#pop"),
            (r"\\(x[0-9a-fA-F]{2}|.)", String.Escape),
            (r'[^"\\\n]+', String.Double),
            (r"\n", Whitespace, "#pop"),
        ],
        "value": [
            (r"\}", String.Interpol, "#pop"),
            # a format spec, as `:.2f` in `"{x:.2f}"`, which runs to the value's end
            (r':[^{}\n"|]*(?=\})', String.Interpol),
            (r"\{", Punctuation, "braces"),
            (r"\n", Whitespace, "#pop:2"),
            include("root"),
        ],
        # braces inside a value, as a map's, which a `}` closes before it ends the value
        "braces": [
            (r"\}", Punctuation, "#pop"),
            (r"\{", Punctuation, "braces"),
            (r"\n", Whitespace, "#pop:3"),
            include("root"),
        ],
        "root": [
            (r"\s+", Whitespace),
            (r"//.*$", Comment.Single),
            # `test` is a keyword only where a test block opens, so `fn test()` stays a name
            (r'^test(?=[ \t]+")', Keyword),
            (r'\bb"', String.Double, "bytes"),
            (r'"', String.Double, "string"),
            (r"(fn)(\s+)([A-Za-z_]\w*)", bygroups(Keyword, Whitespace, Name.Function)),
            (
                r"(enum|struct|impl|trait)(\s+)([A-Za-z_]\w*)",
                bygroups(Keyword, Whitespace, Name.Class),
            ),
            (
                r"(use)(\s+)([A-Za-z_][\w.]*)",
                bygroups(Keyword.Namespace, Whitespace, Name.Namespace),
            ),
            (
                # a type name, maybe inside list[, map[, set[, tuple[, option[, or result[, then closing
                # brackets or more types
                r"(->|:)(\s*)((?:(?:list|map|set|tuple|option|result)\[)*(?:int|float|bool|str|bytes|socket|listener)(?:\]|,\s*(?:(?:list|map|set|tuple|option|result)\[)*(?:int|float|bool|str|bytes|socket|listener))*)(?![\w(])",
                bygroups(Punctuation, Whitespace, Keyword.Type),
            ),
            (r"\b(self|Self)\b", Name.Builtin.Pseudo),
            (words(KEYWORDS, suffix=r"\b"), Keyword),
            (words(CONSTANTS, suffix=r"\b"), Keyword.Constant),
            (words(BUILTINS, suffix=r"\b"), Name.Builtin),
            (words(METHODS, prefix=r"(?<=\.)", suffix=r"\b"), Name.Builtin),
            (r"0[xX][0-9A-Fa-f_]+", Number.Hex),
            (r"0[oO][0-7_]+", Number.Oct),
            (r"0[bB][01_]+", Number.Bin),
            (r"\d[\d_]*\.\d[\d_]*([eE][+-]?\d[\d_]*)?|\d[\d_]*[eE][+-]?\d[\d_]*", Number.Float),
            (r"\d[\d_]*", Number.Integer),
            (
                r"\*\*=|<<=|>>=|[-+*/%&|^]=|==|!=|<=|>=|\*\*|<<|>>|[-+*/%<>=?&^~]",
                Operator,
            ),
            (r"[()\[\]{},:.|]", Punctuation),
            (r"[A-Za-z_]\w*", Name),
            (r".", Text),
        ],
    }


def setup(app):
    app.add_lexer("stone", StoneLexer)
    return {"parallel_read_safe": True, "parallel_write_safe": True}

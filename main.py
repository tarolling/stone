VERBOSE = False
MAX_INDENT = 100
MAX_LEVEL = 200
TABSIZE = 8

tokens = [
    "ENDMARKER",
    "NAME",
    "NUMBER",
    "STRING",
    "NEWLINE",
    "INDENT",
    "DEDENT",
    "LPAR",
    "RPAR",
    "LSQB",
    "RSQB",
    "COLON",
    "COMMA",
    "SEMI",
    "PLUS",
    "MINUS",
]


# Operators and symbols
token_STAR = "*"
token_SLASH = "/"
token_VBAR = "|"
token_AMPER = "&"
token_LESS = "<"
token_GREATER = ">"
token_EQUAL = "="
token_DOT = "."
token_PERCENT = "%"
token_LBRACE = "{"
token_RBRACE = "}"
token_EQEQUAL = "=="
token_NOTEQUAL = "!="
token_LESSEQUAL = "<="
token_GREATEREQUAL = ">="
token_TILDE = "~"
token_CIRCUMFLEX = "^"
token_LEFTSHIFT = "<<"
token_RIGHTSHIFT = ">>"
token_EXCLAMATION = "!"

# Keywords
token_DEF = "def"
token_RETURN = "return"
token_IF = "if"
token_ELIF = "elif"
token_ELSE = "else"
token_WHILE = "while"
token_FOR = "for"
token_BREAK = "break"
token_CONTINUE = "continue"
token_PASS = "pass"
token_CLASS = "class"

# More keywords
keyword_def = "DEF"
keyword_return = "RETURN"
keyword_if = "IF"
keyword_elif = "ELIF"
keyword_else = "ELSE"
keyword_while = "WHILE"
keyword_for = "FOR"
keyword_break = "BREAK"
keyword_continue = "CONTINUE"
keyword_pass = "PASS"
keyword_class = "CLASS"

# lookup for one-char tokens
one_char_tokens = {
    "!": "EXCLAMATION",
    "%": "PERCENT",
    "&": "AMPER",
    "(": "LPAR",
    ")": "RPAR",
    "*": "STAR",
    "+": "PLUS",
    ",": "COMMA",
    "-": "MINUS",
    ".": "DOT",
    "/": "SLASH",
    ":": "COLON",
    ";": "SEMI",
    "<": "LESS",
    "=": "EQUAL",
    ">": "GREATER",
    "@": "AT",
    "[": "LSQB",
    "]": "RSQB",
    "^": "CIRCUMFLEX",
    "{": "LBRACE",
    "|": "VBAR",
    "}": "RBRACE",
    "~": "TILDE",
}

two_char_tokens = {
    "!=": "NOTEQUAL",
    "%=": "PERCENTEQUAL",
    "&=": "AMPEREQUAL",
    "**": "DOUBLESTAR",
    "*=": "STAREQUAL",
    "+=": "PLUSEQUAL",
    "-=": "MINEQUAL",
    "->": "RARROW",
    "//": "DOUBLESLASH",
    "/=": "SLASHEQUAL",
    ":=": "COLONEQUAL",
    "<<": "LEFTSHIFT",
    "<=": "LESSEQUAL",
    "==": "EQEQUAL",
    ">=": "GREATEREQUAL",
    ">>": "RIGHTSHIFT",
    "@=": "ATEQUAL",
    "^=": "CIRCUMFLEXEQUAL",
    "|=": "VBAREQUAL",
}

###############################################################################
#                                   HELPERS                                   #
###############################################################################


def is_potential_identifier_start(c):
    return (c >= "a" and c <= "z") or (c >= "A" and c <= "Z") or (c == "_")


def is_potential_identifier_char(c):
    return (
        (c >= "a" and c <= "z")
        or (c >= "A" and c <= "Z")
        or (c >= "0" and c <= "9")
        or (c == "_")
    )


###############################################################################
#                                SYSTEM FUNCS                                 #
###############################################################################


def isdigit(c):
    return c >= "0" and c <= "9"


###############################################################################
#                                PREPROCESSOR                                 #
###############################################################################


# def preprocess(source_code):
#     """
#     Remove empty lines and whitespace-only lines without using string methods.
#     Preserves indentation and line endings.
#     """
#     result = []
#     line_start = 0
#     length = len(source_code)
#     i = 0

#     while i < length:
#         # Find the next newline or end of string
#         while i < length and source_code[i] != "\n":
#             i += 1

#         # Extract the current line (including newline if present)
#         line = (
#             source_code[line_start:i]
#             if i >= length
#             else source_code[line_start : i + 1]
#         )

#         # Check if line is non-empty
#         is_empty = True
#         for char in line:
#             if char not in (" ", "\t", "\r", "\n"):
#                 is_empty = False
#                 break

#         if not is_empty:
#             # Remove trailing whitespace (but preserve newline if present)
#             end = len(line)
#             has_newline = end > 0 and line[end - 1] == "\n"

#             # Find last non-whitespace character
#             last_char = end - (2 if has_newline else 1)
#             while last_char >= 0 and line[last_char] in (" ", "\t", "\r"):
#                 last_char -= 1

#             # Rebuild the line
#             if has_newline:
#                 cleaned = line[: last_char + 1] + "\n"
#             else:
#                 cleaned = line[: last_char + 1]

#             result.append(cleaned)

#         # Move to next line
#         i += 1
#         line_start = i

#     return "".join(result)


###############################################################################
#                                    LEXER                                    #
###############################################################################

EOF = -2


def Token(token_type, value, line, column):
    return {
        "type": token_type,
        "value": value,
        "line": line,
        "column": column,
    }


def Token__repr__(token):
    return (
        "Token("
        + token["type"]
        + ", "
        + token["value"]
        + "line="
        + token["line"]
        + ", col="
        + token["column"]
        + ")"
    )


Lexer_source = ""
Lexer_position = 0
Lexer_line_num = 1
Lexer_col_offset = -1
Lexer_cur_indent_idx = 0
Lexer_indent_stack = [0]
Lexer_cur_paren_lvl = 0
Lexer_paren_stack = [0]
Lexer_at_bol = True
Lexer_tabsize = TABSIZE
Lexer_pendin = 0
Lexer_tok_start = 0
Lexer_tok_cur = 0
Lexer_comment_newline = False


def Lexer(source_code):
    global Lexer_source
    Lexer_source = source_code


def Lexer_peek():
    """Look at the current character without advancing position"""
    global Lexer_source, Lexer_position
    if Lexer_position >= len(Lexer_source):
        return EOF
    return Lexer_source[Lexer_position]


def Lexer_advance():
    """Get the current character and advance position"""
    global Lexer_source, Lexer_position, Lexer_line_num, Lexer_col_offset
    if Lexer_position >= len(Lexer_source):
        return EOF
    Lexer_col_offset += 1
    char = Lexer_source[Lexer_position]
    Lexer_position += 1
    return char


def Lexer_skip_to_eol():
    while Lexer_peek() != "\n" and Lexer_peek() != "":
        Lexer_advance()


def Lexer_tokenize_string():
    """Tokenize a string literal"""
    global Lexer_line_num, Lexer_col_offset

    start_col = Lexer_col_offset
    quote_char = Lexer_advance()  # Skip the opening quote
    string_value = ""

    while Lexer_peek() and Lexer_peek() != quote_char:
        char = Lexer_peek()
        if char == "\\":
            Lexer_advance()
            next_char = Lexer_peek()

            if not next_char:
                raise SyntaxError(f"Unterminated string at line {Lexer_line_num}")

            if next_char == "n":
                string_value += "\n"
            elif next_char == "t":
                string_value += "\t"
            elif next_char == "r":
                string_value += "\r"
            elif next_char == "\\":
                string_value += "\\"
            else:
                string_value += "\\" + next_char
            Lexer_advance()
        else:
            string_value += Lexer_advance()

    if not Lexer_peek() or Lexer_peek() != quote_char:
        print(f"Unterminated string at line {Lexer_line_num}")
        return

    Lexer_advance()  # Skip the closing quote
    return Token("STRING", string_value, Lexer_line_num, start_col)


def tok_get_normal_mode():
    """Get the next token in normal parsing mode"""
    global Lexer_source, Lexer_position, Lexer_line_num, Lexer_col_offset
    global Lexer_cur_indent_idx, Lexer_indent_stack, Lexer_cur_paren_lvl
    global Lexer_paren_stack, Lexer_at_bol, Lexer_tabsize, Lexer_pendin
    global Lexer_tok_start, Lexer_tok_cur, Lexer_comment_newline

    blankline = False

    # get indentation level
    if Lexer_at_bol:
        Lexer_at_bol = False
        col = 0
        cont_line_col = 0

        while True:
            c = Lexer_peek()
            if c == " ":
                col += 1
            elif c == "\t":
                col = (col / Lexer_tabsize + 1) * Lexer_tabsize
            else:
                break
            Lexer_advance()

        # check for blank lines
        c = Lexer_peek()
        blankline = False
        if c == "#" or c == "\n" or c == "\r":
            if col == 0 and c == "\n":
                blankline = False
            else:
                blankline = True

        # handle indentation changes
        if not blankline and Lexer_cur_paren_lvl == 0:
            if cont_line_col != 0:
                col = cont_line_col
            if col == Lexer_indent_stack[Lexer_cur_indent_idx]:
                pass  # no change
            elif col > Lexer_indent_stack[Lexer_cur_indent_idx]:
                if Lexer_cur_indent_idx + 1 >= MAX_INDENT:
                    return -1  # too deep
                Lexer_pendin += 1
                Lexer_cur_indent_idx += 1
                Lexer_indent_stack.append(col)
            else:  # col < tok->indstack[tok->indent]
                while (
                    Lexer_cur_indent_idx > 0
                    and col < Lexer_indent_stack[Lexer_cur_indent_idx]
                ):
                    Lexer_pendin -= 1
                    Lexer_cur_indent_idx -= 1
                if col != Lexer_indent_stack[Lexer_cur_indent_idx]:
                    return -1

    # handle pending indents/dedents
    if Lexer_pendin != 0:
        if Lexer_pendin < 0:
            Lexer_pendin += 1
            return Token("DEDENT", "", Lexer_line_num, Lexer_col_offset)
        else:
            Lexer_pendin -= 1
            return Token("INDENT", "", Lexer_line_num, Lexer_col_offset)

    # skip whitespace
    while Lexer_peek() == " " or Lexer_peek() == "\t" or Lexer_peek() == "\r":
        Lexer_advance()

    # get current char
    c = Lexer_peek()
    # EOF
    if c == EOF:
        return Token("ENDMARKER", "", Lexer_line_num, Lexer_col_offset)

    # Comments
    if c == "#":
        Lexer_skip_to_eol()
        return tok_get_normal_mode()  # potential issue if tens of thousands of comments

    # Identifiers
    if is_potential_identifier_start(c):
        start_pos = Lexer_position
        start_col = Lexer_col_offset
        while is_potential_identifier_char(c):
            Lexer_advance()
            c = Lexer_peek()

        identifier = Lexer_source[start_pos:Lexer_position]

        if identifier == "def":
            token_type = keyword_def
        elif identifier == "return":
            token_type = keyword_return
        elif identifier == "if":
            token_type = keyword_if
        elif identifier == "elif":
            token_type = keyword_elif
        elif identifier == "else":
            token_type = keyword_else
        elif identifier == "while":
            token_type = keyword_while
        elif identifier == "for":
            token_type = keyword_for
        elif identifier == "break":
            token_type = keyword_break
        elif identifier == "continue":
            token_type = keyword_continue
        elif identifier == "pass":
            token_type = keyword_pass
        elif identifier == "class":
            token_type = keyword_class
        else:
            token_type = "NAME"

        return Token(token_type, identifier, Lexer_line_num, start_col)

    # Newlines
    if c == "\n":
        Lexer_advance()
        Lexer_at_bol = True
        Lexer_line_num += 1
        return Token("NEWLINE", "", Lexer_line_num, Lexer_col_offset)

    # number
    if isdigit(c) or (
        c == "."
        and Lexer_position + 1 < len(Lexer_source)
        and isdigit(Lexer_source[Lexer_position + 1])
    ):
        start_col = Lexer_col_offset
        number = ""

        if c == ".":
            number += Lexer_advance()
            c = Lexer_peek()

        while isdigit(c):
            number += Lexer_advance()
            c = Lexer_peek()

        # Handle decimal point
        if c == ".":
            number += Lexer_advance()
            c = Lexer_peek()
            while isdigit(c):
                number += Lexer_advance()
                c = Lexer_peek()

        return Token("NUMBER", number, Lexer_line_num, start_col)

    # string literals (simple quotes, ignoring f-strings for now)
    if c == '"' or c == "'":
        return Lexer_tokenize_string()

    # punctuation character
    if c in one_char_tokens or c in two_char_tokens:
        start_col = Lexer_col_offset
        first_char = c

        # Check for two-character tokens
        if Lexer_position < len(Lexer_source):
            second_char = Lexer_advance()
            two_char = first_char + second_char
            if two_char in two_char_tokens:
                return Token(
                    two_char_tokens[two_char], two_char, Lexer_line_num, start_col
                )

        # Single character token
        if first_char in one_char_tokens:
            return Token(
                one_char_tokens[first_char], first_char, Lexer_line_num, start_col
            )

    # if we get here, it's an unrecognized character
    return Token(
        "ERROR", f"Unexpected character '{c}'", Lexer_line_num, Lexer_col_offset
    )


def Lexer_tokenize():
    """Convert source code to a list of tokens"""
    global Lexer_source, Lexer_position, Lexer_line_num, Lexer_col_offset, Lexer_indent_stack

    global Lexer_position, Lexer_line_num, Lexer_col_offset
    global Lexer_indent_stack, Lexer_cur_indent_idx

    tokens = []
    # Lexer_position = 0
    # Lexer_line_num = 1
    # Lexer_col_offset = 0
    # Lexer_indent_stack = [0]
    # Lexer_cur_indent_idx = 0

    while True:
        token = tok_get_normal_mode()
        if token["type"] == "ENDMARKER":
            # add any pending DEDENTs
            while len(Lexer_indent_stack) > 1:
                Lexer_indent_stack.pop()
                tokens.append(Token("DEDENT", "", Lexer_line_num, Lexer_col_offset))
            tokens.append(token)
            break
        elif token["type"] == "ERROR":
            print(f"Lexer error: {token['value']}")
            return -1
        else:
            tokens.append(token)

    return tokens


###############################################################################
#                                 PARSER                                      #
###############################################################################
ASTNodeType = {
    "PROGRAM": "Program",
    "FUNCTION_DEF": "FunctionDef",
    "BLOCK": "Block",
    "RETURN_STMT": "Return_Stmt",
    "IF_STMT": "If",
    "WHILE_STMT": "While",
    "ASSIGNMENT": "Assignment",
    "BINARY_EXPR": "Binary_Expr",
    "CALL_EXPR": "Call_Expr",
    "IDENTIFIER": "Identifier",
    "NUMBER": "Number",
    "STRING": "String",
    "DICT": "Dict",
}


class ASTNode:
    def __init__(self, node_type, **kwargs):
        self.node_type = node_type
        for key, value in kwargs.items():
            setattr(self, key, value)

    def __repr__(self):
        attrs = ", ".join(
            f"{k}={v}" for k, v in self.__dict__.items() if k != "node_type"
        )
        return f"{self.node_type}({attrs})"


Parser_tokens = []
Parser_position = 0


def Parser(tokens):
    global Parser_tokens, Parser_position
    Parser_tokens = tokens
    Parser_position = 0


def Parser_peek():
    """Look at the current token without advancing position"""
    global Parser_tokens, Parser_position
    if Parser_position >= len(Parser_tokens):
        return Token(EOF, "")
    return Parser_tokens[Parser_position]


def Parser_advance():
    """Get the current token and advance position"""
    global Parser_position
    token = Parser_peek()
    Parser_position += 1
    return token


def Parser_expect(token_type):
    """Expect a specific token type, advance and return it"""
    token = Parser_peek()
    if token["type"] != token_type:
        raise SyntaxError(
            f"Expected {token_type}, got {token['type']} at line {token['line']}"
        )
    return Parser_advance()


def Parser_parse_file():
    """Parse the entire file"""
    statements = []

    if Parser_parse_statements(statements) != -1 and Parser_expect("EOF") != -1:
        return ASTNode(ASTNodeType["FILE"], statements=statements)
    return -1


def Parser_parse_statements(statements):
    pass


def parse_expressions():
    pass


def parse_expression():
    pass


def parse_atom():
    # if Parser_peek()["type"] == "IDENTIFIER":
    pass


def Parser_parse_statement():
    """Parse a statement"""
    global Parser_position

    token = Parser_peek()

    if token["type"] == "DEF":
        return Parser_parse_function_def()
    elif token["type"] == "RETURN":
        return Parser_parse_return_statement()
    elif token["type"] == "IF":
        return Parser_parse_if_statement()
    elif token["type"] == "WHILE":
        return Parser_parse_while_statement()
    elif token["type"] == "PASS" or token["type"] == "NEWLINE":
        Parser_advance()
        return ASTNode(ASTNodeType["BLOCK"], name="pass")
    elif token["type"] == "IDENTIFIER":
        # This could be an assignment or a function call
        identifier = Parser_advance()["value"]

        if Parser_peek()["type"] == "EQUAL":
            Parser_advance()  # Consume the '='
            value = Parser_parse_expression()
            return ASTNode(ASTNodeType["ASSIGNMENT"], name=identifier, value=value)
        else:
            # Put the identifier back and parse as expression
            Parser_position -= 1
            expr = Parser_parse_expression()
            return expr
    else:
        print(f"Unexpected token {token['type']} at line {token['line']}")
        return -1


def Parser_parse_function_def():
    """Parse a function definition"""
    Parser_expect("DEF")
    name = Parser_expect("NAME")["value"]

    Parser_expect("LPAR")
    params = []

    if Parser_peek()["type"] != "RPAR":
        # Parse parameters
        params.append(Parser_expect("NAME")["value"])

        while Parser_peek()["type"] == "COMMA":
            Parser_advance()  # Consume the comma
            params.append(Parser_expect("NAME")["value"])

    Parser_expect("RPAR")
    Parser_expect("COLON")
    Parser_expect("NEWLINE")

    # Parse function body
    body = Parser_parse_block()
    if body == -1:
        return -1

    return ASTNode(ASTNodeType["FUNCTION_DEF"], name=name, params=params, body=body)


def Parser_parse_block():
    """Parse an indented block of statements"""
    Parser_expect("NEWLINE")
    Parser_expect("INDENT")
    statements = []

    while Parser_peek()["type"] != "DEDENT":
        parsed_statement = Parser_parse_statement()
        if parsed_statement == -1:
            return -1
        statements.append(parsed_statement)

    Parser_expect("DEDENT")
    return ASTNode(ASTNodeType["BLOCK"], statements=statements)


def Parser_parse_return_statement():
    """Parse a return statement"""
    Parser_expect("RETURN")

    # Check if there's an expression or just a newline
    if Parser_peek()["type"] == "NEWLINE":
        expr = None
    else:
        expr = Parser_parse_expression()

    Parser_expect("NEWLINE")
    # while Parser_peek()["type"] == token_NEWLINE:
    #     Parser_advance()
    return ASTNode(ASTNodeType["RETURN_STMT"], expression=expr)


def Parser_parse_if_statement():
    """Parse an if statement"""
    Parser_expect("IF")
    condition = Parser_parse_expression()
    Parser_expect("COLON")
    Parser_expect("NEWLINE")

    then_block = Parser_parse_block()

    # Check for else clause
    else_block = None
    if Parser_peek()["type"] == "ELSE":
        Parser_advance()  # Consume 'else'
        Parser_expect("COLON")
        Parser_expect("NEWLINE")
        else_block = Parser_parse_block()

    return ASTNode(
        ASTNodeType["IF_STMT"],
        condition=condition,
        then_block=then_block,
        else_block=else_block,
    )


def Parser_parse_while_statement():
    """Parse a while statement"""
    Parser_expect("WHILE")
    condition = Parser_parse_expression()
    Parser_expect("COLON")
    Parser_expect("NEWLINE")

    body = Parser_parse_block()

    return ASTNode(ASTNodeType["WHILE_STMT"], condition=condition, body=body)


def Parser_parse_expression():
    """Parse an expression with proper precedence"""
    return Parser_parse_or_expression()


def Parser_parse_or_expression():
    """Parse logical OR expressions (lowest precedence)"""
    left = Parser_parse_and_expression()

    # Note: OR operator not in current token set, but keeping for completeness
    while Parser_peek()["type"] in ("OR",):  # Empty for now
        operator = Parser_advance()["value"]
        right = Parser_parse_and_expression()
        left = ASTNode(
            ASTNodeType["BINARY_EXPR"], left=left, operator=operator, right=right
        )

    return left


def Parser_parse_and_expression():
    """Parse logical AND expressions"""
    left = Parser_parse_not_expression()

    # Note: AND operator not in current token set, but keeping for completeness
    while Parser_peek()["type"] in ("AND",):  # Empty for now
        operator = Parser_advance()["value"]
        right = Parser_parse_not_expression()
        left = ASTNode(
            ASTNodeType["BINARY_EXPR"], left=left, operator=operator, right=right
        )

    return left


def Parser_parse_not_expression():
    """Parse logical NOT expressions"""
    # Note: NOT operator not in current token set, but keeping for completeness
    if Parser_peek()["type"] == "NOT":
        operator = Parser_advance()["value"]
        expr = Parser_parse_not_expression()
        return ASTNode(ASTNodeType["UNARY_EXPR"], operator=operator, operand=expr)

    return Parser_parse_comparison()


def Parser_parse_comparison():
    """Parse comparison expressions (==, !=, <, >, <=, >=)"""
    left = Parser_parse_arithmetic_expression()

    while Parser_peek()["type"] in (
        "EQEQUAL",
        "NOTEQUAL",
        "LESS",
        "GREATER",
        "LESSEQUAL",
        "GREATEREQUAL",
    ):
        operator = Parser_advance()["value"]
        right = Parser_parse_arithmetic_expression()
        left = ASTNode(
            ASTNodeType["BINARY_EXPR"], left=left, operator=operator, right=right
        )

    return left


def Parser_parse_arithmetic_expression():
    """Parse arithmetic expressions (+ and -)"""
    left = Parser_parse_term()

    while Parser_peek()["type"] in ("PLUS", "MINUS"):
        operator = Parser_advance()["value"]
        right = Parser_parse_term()
        left = ASTNode(
            ASTNodeType["BINARY_EXPR"], left=left, operator=operator, right=right
        )

    return left


def Parser_parse_term():
    """Parse a term (multiplication/division)"""
    left = Parser_parse_factor()

    while Parser_peek()["type"] in ("STAR", "SLASH"):
        operator = Parser_advance()["value"]
        right = Parser_parse_factor()
        left = ASTNode(
            ASTNodeType["BINARY_EXPR"], left=left, operator=operator, right=right
        )

    return left


def Parser_parse_factor():
    """Parse a factor (unary expressions)"""
    if Parser_peek()["type"] in ("PLUS", "MINUS"):
        operator = Parser_advance()["value"]
        operand = Parser_parse_factor()
        return ASTNode(ASTNodeType["UNARY_EXPR"], operator=operator, operand=operand)

    return Parser_parse_primary()


def Parser_parse_dict():
    """Parse a dictionary definition/literal"""
    Parser_expect("LBRACE")
    items = []

    if Parser_peek()["type"] != "RBRACE":
        key = Parser_parse_expression()
        Parser_expect("COLON")
        value = Parser_parse_expression()
        items.append((key, value))

        while Parser_peek()["type"] == "COMMA":
            Parser_advance()
            key = Parser_parse_expression()
            Parser_expect("COLON")
            value = Parser_parse_expression()
            items.append((key, value))

    Parser_expect("RBRACE")
    return ASTNode(ASTNodeType["DICT"], items=items)


def Parser_parse_primary():
    """Parse a primary expression (literal, identifier, call, parenthesized)"""
    token = Parser_peek()

    if token["type"] == "NUMBER":
        Parser_advance()
        return ASTNode(ASTNodeType["NUMBER"], value=int(token["value"]))

    elif token["type"] == "STRING":
        Parser_advance()
        return ASTNode(ASTNodeType["STRING"], value=token["value"])

    elif token["type"] == "NAME":
        identifier = Parser_advance()["value"]

        # Check if this is a function call
        if Parser_peek()["type"] == "LPAR":
            Parser_advance()  # Consume '('
            args = []

            if Parser_peek()["type"] != "RPAR":
                args.append(Parser_parse_expression())

                while Parser_peek()["type"] == "COMMA":
                    Parser_advance()  # Consume ','
                    args.append(Parser_parse_expression())

            Parser_expect("RPAR")
            return ASTNode(ASTNodeType["CALL_EXPR"], name=identifier, args=args)
        else:
            return ASTNode(ASTNodeType["IDENTIFIER"], name=identifier)

    elif token["type"] == "LPAR":
        Parser_advance()  # Consume '('
        expr = Parser_parse_expression()
        Parser_expect("RPAR")
        return expr
    elif token["type"] == "LBRACE":
        return Parser_parse_dict()
    else:
        raise SyntaxError(f"Unexpected token {token['type']} at line {token['line']}")


###############################################################################
############### INTERMEDIATE REPRESENTATION (IR) GENERATION ###############
###############################################################################

IROpType = {
    "LOAD_IMM": "LOAD_IMM",
    "LOAD_VAR": "LOAD_VAR",
    "STORE": "STORE",
    "ADD": "ADD",
    "SUB": "SUB",
    "MUL": "MUL",
    "DIV": "DIV",
    "JUMP": "JUMP",
    "JUMP_IF_ZERO": "JUMP_IF_ZERO",
    "JUMP_IF_NEG": "JUMP_IF_NEG",
    "LABEL": "LABEL",
    "CALL": "CALL",
    "RETURN": "RETURN",
    "ALLOCATE": "ALLOCATE",
    "DEALLOCATE": "DEALLOCATE",
    "PRINT": "PRINT",
}


class IRInstruction:
    def __init__(self, op_type, **kwargs):
        self.op_type = op_type
        for key, value in kwargs.items():
            setattr(self, key, value)

    def __repr__(self):
        attrs = ", ".join(
            f"{k}={v}" for k, v in self.__dict__.items() if k != "op_type"
        )
        return f"{self.op_type}({attrs})"


IRGenerator_instructions = []
IRGenerator_label_counter = 0
IRGenerator_current_function = None
IRGenerator_var_offsets = {}  # Maps variable names to stack offsets
IRGenerator_next_offset = 0
IRGenerator_function_start_indices = {}
IRGenerator_string_literals = {}  # Maps string values to their labels
IRGenerator_string_counter = 0


def IRGenerator_add_string_literal(value):
    """Add a string literal and return its label"""
    global IRGenerator_instructions, IRGenerator_label_counter, IRGenerator_current_function, IRGenerator_var_offsets, IRGenerator_next_offset, IRGenerator_function_start_indices, IRGenerator_string_literals, IRGenerator_string_counter

    if value in IRGenerator_string_literals:
        return IRGenerator_string_literals[value]

    label = f"str{IRGenerator_string_counter}"
    IRGenerator_string_counter += 1
    IRGenerator_string_literals[value] = label
    return label


def IRGenerator_new_label():
    """Generate a new unique label"""
    global IRGenerator_instructions, IRGenerator_label_counter, IRGenerator_current_function, IRGenerator_var_offsets, IRGenerator_next_offset, IRGenerator_function_start_indices, IRGenerator_string_literals, IRGenerator_string_counter
    label = f"L{IRGenerator_label_counter}"
    IRGenerator_label_counter += 1
    return label


def IRGenerator_allocate_var(name):
    """Allocate space for a variable and return its offset"""
    global IRGenerator_instructions, IRGenerator_label_counter, IRGenerator_current_function, IRGenerator_var_offsets, IRGenerator_next_offset, IRGenerator_function_start_indices, IRGenerator_string_literals, IRGenerator_string_counter
    if name in IRGenerator_var_offsets:
        return IRGenerator_var_offsets[name]

    offset = IRGenerator_next_offset
    IRGenerator_next_offset += 8  # Use 8 bytes (64-bit) per variable for x86-64
    IRGenerator_var_offsets[name] = offset
    return offset


def IRGenerator_generate(ast: ASTNode):
    """Generate IR from AST"""
    global IRGenerator_instructions, IRGenerator_label_counter, IRGenerator_current_function, IRGenerator_var_offsets, IRGenerator_next_offset, IRGenerator_function_start_indices, IRGenerator_string_literals, IRGenerator_string_counter
    if ast.node_type == ASTNodeType["PROGRAM"]:
        for stmt in ast.statements:
            IRGenerator_generate(stmt)

    elif ast.node_type == ASTNodeType["FUNCTION_DEF"]:
        # Save previous context
        prev_function = IRGenerator_current_function
        prev_offsets = IRGenerator_var_offsets
        prev_next_offset = IRGenerator_next_offset

        # Set up new function context
        IRGenerator_current_function = ast.name
        IRGenerator_var_offsets = {}
        IRGenerator_next_offset = 0

        function_start_idx = len(IRGenerator_instructions)
        IRGenerator_function_start_indices[ast.name] = function_start_idx

        # Add function label
        IRGenerator_instructions.append(IRInstruction(IROpType["LABEL"], name=ast.name))

        param_offsets = {}
        for i, param in enumerate(ast.params):
            # For x86-64, first 6 parameters are in registers
            # Allocate stack space for them
            offset = IRGenerator_allocate_var(param)
            param_offsets[param] = offset

        # Generate code for function body to discover all variables
        # (This is the first pass to determine total stack space needed)
        body_start_idx = len(IRGenerator_instructions)
        IRGenerator_generate(ast.body)

        # Calculate final stack size needed (next_offset contains it)
        stack_size = IRGenerator_next_offset

        # Ensure stack is 16-byte aligned for x86-64 ABI
        if stack_size % 16 != 0:
            stack_size += 16 - (stack_size % 16)

        # Now insert function prologue at the beginning
        IRGenerator_instructions.insert(
            function_start_idx + 1,  # Insert after the function label
            IRInstruction(IROpType["ALLOCATE"], size=stack_size),
        )

        body_start_idx += 1

        # Store parameters in their allocated stack positions
        for i, param in enumerate(ast.params):
            offset = param_offsets[param]
            if i < 6:  # x86-64 passes first 6 args in registers
                reg_mapping = {
                    0: 0,
                    1: 1,
                    2: 2,
                    3: 3,
                    4: 5,
                    5: 6,
                }  # Map to rdi, rsi, rdx, rcx, r8, r9
                IRGenerator_instructions.insert(
                    body_start_idx,  # Insert after ALLOCATE
                    IRInstruction(
                        IROpType["STORE"], dest=offset, src_reg=reg_mapping[i]
                    ),
                )
                body_start_idx += 1

        # Add implicit return if not present
        if len(IRGenerator_instructions) == 0 or (
            IRGenerator_instructions[-1].op_type != IROpType["RETURN"]
            and getattr(IRGenerator_instructions[-1], "name", None)
            != IRGenerator_current_function
        ):
            IRGenerator_instructions.append(IRInstruction(IROpType["RETURN"]))

        # Add deallocate before return
        if stack_size > 0:
            return_index = len(IRGenerator_instructions) - 1
            IRGenerator_instructions.insert(
                return_index,  # Insert before RETURN
                IRInstruction(IROpType["DEALLOCATE"], size=stack_size),
            )

        # Restore previous context
        IRGenerator_current_function = prev_function
        IRGenerator_var_offsets = prev_offsets
        IRGenerator_next_offset = prev_next_offset

    elif ast.node_type == ASTNodeType["BLOCK"]:
        for stmt in ast.statements:
            IRGenerator_generate(stmt)

    elif ast.node_type == ASTNodeType["RETURN_STMT"]:
        if ast.expression:
            # Evaluate the return expression and leave result in r0
            IRGenerator_generate_expr(ast.expression, 0)
        IRGenerator_instructions.append(IRInstruction(IROpType["RETURN"]))

    elif ast.node_type == ASTNodeType["IF_STMT"]:
        else_label = IRGenerator_new_label()
        end_label = IRGenerator_new_label()

        # Generate condition code, result in r0
        IRGenerator_generate_expr(ast.condition, 0)

        # Jump to else block if condition is false (0)
        IRGenerator_instructions.append(
            IRInstruction(IROpType["JUMP_IF_ZERO"], dest=else_label, src_reg=0)
        )

        # Generate then block
        IRGenerator_generate(ast.then_block)
        IRGenerator_instructions.append(IRInstruction(IROpType["JUMP"], dest=end_label))

        # Generate else block if present
        IRGenerator_instructions.append(
            IRInstruction(IROpType["LABEL"], name=else_label)
        )
        if ast.else_block:
            IRGenerator_generate(ast.else_block)

        IRGenerator_instructions.append(
            IRInstruction(IROpType["LABEL"], name=end_label)
        )

    elif ast.node_type == ASTNodeType["WHILE_STMT"]:
        start_label = IRGenerator_new_label()
        end_label = IRGenerator_new_label()

        # Generate loop start label
        IRGenerator_instructions.append(
            IRInstruction(IROpType["LABEL"], name=start_label)
        )

        # Generate condition code, result in r0
        IRGenerator_generate_expr(ast.condition, 0)

        # Jump to end if condition is false (0)
        IRGenerator_instructions.append(
            IRInstruction(IROpType["JUMP_IF_ZERO"], dest=end_label, src_reg=0)
        )

        # Generate loop body
        IRGenerator_generate(ast.body)

        # Jump back to start
        IRGenerator_instructions.append(
            IRInstruction(IROpType["JUMP"], dest=start_label)
        )

        # End label
        IRGenerator_instructions.append(
            IRInstruction(IROpType["LABEL"], name=end_label)
        )

    elif ast.node_type == ASTNodeType["ASSIGNMENT"]:
        # Evaluate the expression and store result in the variable
        offset = IRGenerator_allocate_var(ast.name)
        IRGenerator_generate_expr(ast.value, 0)
        IRGenerator_instructions.append(
            IRInstruction(IROpType["STORE"], dest=offset, src_reg=0)
        )

    elif ast.node_type in (
        ASTNodeType["BINARY_EXPR"],
        ASTNodeType["CALL_EXPR"],
        ASTNodeType["IDENTIFIER"],
        ASTNodeType["NUMBER"],
        ASTNodeType["STRING"],
    ):
        # For standalone expressions, evaluate and discard the result
        IRGenerator_generate_expr(ast, 0)

    return IRGenerator_instructions


def IRGenerator_generate_expr(expr: ASTNode, target_reg: int):
    """Generate code for an expression, leaving result in the target register"""
    global IRGenerator_instructions, IRGenerator_label_counter, IRGenerator_current_function, IRGenerator_var_offsets, IRGenerator_next_offset, IRGenerator_function_start_indices, IRGenerator_string_literals, IRGenerator_string_counter
    if expr.node_type == ASTNodeType["NUMBER"]:
        IRGenerator_instructions.append(
            IRInstruction(IROpType["LOAD_IMM"], dest_reg=target_reg, value=expr.value)
        )

    elif expr.node_type == ASTNodeType["STRING"]:
        label = IRGenerator_add_string_literal(expr.value)
        # For strings, we might want to load the address into a register
        # This would be handled differently in the code generator
        IRGenerator_instructions.append(
            IRInstruction(
                IROpType["LOAD_VAR"],
                dest_reg=target_reg,
                value=label,
                string_literal=True,
                string_value=expr.value,
                string_label=label,
            )
        )

    elif expr.node_type == ASTNodeType["IDENTIFIER"]:
        offset = IRGenerator_var_offsets.get(expr.name)
        if offset is None:
            raise ValueError(f"Undefined variable: {expr.name}")
        IRGenerator_instructions.append(
            IRInstruction(IROpType["LOAD_VAR"], dest_reg=target_reg, src_offset=offset)
        )

    elif expr.node_type == ASTNodeType["BINARY_EXPR"]:
        # Generate code for left and right operands
        IRGenerator_generate_expr(expr.left, target_reg)
        temp_reg = (target_reg + 1) % 4  # Use next register for right operand
        IRGenerator_generate_expr(expr.right, temp_reg)

        # Generate operation
        if expr.operator == "+":
            IRGenerator_instructions.append(
                IRInstruction(IROpType["ADD"], dest_reg=target_reg, src_reg=temp_reg)
            )
        elif expr.operator == "-":
            IRGenerator_instructions.append(
                IRInstruction(IROpType["SUB"], dest_reg=target_reg, src_reg=temp_reg)
            )
        elif expr.operator == "*":
            IRGenerator_instructions.append(
                IRInstruction(IROpType["MUL"], dest_reg=target_reg, src_reg=temp_reg)
            )
        elif expr.operator == "/":
            IRGenerator_instructions.append(
                IRInstruction(IROpType["DIV"], dest_reg=target_reg, src_reg=temp_reg)
            )

    elif expr.node_type == ASTNodeType["CALL_EXPR"]:
        # Evaluate arguments and put in argument registers
        for i, arg in enumerate(expr.args):
            if i < 4:  # ARM uses r0-r3 for first 4 args
                IRGenerator_generate_expr(arg, i)
            else:
                # Would need to push to stack for args > 4, simplifying for now
                pass

        # Generate call instruction
        if expr.name == "print":
            IRGenerator_instructions.append(
                IRInstruction(
                    IROpType["CALL"],
                    name=expr.name,
                    print_type="string",
                    string_label=IRGenerator_add_string_literal(expr.args[0].value),
                    string_length=len(expr.args[0].value),
                )
            )
        else:
            IRGenerator_instructions.append(
                IRInstruction(IROpType["CALL"], name=expr.name)
            )

        # Result is in r0, move to target register if needed
        if target_reg != 0:
            # This would be a MOV instruction in ARM
            # For simplicity, we'll use ADD with 0
            IRGenerator_instructions.append(
                IRInstruction(IROpType["ADD"], dest_reg=target_reg, src_reg=0)
            )

    elif expr.node_type == ASTNodeType["DICT"]:
        IRGenerator_instructions.append(
            IRInstruction(
                IROpType["ALLOCATE"], size=len(expr.items) * 16 + 8  # simplified
            )
        )
        IRGenerator_instructions.append(
            IRInstruction(
                IROpType["STORE"], dest=0, src_reg=target_reg, value=len(expr.items)
            )
        )

        for i, (key, value) in enumerate(expr.items):
            IRGenerator_generate_expr(key, target_reg)
            IRGenerator_instructions.append(
                IRInstruction(IROpType["STORE"], dest=i * 16 + 8, src_reg=target_reg)
            )
            IRGenerator_generate_expr(value, target_reg)
            IRGenerator_instructions.append(
                IRInstruction(IROpType["STORE"], dest=i * 16 + 16, src_reg=target_reg)
            )


###############################################################################
#                         MACHINE CODE GENERATION                             #
###############################################################################
#####  #####
# thanks to https://github.com/ImanHosseini/AtX for the translation
register_map = {
    "x86_64": [
        "%rax",
        "%rsi",
        "%rdx",
        "%rcx",
        "%rdi",
        "%rbx",
        "%r8d",
        "%r9d",
        "%r10d",
        "%r11d",
        "%r12d",
        "%r13d",
        "%r14d",
        "%r15",
        "%rsp",
    ],
}


X86_64Generator_data_counter = 0
X86_64Generator_data_section = []


def X86_64Generator_add_string_literal(value):
    """Add a string literal to the data section and return its label"""
    global X86_64Generator_data_counter, X86_64Generator_data_section
    label = f"str{X86_64Generator_data_counter}"
    X86_64Generator_data_counter += 1
    escaped_value = value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")
    X86_64Generator_data_section.append(f'{label}: .string "{escaped_value}"')
    return label


def X86_64Generator_generate_print_syscall(data_label, length):
    """Generate syscall instructions to print a string"""
    return [
        f"\tmovq\t$1, %rax",  # sys_write syscall number
        f"\tmovq\t$1, %rdi",  # file descriptor (stdout)
        f"\tleaq\t{data_label}(%rip), %rsi",  # RIP-relative address loading
        f"\tmovq\t${length}, %rdx",  # number of bytes to write
        f"\tsyscall",  # invoke system call
    ]


def X86_64Generator_generate_print_int_immediate(value):
    """Generate code to print an immediate integer value"""
    return [
        f"\tmovq\t${value}, %rdi",  # Move immediate value to first argument register
        f"\tcall\tint_to_string",  # Call int conversion function
        f"\tmovq\t%rax, %rsi",  # String address returned in %rax
        f"\tmovq\t%rdx, %rdx",  # Length returned in %rdx
        f"\tmovq\t$1, %rax",  # sys_write
        f"\tmovq\t$1, %rdi",  # stdout
        f"\tsyscall",
    ]


def X86_64Generator_generate_print_int_register(src_reg):
    """Generate code to print an integer from a register"""
    reg = register_map["x86_64"][src_reg]
    return [
        f"\tmovq\t{reg}, %rdi",  # Move register value to first argument register
        f"\tcall\tint_to_string",  # Call int conversion function
        f"\tmovq\t%rax, %rsi",  # String address returned in %rax
        f"\tmovq\t%rdx, %rdx",  # Length returned in %rdx
        f"\tmovq\t$1, %rax",  # sys_write
        f"\tmovq\t$1, %rdi",  # stdout
        f"\tsyscall",
    ]


def X86_64Generator_generate_int_to_string_function():
    """Generate a helper function to convert integer to string"""
    return [
        "",
        "int_to_string:",
        "\tpushq\t%rbp",
        "\tmovq\t%rsp, %rbp",
        "\tsubq\t$32, %rsp",  # Allocate space for string buffer
        "\tmovq\t%rdi, %rax",  # Move input integer to %rax
        "\tmovq\t$0, %rcx",  # Counter for digits
        "\tmovq\t$10, %rbx",  # Divisor
        "\tcmpq\t$0, %rax",  # Check if negative
        "\tjge\tconvert_loop",
        "\tnegq\t%rax",  # Make positive
        "\tmovb\t$45, -32(%rbp)",  # Store '-' character
        "\tincq\t%rcx",
        "convert_loop:",
        "\txorq\t%rdx, %rdx",  # Clear remainder
        "\tdivq\t%rbx",  # Divide by 10
        "\taddq\t$48, %rdx",  # Convert remainder to ASCII
        "\tmovb\t%dl, -32(%rbp,%rcx,1)",  # Store digit
        "\tincq\t%rcx",
        "\tcmpq\t$0, %rax",
        "\tjne\tconvert_loop",
        "\tleaq\t-32(%rbp), %rax",  # Return string address
        "\tmovq\t%rcx, %rdx",  # Return string length
        "\taddq\t$32, %rsp",
        "\tpopq\t%rbp",
        "\tret",
    ]


def X86_64Generator_generate(ir_instructions, string_literals) -> str:
    """Generate x84-64 assembly from IR instructions"""
    global X86_64Generator_data_counter, X86_64Generator_data_section
    asm_lines = [f'\t.file\t"example.py"', "\t.text", "\t.globl\tmain"]

    for string_literal in string_literals:
        X86_64Generator_add_string_literal(string_literal)

    # Process instructions
    for ir in ir_instructions:
        if ir.op_type == IROpType["LABEL"]:
            asm_lines.append(f"{ir.name}:")
        elif ir.op_type == IROpType["LOAD_IMM"]:
            asm_lines.append(
                f"\tmovq\t${ir.value}, {register_map['x86_64'][ir.dest_reg]}"
            )
        elif ir.op_type == IROpType["LOAD_VAR"]:
            if hasattr(ir, "string_literal") and ir.string_literal:
                asm_lines.append(
                    f"\tleaq\t{ir.value}(%rip), {register_map['x86_64'][ir.dest_reg]}"
                )
            else:
                asm_lines.append(
                    f"\tmovq\t-{ir.src_offset+4}(%rbp), {register_map['x86_64'][ir.dest_reg]}"
                )

        elif ir.op_type == IROpType["STORE"]:
            if hasattr(ir, "src_reg"):
                asm_lines.append(
                    f"\tmovq\t{register_map['x86_64'][ir.src_reg]}, -{ir.dest+8}(%rbp)"
                )
            else:
                asm_lines.append(f"\tmovq\t%r4, ${ir.value}")
                asm_lines.append(f"\tstr\t%r4, -{ir.dest+8}(%rbp)")
        elif ir.op_type == IROpType["ADD"]:
            asm_lines.append(
                f"\taddq\t{register_map['x86_64'][ir.src_reg]}, {register_map['x86_64'][ir.dest_reg]}"
            )
        elif ir.op_type == IROpType["SUB"]:
            asm_lines.append(
                f"\tsubq\t{register_map['x86_64'][ir.src_reg]}, {register_map['x86_64'][ir.dest_reg]}"
            )
        elif ir.op_type == IROpType["MUL"]:
            asm_lines.append(
                f"\timul\t{register_map['x86_64'][ir.src_reg]}, {register_map['x86_64'][ir.dest_reg]}"
            )
        elif ir.op_type == IROpType["DIV"]:
            asm_lines.append(f"\tmovq\t{register_map['x86_64'][ir.dest_reg]}, %eax")
            asm_lines.append("\tcdq")  # Sign-extend EAX into EDX:EAX
            asm_lines.append(f"\tidivq\t{register_map['x86_64'][ir.src_reg]}")
            if ir.dest_reg != 0:  # If result doesn't belong in EAX
                asm_lines.append(f"\tmovq\t%eax, {register_map['x86_64'][ir.dest_reg]}")
        elif ir.op_type == IROpType["JUMP"]:
            asm_lines.append(f"\tjmp\t{ir.dest}")
        elif ir.op_type == IROpType["JUMP_IF_ZERO"]:
            asm_lines.append(
                f"\ttest\t{register_map['x86_64'][ir.src_reg]}, {register_map['x86_64'][ir.src_reg]}"
            )
            asm_lines.append(f"\tje\t{ir.dest}")
        elif ir.op_type == IROpType["JUMP_IF_NEG"]:
            asm_lines.append(
                f"\ttest\t{register_map['x86_64'][ir.src_reg]}, {register_map['x86_64'][ir.src_reg]}"
            )
            asm_lines.append(f"\tjl\t{ir.dest}")
        elif ir.op_type == IROpType["CALL"]:
            if ir.name == "print":
                if hasattr(ir, "print_type") and ir.print_type == "string":
                    # Use the string_label and string_length from IR
                    asm_lines.extend(
                        X86_64Generator_generate_print_syscall(
                            ir.string_label, ir.string_length
                        )
                    )
                elif hasattr(ir, "print_type") and ir.print_type == "int":
                    # Handle integer printing
                    if hasattr(ir, "value"):
                        # Direct value
                        asm_lines.extend(
                            X86_64Generator_generate_print_int_immediate(ir.value)
                        )
                    else:
                        # From register
                        asm_lines.extend(
                            X86_64Generator_generate_print_int_register(ir.src_reg)
                        )
            else:
                asm_lines.append(f"\tcall\t{ir.name}")
        elif ir.op_type == IROpType["RETURN"]:
            asm_lines.append(f"\tret")
        elif ir.op_type == IROpType["ALLOCATE"]:
            # x86-64 function prologue
            asm_lines.append(
                f"\tenter\t${ir.size}, $0"
            )  # TODO: change 2nd arg to lexical nesting level of func
            # Optional: add stack canary for security
            # asm_lines.append("\tmovq\t%fs:40, %rax")
            # asm_lines.append("\tmovq\t%rax, -8(%rbp)")
            # asm_lines.append("\txorl\t%eax, %eax")
        elif ir.op_type == IROpType["DEALLOCATE"]:
            # x86 function epilogue: restore stack pointer and restore frame pointer
            asm_lines.append(f"\tleave")
            # leave just an alias for following lines
            # asm_lines.append("\tmovq\t%rbp, %rsp")  # Restore stack pointer
            # asm_lines.append("\tpopq\t%rbp")        # Restore old base pointer

    asm_lines.extend(X86_64Generator_generate_int_to_string_function())

    if X86_64Generator_data_section:
        asm_lines.extend(["", ".section .data"])
        asm_lines.extend(X86_64Generator_data_section)

    asm_lines.append("")
    return "\n".join(asm_lines)


MachineCodeGenerator_architecture = ""
IRGenerator_string_literals = {}


def MachineCodeGenerator(architecture):
    global MachineCodeGenerator_architecture
    MachineCodeGenerator_architecture = architecture.lower()


def MachineCodeGenerator_generate(ir_instructions):
    """Generate machine code for the target architecture"""
    return X86_64Generator_generate(ir_instructions, IRGenerator_string_literals)


def compile_code(source_code, architecture):
    """Compile source code to target machine code"""
    # preprocessed_code = preprocess(source_code)

    # Stage 1: Lexing
    # Lexer(preprocessed_code)
    Lexer(source_code)
    tokens = Lexer_tokenize()

    if VERBOSE:
        print("----------TOKENS----------")
        print(tokens[:1000])

    # Stage 2: Parsing
    Parser(tokens)
    ast = Parser_parse_file()

    if ast == -1:
        return -1

    if VERBOSE:
        print("----------AST----------")
        print(ast)

    # Stage 3: IR Generation
    ir = IRGenerator_generate(ast)

    if VERBOSE:
        print("----------IR----------")
        print(ir)

    # Stage 4: Target Code Generation
    MachineCodeGenerator(architecture)
    target_code = MachineCodeGenerator_generate(ir)

    if VERBOSE:
        print("----------ASSEMBLY----------")
        print(target_code)

    return target_code


def main():
    global VERBOSE
    # input_file = "main.py"
    input_file = "test/simple.pi"
    output_file = "newlang.s"
    target_architecture = "x86_64"

    VERBOSE = True

    source_code = ""

    with open(input_file) as fp:
        source_code = "".join(fp.readlines())

    arm_code = compile_code(source_code, target_architecture)

    if arm_code == -1:
        return -1

    # write to file
    with open(output_file, "+w") as fp:
        fp.write(arm_code)


if __name__ == "__main__":
    main()

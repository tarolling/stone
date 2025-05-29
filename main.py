import argparse

VERBOSE = False


TokenType = {
    "IDENTIFIER": "IDENTIFIER",  # Variable/function names
    "NUMBER": "NUMBER",  # Integer literals
    "STRING": "STRING",  # String literals
    "NEWLINE": "NEWLINE",  # Line breaks
    "INDENT": "INDENT",  # Indentation level increase
    "DEDENT": "DEDENT",  # Indentation level decrease
    # Operators and symbols
    "LPAR": "(",
    "RPAR": ")",
    "LSQB": "[",
    "RSQB": "]",
    "COLON": ":",
    "COMMA": ",",
    "SEMI": ";",
    "PLUS": "+",
    "MINUS": "-",
    "STAR": "*",
    "SLASH": "/",
    "VBAR": "|",
    "AMPER": "&",
    "LESS": "<",
    "GREATER": ">",
    "EQUAL": "=",
    "DOT": ".",
    "PERCENT": "%",
    "LBRACE": "{",
    "RBRACE": "}",
    "EQEQUAL": "==",
    "NOTEQUAL": "!=",
    "LESSEQUAL": "<=",
    "GREATEREQUAL": ">=",
    "TILDE": "~",
    "CIRCUMFLEX": "^",
    "LEFTSHIFT": "<<",
    "RIGHTSHIFT": ">>",
    "EXCLAMATION": "!",
    "EOF": 21,  # End of file
    # Keywords
    "DEF": "def",
    "RETURN": "return",
    "IF": "if",
    "ELSE": "else",
    "WHILE": "while",
    "FOR": "for",
    "BREAK": "break",
    "CONTINUE": "continue",
    "PASS": "pass",
    "CLASS": "class",
}

KEYWORDS = {
    "def": "DEF",
    "return": "RETURN",
    "if": "IF",
    "else": "ELSE",
    "while": "WHILE",
    "for": "FOR",
    "break": "BREAK",
    "continue": "CONTINUE",
    "pass": "PASS",
    "class": "CLASS",
}

###############################################################################
#                                  LEXER                                      #
###############################################################################


def Token(token_type, value, line, column):
    return {"type": token_type, "value": value, "line": line, "column": column}


def Token__repr__(token):
    return f"Token({token['type']}, '{token['value']}', line={token['line']}, col={token['column']})"


Lexer_source = ""
Lexer_position = 0
Lexer_line = 1
Lexer_column = 1
Lexer_indent_stack = [0]


def Lexer(source_code):
    global Lexer_source, Lexer_position, Lexer_line, Lexer_column, Lexer_indent_stack
    Lexer_source = source_code
    Lexer_position = 0
    Lexer_line = 1
    Lexer_column = 1
    Lexer_indent_stack = [0]


def Lexer_peek():
    """Look at the current character without advancing position"""
    global Lexer_source, Lexer_position
    if Lexer_position >= len(Lexer_source):
        return ""
    return Lexer_source[Lexer_position]


def Lexer_advance():
    """Get the current character and advance position"""
    global Lexer_source, Lexer_position, Lexer_line, Lexer_column
    if Lexer_position >= len(Lexer_source):
        return ""

    char = Lexer_source[Lexer_position]
    Lexer_position += 1
    Lexer_column += 1

    if char == "\n":
        Lexer_line += 1
        Lexer_column = 1

    return char


def Lexer_skip_whitespace():
    """Skip whitespace except for newlines and indentation"""
    while Lexer_peek() in " \t\r" and Lexer_peek() != "\n":
        Lexer_advance()


def Lexer_skip_to_eol():
    while Lexer_peek() not in ("\n", ""):
        Lexer_advance()


def Lexer_handle_indentation():
    """Process indentation at the beginning of a line"""
    global Lexer_line, Lexer_column, Lexer_indent_stack

    # Skip the newline character
    Lexer_advance()

    # Count spaces at the beginning of the new line
    indent_level = 0
    while Lexer_peek() == " ":
        Lexer_advance()
        indent_level += 1

    tokens = []

    tokens.append(Token("NEWLINE", TokenType["NEWLINE"], Lexer_line - 1, Lexer_column))

    current_indent = Lexer_indent_stack[-1]

    if indent_level > current_indent:
        Lexer_indent_stack.append(indent_level)
        tokens.append(Token("INDENT", TokenType["INDENT"], Lexer_line, Lexer_column))
    elif indent_level < current_indent:
        while indent_level < Lexer_indent_stack[-1]:
            Lexer_indent_stack.pop()
            tokens.append(
                Token("DEDENT", TokenType["DEDENT"], Lexer_line, Lexer_column)
            )

        if indent_level != Lexer_indent_stack[-1]:
            raise SyntaxError(f"Invalid indentation at line {Lexer_line}")

    return tokens


def Lexer_tokenize_number():
    """Tokenize a numeric literal"""
    global Lexer_line, Lexer_column

    start_col = Lexer_column
    number = ""

    # Collect digits
    while Lexer_peek().isdigit():
        number += Lexer_advance()

    return Token(TokenType["NUMBER"], number, Lexer_line, start_col)


def Lexer_tokenize_identifier():
    """Tokenize an identifier or keyword"""
    global Lexer_line, Lexer_column

    start_col = Lexer_column
    identifier = ""

    # Collect identifier characters (letters, digits, underscore)
    while Lexer_peek().isalnum() or Lexer_peek() == "_":
        identifier += Lexer_advance()

    # Check if this is a keyword
    if identifier in KEYWORDS:
        token_type = KEYWORDS[identifier]
    else:
        token_type = TokenType["IDENTIFIER"]

    return Token(token_type, identifier, Lexer_line, start_col)


def Lexer_tokenize_string():
    """Tokenize a string literal"""
    global Lexer_line, Lexer_column

    start_col = Lexer_column
    quote_char = Lexer_advance()  # Skip the opening quote
    string_value = ""

    while Lexer_peek() and Lexer_peek() != quote_char:
        char = Lexer_peek()
        if char == "\\":
            Lexer_advance()
            next_char = Lexer_peek()

            if not next_char:
                raise SyntaxError(f"Unterminated string at line {Lexer_line}")

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
        raise SyntaxError(f"Unterminated string at line {Lexer_line}")

    Lexer_advance()  # Skip the closing quote
    return Token(TokenType["STRING"], string_value, Lexer_line, start_col)


def Lexer_tokenize():
    """Convert source code to a list of tokens"""
    global Lexer_source, Lexer_position, Lexer_line, Lexer_column, Lexer_indent_stack

    tokens = []

    while Lexer_position < len(Lexer_source):
        char = Lexer_peek()

        # Handle whitespace
        if char in " \t\r":
            Lexer_skip_whitespace()

        elif char == "#":
            Lexer_skip_to_eol()

        # Handle newlines and indentation
        elif char == "\n":
            tokens.extend(Lexer_handle_indentation())

        # Handle numbers
        elif char.isdigit():
            tokens.append(Lexer_tokenize_number())

        # Handle identifiers and keywords
        elif char.isalpha() or char == "_":
            tokens.append(Lexer_tokenize_identifier())

        # Handle string literals
        elif char == '"' or char == "'":
            tokens.append(Lexer_tokenize_string())

        # Handle operators and symbols
        elif char == "+":
            tokens.append(Token("PLUS", TokenType["PLUS"], Lexer_line, Lexer_column))
            Lexer_advance()
        elif char == "-":
            tokens.append(Token("MINUS", TokenType["MINUS"], Lexer_line, Lexer_column))
            Lexer_advance()
        elif char == "*":
            tokens.append(Token("STAR", TokenType["STAR"], Lexer_line, Lexer_column))
            Lexer_advance()
        elif char == "/":
            tokens.append(Token("SLASH", TokenType["SLASH"], Lexer_line, Lexer_column))
            Lexer_advance()
        elif char == "=":
            start_col = Lexer_column
            Lexer_advance()
            if Lexer_peek() == "=":
                tokens.append(
                    Token("EQEQUAL", TokenType["EQEQUAL"], Lexer_line, start_col)
                )
                Lexer_advance()
            else:
                tokens.append(Token("EQUAL", TokenType["EQUAL"], Lexer_line, start_col))
        elif char == ".":
            tokens.append(Token("DOT", TokenType["DOT"], Lexer_line, Lexer_column))
            Lexer_advance()
        elif char == ">":
            start_col = Lexer_column
            Lexer_advance()
            if Lexer_peek() == "=":
                tokens.append(
                    Token(
                        "GREATEREQUAL", TokenType["GREATEREQUAL"], Lexer_line, start_col
                    )
                )
                Lexer_advance()
            elif Lexer_peek() == ">":
                tokens.append(
                    Token("RIGHTSHIFT", TokenType["RIGHTSHIFT"], Lexer_line, start_col)
                )
                Lexer_advance()
            else:
                tokens.append(
                    Token("GREATER", TokenType["GREATER"], Lexer_line, start_col)
                )
        elif char == "<":
            start_col = Lexer_column
            Lexer_advance()
            if Lexer_peek() == "=":
                tokens.append(
                    Token(TokenType["LESSEQUAL"], "<=", Lexer_line, start_col)
                )
                Lexer_advance()
            elif Lexer_peek() == "<":
                tokens.append(
                    Token(TokenType["LEFTSHIFT"], "<<", Lexer_line, start_col)
                )
                Lexer_advance()
            else:
                tokens.append(Token(TokenType["LESS"], "<", Lexer_line, start_col))
        elif char == "(":
            tokens.append(Token("LPAR", TokenType["LPAR"], Lexer_line, Lexer_column))
            Lexer_advance()
        elif char == ")":
            tokens.append(Token("RPAR", TokenType["RPAR"], Lexer_line, Lexer_column))
            Lexer_advance()
        elif char == "[":
            tokens.append(Token(TokenType["LSQB"], "[", Lexer_line, Lexer_column))
            Lexer_advance()
        elif char == "]":
            tokens.append(Token(TokenType["RSQB"], "]", Lexer_line, Lexer_column))
            Lexer_advance()
        elif char == "{":
            tokens.append(
                Token("LBRACE", TokenType["LBRACE"], Lexer_line, Lexer_column)
            )
            Lexer_advance()
        elif char == "}":
            tokens.append(
                Token("RBRACE", TokenType["RBRACE"], Lexer_line, Lexer_column)
            )
            Lexer_advance()
        elif char == ":":
            tokens.append(Token("COLON", TokenType["COLON"], Lexer_line, Lexer_column))
            Lexer_advance()
        elif char == ",":
            tokens.append(Token("COMMA", TokenType["COMMA"], Lexer_line, Lexer_column))
            Lexer_advance()
        elif char == "!":
            start_col = Lexer_column
            Lexer_advance()
            if Lexer_peek() == "=":
                tokens.append(Token(TokenType["NOTEQUAL"], "!=", Lexer_line, start_col))
                Lexer_advance()
            else:
                tokens.append(
                    Token(TokenType["EXCLAMATION"], "!", Lexer_line, start_col)
                )
        else:
            if VERBOSE:
                print(
                    f"Unexpected character '{char}' at line {Lexer_line}, column {Lexer_column}"
                )
            raise SyntaxError(
                f"Unexpected character '{char}' at line {Lexer_line}, column {Lexer_column}"
            )

    # Add DEDENT tokens for any remaining indentation levels
    while len(Lexer_indent_stack) > 1:
        Lexer_indent_stack.pop()
        tokens.append(Token("DEDENT", TokenType["DEDENT"], Lexer_line, Lexer_column))

    tokens.append(Token("EOF", TokenType["EOF"], Lexer_line, Lexer_column))

    return tokens


###############################################################################
#                                 PARSER                                      #
###############################################################################
ASTNodeType = {
    "PROGRAM": "Program",
    "FUNCTION_DEF": "Function_Def",
    "BLOCK": "Block",
    "RETURN_STMT": "Return_Stmt",
    "IF_STMT": "If_Stmt",
    "WHILE_STMT": "While_Stmt",
    "ASSIGNMENT": "Assignment",
    "BINARY_EXPR": "Binary_Expr",
    "CALL_EXPR": "Call_Expr",
    "IDENTIFIER": "Identifier",
    "NUMBER": "Number",
    "STRING": "String",
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
        return Token(TokenType["EOF"], "")
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


def Parser_parse():
    """Parse the entire program"""
    statements = []

    while Parser_peek()["type"] != TokenType["EOF"]:
        # Skip any standalone newlines at the top level
        if Parser_peek()["type"] == TokenType["NEWLINE"]:
            Parser_advance()
            continue

        statements.append(Parser_parse_statement())

    return ASTNode(ASTNodeType["PROGRAM"], statements=statements)


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
    elif token["type"] == "PASS":
        Parser_advance()
        return ASTNode(ASTNodeType["BLOCK"], name="pass")
    elif token["type"] == "IDENTIFIER":
        # This could be an assignment or a function call
        identifier = Parser_advance()["value"]

        if Parser_peek()["type"] == TokenType["EQUAL"]:
            Parser_advance()  # Consume the '='
            value = Parser_parse_expression()
            Parser_expect(TokenType["NEWLINE"])  # Expect newline after assignment
            return ASTNode(ASTNodeType["ASSIGNMENT"], name=identifier, value=value)
        else:
            # Put the identifier back and parse as expression
            Parser_position -= 1
            expr = Parser_parse_expression()
            Parser_expect(
                TokenType["NEWLINE"]
            )  # Expect newline after expression statement
            return expr
    else:
        raise SyntaxError(f"Unexpected token {token['type']} at line {token['line']}")


def Parser_parse_function_def():
    """Parse a function definition"""
    Parser_expect("DEF")
    name = Parser_expect(TokenType["IDENTIFIER"])["value"]

    Parser_expect(TokenType["LPAR"])
    params = []

    if Parser_peek()["type"] != TokenType["RPAR"]:
        # Parse parameters
        params.append(Parser_expect(TokenType["IDENTIFIER"])["value"])

        while Parser_peek()["type"] == TokenType["COMMA"]:
            Parser_advance()  # Consume the comma
            params.append(Parser_expect(TokenType["IDENTIFIER"])["value"])

    Parser_expect(TokenType["RPAR"])
    Parser_expect(TokenType["COLON"])
    Parser_expect(TokenType["NEWLINE"])

    # Parse function body
    body = Parser_parse_block()

    return ASTNode(ASTNodeType["FUNCTION_DEF"], name=name, params=params, body=body)


def Parser_parse_block():
    """Parse an indented block of statements"""
    Parser_expect(TokenType["INDENT"])
    statements = []

    while (
        Parser_peek()["type"] != TokenType["DEDENT"]
        and Parser_peek()["type"] != TokenType["EOF"]
    ):
        statements.append(Parser_parse_statement())

    Parser_expect(TokenType["DEDENT"])

    return ASTNode(ASTNodeType["BLOCK"], statements=statements)


def Parser_parse_return_statement():
    """Parse a return statement"""
    Parser_expect("RETURN")

    # Check if there's an expression or just a newline
    if Parser_peek()["type"] == TokenType["NEWLINE"]:
        expr = None
    else:
        expr = Parser_parse_expression()

    Parser_expect(TokenType["NEWLINE"])
    return ASTNode(ASTNodeType["RETURN_STMT"], expression=expr)


def Parser_parse_if_statement():
    """Parse an if statement"""
    Parser_expect("IF")
    condition = Parser_parse_expression()
    Parser_expect(TokenType["COLON"])
    Parser_expect(TokenType["NEWLINE"])

    then_block = Parser_parse_block()

    # Check for else clause
    else_block = None
    if Parser_peek()["type"] == TokenType["ELSE"]:
        Parser_advance()  # Consume 'else'
        Parser_expect(TokenType["COLON"])
        Parser_expect(TokenType["NEWLINE"])
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
    Parser_expect(TokenType["COLON"])
    Parser_expect(TokenType["NEWLINE"])

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
        TokenType["EQEQUAL"],
        TokenType["NOTEQUAL"],
        TokenType["LESS"],
        TokenType["GREATER"],
        TokenType["LESSEQUAL"],
        TokenType["GREATEREQUAL"],
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

    while Parser_peek()["type"] in (TokenType["PLUS"], TokenType["MINUS"]):
        operator = Parser_advance()["value"]
        right = Parser_parse_term()
        left = ASTNode(
            ASTNodeType["BINARY_EXPR"], left=left, operator=operator, right=right
        )

    return left


def Parser_parse_term():
    """Parse a term (multiplication/division)"""
    left = Parser_parse_factor()

    while Parser_peek()["type"] in (TokenType["STAR"], TokenType["SLASH"]):
        operator = Parser_advance()["value"]
        right = Parser_parse_factor()
        left = ASTNode(
            ASTNodeType["BINARY_EXPR"], left=left, operator=operator, right=right
        )

    return left


def Parser_parse_factor():
    """Parse a factor (unary expressions)"""
    if Parser_peek()["type"] in (TokenType["PLUS"], TokenType["MINUS"]):
        operator = Parser_advance()["value"]
        operand = Parser_parse_factor()
        return ASTNode(ASTNodeType["UNARY_EXPR"], operator=operator, operand=operand)

    return Parser_parse_primary()


def Parser_parse_primary():
    """Parse a primary expression (literal, identifier, call, parenthesized)"""
    token = Parser_peek()

    if token["type"] == TokenType["NUMBER"]:
        Parser_advance()
        return ASTNode(ASTNodeType["NUMBER"], value=int(token["value"]))

    elif token["type"] == TokenType["STRING"]:
        Parser_advance()
        return ASTNode(ASTNodeType["STRING"], value=token["value"])

    elif token["type"] == TokenType["IDENTIFIER"]:
        identifier = Parser_advance()["value"]

        # Check if this is a function call
        if Parser_peek()["type"] == TokenType["LPAR"]:
            Parser_advance()  # Consume '('
            args = []

            if Parser_peek()["type"] != TokenType["RPAR"]:
                args.append(Parser_parse_expression())

                while Parser_peek()["type"] == TokenType["COMMA"]:
                    Parser_advance()  # Consume ','
                    args.append(Parser_parse_expression())

            Parser_expect(TokenType["RPAR"])
            return ASTNode(ASTNodeType["CALL_EXPR"], name=identifier, args=args)
        else:
            return ASTNode(ASTNodeType["IDENTIFIER"], name=identifier)

    elif token["type"] == TokenType["LPAR"]:
        Parser_advance()  # Consume '('
        expr = Parser_parse_expression()
        Parser_expect(TokenType["RPAR"])
        return expr

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


def IRGenerator__init__():
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
    if value in IRGenerator_string_literals:
        return IRGenerator_string_literals[value]

    label = f"str{IRGenerator_string_counter}"
    IRGenerator_string_counter += 1
    IRGenerator_string_literals[value] = label
    return label


def IRGenerator_new_label():
    """Generate a new unique label"""
    label = f"L{IRGenerator_label_counter}"
    IRGenerator_label_counter += 1
    return label


def IRGenerator_allocate_var(name):
    """Allocate space for a variable and return its offset"""
    if name in IRGenerator_var_offsets:
        return IRGenerator_var_offsets[name]

    offset = IRGenerator_next_offset
    IRGenerator_next_offset += 8  # Use 8 bytes (64-bit) per variable for x86-64
    IRGenerator_var_offsets[name] = offset
    return offset


def IRGenerator_generate(ast: ASTNode):
    """Generate IR from AST"""
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
        IRGenerator_instructions.append(IRInstruction(IROpType["CALL"], name=expr.name))

        # Result is in r0, move to target register if needed
        if target_reg != 0:
            # This would be a MOV instruction in ARM
            # For simplicity, we'll use ADD with 0
            IRGenerator_instructions.append(
                IRInstruction(IROpType["ADD"], dest_reg=target_reg, src_reg=0)
            )


###############################################################################
#                         MACHINE CODE GENERATION                             #
###############################################################################
#####  #####
# thanks to https://github.com/ImanHosseini/AtX for the translation
register_map = {
    "arm": [
        "R0",
        "R1",
        "R2",
        "R3",
        "R4",
        "R5",
        "R6",
        "R7",
        "R8",
        "R9",
        "R10",
        "R11",
        "R12",
        "LR",
        "SP",
    ],
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


class ARMGenerator:
    def __init__(self):
        self.data_counter = 0
        self.data_section = []

    def add_string_literal(self, value: str) -> str:
        """Add a string literal to the data section and return its label"""
        label = f"str{self.data_counter}"
        self.data_counter += 1
        escaped_value = value.replace('"', '\\"')
        self.data_section.append(f'{label}: .asciz "{escaped_value}"')
        return label

    def generate(self, ir_instructions):
        """Generate ARM assembly from IR instructions"""
        asm_lines = [".global main", ""]

        # Process instructions
        for ir in ir_instructions:
            match ir.op_type:
                case IROpType.LABEL:
                    asm_lines.append(f"{ir.name}:")
                case IROpType.LOAD_IMM:
                    asm_lines.append(f"    mov r{ir.dest_reg}, #{ir.value}")
                case IROpType.LOAD_VAR:
                    asm_lines.append(
                        f"    ldr r{ir.dest_reg}, [fp, #-{ir.src_offset+4}]"
                    )
                case IROpType.STORE:
                    if hasattr(ir, "src_reg"):
                        asm_lines.append(f"    str r{ir.src_reg}, [fp, #-{ir.dest+4}]")
                    else:
                        # This would be a store immediate, which ARM doesn't support directly
                        asm_lines.append(f"    mov r4, #{ir.value}")
                        asm_lines.append(f"    str r4, [fp, #-{ir.dest+4}]")
                case IROpType.ADD:
                    asm_lines.append(
                        f"    add r{ir.dest_reg}, r{ir.dest_reg}, r{ir.src_reg}"
                    )
                case IROpType.SUB:
                    asm_lines.append(
                        f"    sub r{ir.dest_reg}, r{ir.dest_reg}, r{ir.src_reg}"
                    )
                case IROpType.MUL:
                    asm_lines.append(
                        f"    mul r{ir.dest_reg}, r{ir.dest_reg}, r{ir.src_reg}"
                    )
                case IROpType.DIV:
                    # ARM doesn't have a DIV instruction in base instruction set
                    # In a real compiler, you'd call a division routine or use SDIV on newer ARM processors
                    asm_lines.append(
                        f"    ; Division would be implemented with SDIV or a library call"
                    )
                    asm_lines.append(
                        f"    ; For ARMv7 with Thumb-2, you could use: sdiv r{ir.dest_reg}, r{ir.dest_reg}, r{ir.src_reg}"
                    )
                case IROpType.JUMP:
                    asm_lines.append(f"    b {ir.dest}")
                case IROpType.JUMP_IF_ZERO:
                    asm_lines.append(f"    cmp r{ir.src_reg}, #0")
                    asm_lines.append(f"    beq {ir.dest}")
                case IROpType.JUMP_IF_NEG:
                    asm_lines.append(f"    cmp r{ir.src_reg}, #0")
                    asm_lines.append(f"    blt {ir.dest}")
                case IROpType.CALL:
                    asm_lines.append(f"    bl {ir.name}")
                case IROpType.RETURN:
                    asm_lines.append(f"    mov pc, lr")
                case IROpType.ALLOCATE:
                    # ARM function prologue
                    asm_lines.append(
                        f"    push {{fp, lr}}"
                    )  # Save frame pointer and return address
                    asm_lines.append(f"    mov fp, sp")  # Set new frame pointer
                    asm_lines.append(
                        f"    sub sp, sp, #{ir.size}"
                    )  # Allocate stack space
                case IROpType.DEALLOCATE:
                    # ARM function epilogue
                    asm_lines.append(f"    mov sp, fp")  # Restore stack pointer
                    asm_lines.append(
                        f"    pop {{fp, pc}}"
                    )  # Restore frame pointer and return
                case _:
                    raise Exception("This don't exist twin")

        # Add data section if needed
        if self.data_section:
            asm_lines.append("")
            asm_lines.append(".data")
            asm_lines.extend(self.data_section)

        return "\n".join(asm_lines)


class X86_64Generator:
    def __init__(self):
        self.data_counter = 0
        self.data_section = []

    def add_string_literal(self, value: str):
        """Add a string literal to the data section and return its label"""
        label = f"str{self.data_counter}"
        self.data_counter += 1
        escaped_value = (
            value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")
        )
        self.data_section.append(f'{label}: .string "{escaped_value}"')
        return label

    def generate_print_syscall(self, data_label: str, length: int):
        """Generate syscall instructions to print a string"""
        return [
            f"\tmovq\t$1, %rax",  # sys_write syscall number
            f"\tmovq\t$1, %rdi",  # file descriptor (stdout)
            f"\tleaq\t{data_label}(%rip), %rsi",  # RIP-relative address loading
            f"\tmovq\t${length}, %rdx",  # number of bytes to write
            f"\tsyscall",  # invoke system call
        ]

    def generate_print_int_immediate(self, value: int):
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

    def generate_print_int_register(self, src_reg: int):
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

    def generate_int_to_string_function(self):
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

    def generate(self, ir_instructions, string_literals) -> str:
        """Generate x84-64 assembly from IR instructions"""
        asm_lines = [f'\t.file\t"example.py"', "\t.text", "\t.globl\tmain"]

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
                    asm_lines.append(
                        f"\tmovq\t%eax, {register_map['x86_64'][ir.dest_reg]}"
                    )
            elif ir.op_type == IROpType["JUMP"]:
                asm_lines.append(f"\tjmp\t{ir.dest}")
            elif ir.op_type == IROpType["JUMP_IF_ZERO"]:
                asm_lines.append(
                    f"\ttestl\t{register_map['x86_64'][ir.src_reg]}, {register_map['x86_64'][ir.src_reg]}"
                )
                asm_lines.append(f"\tje\t{ir.dest}")
            elif ir.op_type == IROpType["JUMP_IF_NEG"]:
                asm_lines.append(
                    f"\ttestl\t{register_map['x86_64'][ir.src_reg]}, {register_map['x86_64'][ir.src_reg]}"
                )
                asm_lines.append(f"\tjl\t{ir.dest}")
            elif ir.op_type == IROpType["CALL"]:
                if ir.name == "print":
                    print(repr(ir))
                    if hasattr(ir, "print_type") and ir.print_type == "string":
                        # Use the string_label and string_length from IR
                        asm_lines.extend(
                            self.generate_print_syscall(
                                ir.string_label, ir.string_length
                            )
                        )
                    elif hasattr(ir, "print_type") and ir.print_type == "int":
                        # Handle integer printing
                        if hasattr(ir, "value"):
                            # Direct value
                            asm_lines.extend(
                                self.generate_print_int_immediate(ir.value)
                            )
                        else:
                            # From register
                            asm_lines.extend(
                                self.generate_print_int_register(ir.src_reg)
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

        asm_lines.extend(self.generate_int_to_string_function())

        for string_literal in string_literals:
            IRGenerator_add_string_literal(string_literal)

        if self.data_section:
            asm_lines.extend(["", ".section .data"])
            asm_lines.extend(self.data_section)

        asm_lines.append("")
        return "\n".join(asm_lines)


class MachineCodeGenerator:
    def __init__(self, architecture: str = "arm"):
        self.architecture = architecture.lower()
        IRGenerator_string_literals = {}

    def generate(self, ir_instructions) -> str:
        """Generate machine code for the target architecture"""
        if self.architecture == "arm":
            gen = ARMGenerator()
            return gen.generate(ir_instructions)
        elif self.architecture == "x86_64":
            gen = X86_64Generator()
            return gen.generate(ir_instructions, IRGenerator_string_literals)
        else:
            raise ValueError(f"Unsupported architecture: {self.architecture}")


def compile_code(source_code, architecture: str = "arm"):
    """Compile source code to target machine code"""
    # Stage 1: Lexing
    Lexer(source_code)
    tokens = Lexer_tokenize()

    if VERBOSE:
        print("----------TOKENS----------")
        print(tokens)

    # Stage 2: Parsing
    Parser(tokens)
    ast = Parser_parse()

    if VERBOSE:
        print("----------AST----------")
        print(ast)

    # Stage 3: IR Generation
    ir = IRGenerator_generate(ast)

    if VERBOSE:
        print("----------IR----------")
        print(ir)

    # Stage 4: Target Code Generation
    code_gen = MachineCodeGenerator(architecture)
    code_gen.string_literals = IRGenerator_string_literals
    target_code = code_gen.generate(ir)

    if VERBOSE:
        print("----------ASSEMBLY----------")
        print(target_code)

    return target_code


def main():
    global VERBOSE
    parser = argparse.ArgumentParser(prog="newlang")
    parser.add_argument("-t", "--target", choices=["arm", "x86_64"], default="arm")
    parser.add_argument("--debug", action="store_true", default=False)

    args = vars(parser.parse_args())
    VERBOSE = args["debug"]

    source_code = ""

    with open("test/simple.pi") as fp:
        source_code = "".join(fp.readlines())

    # Compile to ARM assembly
    arm_code = compile_code(source_code, args["target"])

    # write to file
    with open("simple.s", "+w") as fp:
        fp.write(arm_code)


if __name__ == "__main__":
    main()

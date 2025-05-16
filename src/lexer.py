from enum import Enum, auto
from typing import List, Dict, Union, Optional, Tuple


###############################################################################
# PART 1: LEXER - Convert source text to tokens
###############################################################################


class TokenType(Enum):
    IDENTIFIER = auto()  # Variable/function names
    NUMBER = auto()  # Integer literals
    STRING = auto()  # String literals
    NEWLINE = auto()  # Line breaks
    INDENT = auto()  # Indentation level increase
    DEDENT = auto()  # Indentation level decrease

    # Keywords
    DEF = auto()
    RETURN = auto()
    IF = auto()
    ELSE = auto()
    WHILE = auto()

    # Operators and symbols
    PLUS = auto()
    MINUS = auto()
    MULTIPLY = auto()
    DIVIDE = auto()
    ASSIGN = auto()
    EQUALS = auto()
    LPAREN = auto()
    RPAREN = auto()
    COLON = auto()
    COMMA = auto()

    EOF = auto()  # End of file


class Token:
    def __init__(
        self, token_type: TokenType, value: str = "", line: int = 0, column: int = 0
    ):
        self.type = token_type
        self.value = value
        self.line = line
        self.column = column

    def __repr__(self):
        return (
            f"Token({self.type}, '{self.value}', line={self.line}, col={self.column})"
        )


class Lexer:
    def __init__(self, source_code: str):
        self.source = source_code
        self.position = 0
        self.line = 1
        self.column = 1
        self.indent_stack = [0]  # Stack to track indentation levels

        # Define keywords
        self.keywords = {
            "def": TokenType.DEF,
            "return": TokenType.RETURN,
            "if": TokenType.IF,
            "else": TokenType.ELSE,
            "while": TokenType.WHILE,
        }

    def peek(self) -> str:
        """Look at the current character without advancing position"""
        if self.position >= len(self.source):
            return ""
        return self.source[self.position]

    def advance(self) -> str:
        """Get the current character and advance position"""
        if self.position >= len(self.source):
            return ""

        char = self.source[self.position]
        self.position += 1
        self.column += 1

        if char == "\n":
            self.line += 1
            self.column = 1

        return char

    def skip_whitespace(self):
        """Skip whitespace except for newlines and indentation"""
        while self.peek() in " \t\r" and self.peek() != "\n":
            self.advance()

    def handle_indentation(self) -> List[Token]:
        """Process indentation at the beginning of a line"""
        # Skip the newline character
        self.advance()

        # Count spaces at the beginning of the new line
        indent_level = 0
        while self.peek() == " ":
            self.advance()
            indent_level += 1

        tokens = []

        tokens.append(Token(TokenType.NEWLINE, "\n", self.line - 1, self.column))

        # Compare with the current indentation level
        current_indent = self.indent_stack[-1]

        if indent_level > current_indent:
            # Indentation increased
            self.indent_stack.append(indent_level)
            tokens.append(Token(TokenType.INDENT, "", self.line, self.column))
        elif indent_level < current_indent:
            # Indentation decreased - may need multiple DEDENT tokens
            while indent_level < self.indent_stack[-1]:
                self.indent_stack.pop()
                tokens.append(Token(TokenType.DEDENT, "", self.line, self.column))

            if indent_level != self.indent_stack[-1]:
                raise SyntaxError(f"Invalid indentation at line {self.line}")

        return tokens

    def tokenize_number(self) -> Token:
        """Tokenize a numeric literal"""
        start_col = self.column
        number = ""

        # Collect digits
        while self.peek().isdigit():
            number += self.advance()

        return Token(TokenType.NUMBER, number, self.line, start_col)

    def tokenize_identifier(self) -> Token:
        """Tokenize an identifier or keyword"""
        start_col = self.column
        identifier = ""

        # Collect identifier characters (letters, digits, underscore)
        while self.peek().isalnum() or self.peek() == "_":
            identifier += self.advance()

        # Check if this is a keyword
        token_type = self.keywords.get(identifier, TokenType.IDENTIFIER)

        return Token(token_type, identifier, self.line, start_col)

    def tokenize_string(self) -> Token:
        """Tokenize a string literal"""
        start_col = self.column
        self.advance()  # Skip the opening quote
        string = ""

        while self.peek() and self.peek() != '"':
            string += self.advance()

        if self.peek() != '"':
            raise SyntaxError(f"Unterminated string at line {self.line}")

        self.advance()  # Skip the closing quote
        return Token(TokenType.STRING, string, self.line, start_col)

    def tokenize(self) -> List[Token]:
        """Convert source code to a list of tokens"""
        tokens = []

        while self.position < len(self.source):
            char = self.peek()

            # Handle whitespace
            if char in " \t\r":
                self.skip_whitespace()

            # Handle newlines and indentation
            elif char == "\n":
                tokens.extend(self.handle_indentation())

            # Handle numbers
            elif char.isdigit():
                tokens.append(self.tokenize_number())

            # Handle identifiers and keywords
            elif char.isalpha() or char == "_":
                tokens.append(self.tokenize_identifier())

            # Handle string literals
            elif char == '"':
                tokens.append(self.tokenize_string())

            # Handle operators and symbols
            elif char == "+":
                tokens.append(Token(TokenType.PLUS, "+", self.line, self.column))
                self.advance()
            elif char == "-":
                tokens.append(Token(TokenType.MINUS, "-", self.line, self.column))
                self.advance()
            elif char == "*":
                tokens.append(Token(TokenType.MULTIPLY, "*", self.line, self.column))
                self.advance()
            elif char == "/":
                tokens.append(Token(TokenType.DIVIDE, "/", self.line, self.column))
                self.advance()
            elif char == "=":
                start_col = self.column
                self.advance()
                if self.peek() == "=":
                    self.advance()
                    tokens.append(Token(TokenType.EQUALS, "==", self.line, start_col))
                else:
                    tokens.append(Token(TokenType.ASSIGN, "=", self.line, start_col))
            elif char == "(":
                tokens.append(Token(TokenType.LPAREN, "(", self.line, self.column))
                self.advance()
            elif char == ")":
                tokens.append(Token(TokenType.RPAREN, ")", self.line, self.column))
                self.advance()
            elif char == ":":
                tokens.append(Token(TokenType.COLON, ":", self.line, self.column))
                self.advance()
            elif char == ",":
                tokens.append(Token(TokenType.COMMA, ",", self.line, self.column))
                self.advance()
            else:
                raise SyntaxError(
                    f"Unexpected character '{char}' at line {self.line}, column {self.column}"
                )

        # Add DEDENT tokens for any remaining indentation levels
        while len(self.indent_stack) > 1:
            self.indent_stack.pop()
            tokens.append(Token(TokenType.DEDENT, "", self.line, self.column))

        # Add EOF token
        tokens.append(Token(TokenType.EOF, "", self.line, self.column))

        return tokens

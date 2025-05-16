from enum import Enum, auto
from typing import List, Dict, Union, Optional, Tuple
from src.lexer import Token, TokenType


###############################################################################
# PART 2: PARSER - Convert tokens to AST
###############################################################################


class ASTNodeType(Enum):
    PROGRAM = auto()
    FUNCTION_DEF = auto()
    BLOCK = auto()
    RETURN_STMT = auto()
    IF_STMT = auto()
    WHILE_STMT = auto()
    ASSIGNMENT = auto()
    BINARY_EXPR = auto()
    CALL_EXPR = auto()
    IDENTIFIER = auto()
    NUMBER = auto()
    STRING = auto()


class ASTNode:
    def __init__(self, node_type: ASTNodeType, **kwargs):
        self.node_type = node_type
        for key, value in kwargs.items():
            setattr(self, key, value)

    def __repr__(self):
        attrs = ", ".join(
            f"{k}={v}" for k, v in self.__dict__.items() if k != "node_type"
        )
        return f"{self.node_type.name}({attrs})"


class Parser:
    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.position = 0

    def peek(self) -> Token:
        """Look at the current token without advancing position"""
        if self.position >= len(self.tokens):
            return Token(TokenType.EOF, "")
        return self.tokens[self.position]

    def advance(self) -> Token:
        """Get the current token and advance position"""
        token = self.peek()
        self.position += 1
        return token

    def expect(self, token_type: TokenType) -> Token:
        """Expect a specific token type, advance and return it"""
        token = self.peek()
        if token.type != token_type:
            raise SyntaxError(
                f"Expected {token_type}, got {token.type} at line {token.line}"
            )
        return self.advance()

    def parse(self) -> ASTNode:
        """Parse the entire program"""
        statements = []

        while self.peek().type != TokenType.EOF:
            # Skip any standalone newlines at the top level
            if self.peek().type == TokenType.NEWLINE:
                self.advance()
                continue

            statements.append(self.parse_statement())

        return ASTNode(ASTNodeType.PROGRAM, statements=statements)

    def parse_statement(self) -> ASTNode:
        """Parse a statement"""
        token = self.peek()

        if token.type == TokenType.DEF:
            return self.parse_function_def()
        elif token.type == TokenType.RETURN:
            return self.parse_return_statement()
        elif token.type == TokenType.IF:
            return self.parse_if_statement()
        elif token.type == TokenType.WHILE:
            return self.parse_while_statement()
        elif token.type == TokenType.IDENTIFIER:
            # This could be an assignment or a function call
            identifier = self.advance().value

            if self.peek().type == TokenType.ASSIGN:
                self.advance()  # Consume the '='
                value = self.parse_expression()
                self.expect(TokenType.NEWLINE)  # Expect newline after assignment
                return ASTNode(ASTNodeType.ASSIGNMENT, name=identifier, value=value)
            else:
                # Put the identifier back and parse as expression
                self.position -= 1
                expr = self.parse_expression()
                self.expect(
                    TokenType.NEWLINE
                )  # Expect newline after expression statement
                return expr
        else:
            raise SyntaxError(f"Unexpected token {token.type} at line {token.line}")

    def parse_function_def(self) -> ASTNode:
        """Parse a function definition"""
        self.expect(TokenType.DEF)
        name = self.expect(TokenType.IDENTIFIER).value

        self.expect(TokenType.LPAREN)
        params = []

        if self.peek().type != TokenType.RPAREN:
            # Parse parameters
            params.append(self.expect(TokenType.IDENTIFIER).value)

            while self.peek().type == TokenType.COMMA:
                self.advance()  # Consume the comma
                params.append(self.expect(TokenType.IDENTIFIER).value)

        self.expect(TokenType.RPAREN)
        self.expect(TokenType.COLON)
        self.expect(TokenType.NEWLINE)

        # Parse function body
        body = self.parse_block()

        return ASTNode(ASTNodeType.FUNCTION_DEF, name=name, params=params, body=body)

    def parse_block(self) -> ASTNode:
        """Parse an indented block of statements"""
        self.expect(TokenType.INDENT)
        statements = []

        while (
            self.peek().type != TokenType.DEDENT and self.peek().type != TokenType.EOF
        ):
            statements.append(self.parse_statement())

        self.expect(TokenType.DEDENT)

        return ASTNode(ASTNodeType.BLOCK, statements=statements)

    def parse_return_statement(self) -> ASTNode:
        """Parse a return statement"""
        self.expect(TokenType.RETURN)

        # Check if there's an expression or just a newline
        if self.peek().type == TokenType.NEWLINE:
            expr = None
        else:
            expr = self.parse_expression()

        self.expect(TokenType.NEWLINE)
        return ASTNode(ASTNodeType.RETURN_STMT, expression=expr)

    def parse_if_statement(self) -> ASTNode:
        """Parse an if statement"""
        self.expect(TokenType.IF)
        condition = self.parse_expression()
        self.expect(TokenType.COLON)
        self.expect(TokenType.NEWLINE)

        then_block = self.parse_block()

        # Check for else clause
        else_block = None
        if self.peek().type == TokenType.ELSE:
            self.advance()  # Consume 'else'
            self.expect(TokenType.COLON)
            self.expect(TokenType.NEWLINE)
            else_block = self.parse_block()

        return ASTNode(
            ASTNodeType.IF_STMT,
            condition=condition,
            then_block=then_block,
            else_block=else_block,
        )

    def parse_while_statement(self) -> ASTNode:
        """Parse a while statement"""
        self.expect(TokenType.WHILE)
        condition = self.parse_expression()
        self.expect(TokenType.COLON)
        self.expect(TokenType.NEWLINE)

        body = self.parse_block()

        return ASTNode(ASTNodeType.WHILE_STMT, condition=condition, body=body)

    def parse_expression(self) -> ASTNode:
        """Parse an expression (currently only simple expressions)"""
        return self.parse_term()

    def parse_term(self) -> ASTNode:
        """Parse a term (addition/subtraction)"""
        left = self.parse_factor()

        while self.peek().type in (TokenType.PLUS, TokenType.MINUS):
            operator = self.advance().value
            right = self.parse_factor()
            left = ASTNode(
                ASTNodeType.BINARY_EXPR, left=left, operator=operator, right=right
            )

        return left

    def parse_factor(self) -> ASTNode:
        """Parse a factor (multiplication/division)"""
        left = self.parse_primary()

        while self.peek().type in (TokenType.MULTIPLY, TokenType.DIVIDE):
            operator = self.advance().value
            right = self.parse_primary()
            left = ASTNode(
                ASTNodeType.BINARY_EXPR, left=left, operator=operator, right=right
            )

        return left

    def parse_primary(self) -> ASTNode:
        """Parse a primary expression (literal, identifier, call, parenthesized)"""
        token = self.peek()

        if token.type == TokenType.NUMBER:
            self.advance()
            return ASTNode(ASTNodeType.NUMBER, value=int(token.value))

        elif token.type == TokenType.STRING:
            self.advance()
            return ASTNode(ASTNodeType.STRING, value=token.value)

        elif token.type == TokenType.IDENTIFIER:
            identifier = self.advance().value

            # Check if this is a function call
            if self.peek().type == TokenType.LPAREN:
                self.advance()  # Consume '('
                args = []

                if self.peek().type != TokenType.RPAREN:
                    args.append(self.parse_expression())

                    while self.peek().type == TokenType.COMMA:
                        self.advance()  # Consume ','
                        args.append(self.parse_expression())

                self.expect(TokenType.RPAREN)
                return ASTNode(ASTNodeType.CALL_EXPR, name=identifier, args=args)
            else:
                return ASTNode(ASTNodeType.IDENTIFIER, name=identifier)

        elif token.type == TokenType.LPAREN:
            self.advance()  # Consume '('
            expr = self.parse_expression()
            self.expect(TokenType.RPAREN)
            return expr

        else:
            raise SyntaxError(f"Unexpected token {token.type} at line {token.line}")

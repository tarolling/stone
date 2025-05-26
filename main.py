import argparse


TokenType = {
    "IDENTIFIER": 0,  # Variable/function names
    "NUMBER": 1,  # Integer literals
    "STRING": 2,  # String literals
    "NEWLINE": 3,  # Line breaks
    "INDENT": 4,  # Indentation level increase
    "DEDENT": 5,  # Indentation level decrease
    # Keywords
    "DEF": 6,
    "RETURN": 7,
    "IF": 8,
    "ELSE": 9,
    "WHILE": 10,
    "FOR": 22,
    # Operators and symbols
    "PLUS": 11,
    "MINUS": 12,
    "MULTIPLY": 13,
    "DIVIDE": 14,
    "ASSIGN": 15,
    "EQUALS": 16,
    "LPAREN": 17,
    "RPAREN": 18,
    "COLON": 19,
    "COMMA": 20,
    "EOF": 21,  # End of file
}


class Token:
    def __init__(self, token_type, value: str = "", line: int = 0, column: int = 0):
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
            "def": TokenType["DEF"],
            "return": TokenType["RETURN"],
            "if": TokenType["IF"],
            "else": TokenType["ELSE"],
            "while": TokenType["WHILE"],
            "for": TokenType["FOR"],
        }

    def peek(self):
        """Look at the current character without advancing position"""
        if self.position >= len(self.source):
            return ""
        return self.source[self.position]

    def advance(self):
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

    def handle_indentation(self):
        """Process indentation at the beginning of a line"""
        # Skip the newline character
        self.advance()

        # Count spaces at the beginning of the new line
        indent_level = 0
        while self.peek() == " ":
            self.advance()
            indent_level += 1

        tokens = []

        tokens.append(Token(TokenType["NEWLINE"], "\n", self.line - 1, self.column))

        current_indent = self.indent_stack[-1]

        if indent_level > current_indent:
            self.indent_stack.append(indent_level)
            tokens.append(Token(TokenType["INDENT"], "", self.line, self.column))
        elif indent_level < current_indent:
            while indent_level < self.indent_stack[-1]:
                self.indent_stack.pop()
                tokens.append(Token(TokenType["DEDENT"], "", self.line, self.column))

            if indent_level != self.indent_stack[-1]:
                raise SyntaxError(f"Invalid indentation at line {self.line}")

        return tokens

    def tokenize_number(self):
        """Tokenize a numeric literal"""
        start_col = self.column
        number = ""

        # Collect digits
        while self.peek().isdigit():
            number += self.advance()

        return Token(TokenType["NUMBER"], number, self.line, start_col)

    def tokenize_identifier(self):
        """Tokenize an identifier or keyword"""
        start_col = self.column
        identifier = ""

        # Collect identifier characters (letters, digits, underscore)
        while self.peek().isalnum() or self.peek() == "_":
            identifier += self.advance()

        # Check if this is a keyword
        token_type = self.keywords.get(identifier, TokenType["IDENTIFIER"])

        return Token(token_type, identifier, self.line, start_col)

    def tokenize_string(self):
        """Tokenize a string literal"""
        start_col = self.column
        self.advance()  # Skip the opening quote
        string = ""

        while self.peek() and self.peek() != '"':
            string += self.advance()

        if self.peek() != '"':
            raise SyntaxError(f"Unterminated string at line {self.line}")

        self.advance()  # Skip the closing quote
        return Token(TokenType["STRING"], string, self.line, start_col)

    def tokenize(self):
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
                tokens.append(Token(TokenType["PLUS"], "+", self.line, self.column))
                self.advance()
            elif char == "-":
                tokens.append(Token(TokenType["MINUS"], "-", self.line, self.column))
                self.advance()
            elif char == "*":
                tokens.append(Token(TokenType["MULTIPLY"], "*", self.line, self.column))
                self.advance()
            elif char == "/":
                tokens.append(Token(TokenType["DIVIDE"], "/", self.line, self.column))
                self.advance()
            elif char == "=":
                start_col = self.column
                self.advance()
                if self.peek() == "=":
                    self.advance()
                    tokens.append(
                        Token(TokenType["EQUALS"], "==", self.line, start_col)
                    )
                else:
                    tokens.append(Token(TokenType["ASSIGN"], "=", self.line, start_col))
            elif char == "(":
                tokens.append(Token(TokenType["LPAREN"], "(", self.line, self.column))
                self.advance()
            elif char == ")":
                tokens.append(Token(TokenType["RPAREN"], ")", self.line, self.column))
                self.advance()
            elif char == ":":
                tokens.append(Token(TokenType["COLON"], ":", self.line, self.column))
                self.advance()
            elif char == ",":
                tokens.append(Token(TokenType["COMMA"], ",", self.line, self.column))
                self.advance()
            else:
                raise SyntaxError(
                    f"Unexpected character '{char}' at line {self.line}, column {self.column}"
                )

        # Add DEDENT tokens for any remaining indentation levels
        while len(self.indent_stack) > 1:
            self.indent_stack.pop()
            tokens.append(Token(TokenType["DEDENT"], "", self.line, self.column))

        # Add EOF token
        tokens.append(Token(TokenType["EOF"], "", self.line, self.column))

        return tokens


##### PARSER #####
ASTNodeType = {
    "PROGRAM": 0,
    "FUNCTION_DEF": 1,
    "BLOCK": 2,
    "RETURN_STMT": 3,
    "IF_STMT": 4,
    "WHILE_STMT": 5,
    "ASSIGNMENT": 6,
    "BINARY_EXPR": 7,
    "CALL_EXPR": 8,
    "IDENTIFIER": 9,
    "NUMBER": 10,
    "STRING": 11,
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


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.position = 0

    def peek(self):
        """Look at the current token without advancing position"""
        if self.position >= len(self.tokens):
            return Token(TokenType.EOF, "")
        return self.tokens[self.position]

    def advance(self):
        """Get the current token and advance position"""
        token = self.peek()
        self.position += 1
        return token

    def expect(self, token_type):
        """Expect a specific token type, advance and return it"""
        token = self.peek()
        if token.type != token_type:
            raise SyntaxError(
                f"Expected {token_type}, got {token.type} at line {token.line}"
            )
        return self.advance()

    def parse(self):
        """Parse the entire program"""
        statements = []

        while self.peek().type != TokenType["EOF"]:
            # Skip any standalone newlines at the top level
            if self.peek().type == TokenType["NEWLINE"]:
                self.advance()
                continue

            statements.append(self.parse_statement())

        return ASTNode(ASTNodeType["PROGRAM"], statements=statements)

    def parse_statement(self):
        """Parse a statement"""
        token = self.peek()

        if token.type == TokenType["DEF"]:
            return self.parse_function_def()
        elif token.type == TokenType["RETURN"]:
            return self.parse_return_statement()
        elif token.type == TokenType["IF"]:
            return self.parse_if_statement()
        elif token.type == TokenType["WHILE"]:
            return self.parse_while_statement()
        elif token.type == TokenType["IDENTIFIER"]:
            # This could be an assignment or a function call
            identifier = self.advance().value

            if self.peek().type == TokenType["ASSIGN"]:
                self.advance()  # Consume the '='
                value = self.parse_expression()
                self.expect(TokenType["NEWLINE"])  # Expect newline after assignment
                return ASTNode(ASTNodeType["ASSIGNMENT"], name=identifier, value=value)
            else:
                # Put the identifier back and parse as expression
                self.position -= 1
                expr = self.parse_expression()
                self.expect(
                    TokenType["NEWLINE"]
                )  # Expect newline after expression statement
                return expr
        else:
            raise SyntaxError(f"Unexpected token {token.type} at line {token.line}")

    def parse_function_def(self):
        """Parse a function definition"""
        self.expect(TokenType["DEF"])
        name = self.expect(TokenType["IDENTIFIER"]).value

        self.expect(TokenType["LPAREN"])
        params = []

        if self.peek().type != TokenType["RPAREN"]:
            # Parse parameters
            params.append(self.expect(TokenType["IDENTIFIER"]).value)

            while self.peek().type == TokenType["COMMA"]:
                self.advance()  # Consume the comma
                params.append(self.expect(TokenType["IDENTIFIER"]).value)

        self.expect(TokenType["RPAREN"])
        self.expect(TokenType["COLON"])
        self.expect(TokenType["NEWLINE"])

        # Parse function body
        body = self.parse_block()

        return ASTNode(ASTNodeType["FUNCTION_DEF"], name=name, params=params, body=body)

    def parse_block(self):
        """Parse an indented block of statements"""
        self.expect(TokenType["INDENT"])
        statements = []

        while (
            self.peek().type != TokenType["DEDENT"]
            and self.peek().type != TokenType["EOF"]
        ):
            statements.append(self.parse_statement())

        self.expect(TokenType["DEDENT"])

        return ASTNode(ASTNodeType["BLOCK"], statements=statements)

    def parse_return_statement(self):
        """Parse a return statement"""
        self.expect(TokenType["RETURN"])

        # Check if there's an expression or just a newline
        if self.peek().type == TokenType["NEWLINE"]:
            expr = None
        else:
            expr = self.parse_expression()

        self.expect(TokenType["NEWLINE"])
        return ASTNode(ASTNodeType["RETURN_STMT"], expression=expr)

    def parse_if_statement(self):
        """Parse an if statement"""
        self.expect(TokenType["IF"])
        condition = self.parse_expression()
        self.expect(TokenType["COLON"])
        self.expect(TokenType["NEWLINE"])

        then_block = self.parse_block()

        # Check for else clause
        else_block = None
        if self.peek().type == TokenType["ELSE"]:
            self.advance()  # Consume 'else'
            self.expect(TokenType["COLON"])
            self.expect(TokenType["NEWLINE"])
            else_block = self.parse_block()

        return ASTNode(
            ASTNodeType.IF_STMT,
            condition=condition,
            then_block=then_block,
            else_block=else_block,
        )

    def parse_while_statement(self):
        """Parse a while statement"""
        self.expect(TokenType["WHILE"])
        condition = self.parse_expression()
        self.expect(TokenType["COLON"])
        self.expect(TokenType["NEWLINE"])

        body = self.parse_block()

        return ASTNode(ASTNodeType.WHILE_STMT, condition=condition, body=body)

    def parse_expression(self):
        """Parse an expression (currently only simple expressions)"""
        return self.parse_term()

    def parse_term(self):
        """Parse a term (addition/subtraction)"""
        left = self.parse_factor()

        while self.peek().type in (TokenType["PLUS"], TokenType["MINUS"]):
            operator = self.advance().value
            right = self.parse_factor()
            left = ASTNode(
                ASTNodeType["BINARY_EXPR"], left=left, operator=operator, right=right
            )

        return left

    def parse_factor(self):
        """Parse a factor (multiplication/division)"""
        left = self.parse_primary()

        while self.peek().type in (TokenType["MULTIPLY"], TokenType["DIVIDE"]):
            operator = self.advance().value
            right = self.parse_primary()
            left = ASTNode(
                ASTNodeType["BINARY_EXPR"], left=left, operator=operator, right=right
            )

        return left

    def parse_primary(self):
        """Parse a primary expression (literal, identifier, call, parenthesized)"""
        token = self.peek()

        if token.type == TokenType["NUMBER"]:
            self.advance()
            return ASTNode(ASTNodeType["NUMBER"], value=int(token.value))

        elif token.type == TokenType["STRING"]:
            self.advance()
            return ASTNode(ASTNodeType["STRING"], value=token.value)

        elif token.type == TokenType["IDENTIFIER"]:
            identifier = self.advance().value

            # Check if this is a function call
            if self.peek().type == TokenType["LPAREN"]:
                self.advance()  # Consume '('
                args = []

                if self.peek().type != TokenType["RPAREN"]:
                    args.append(self.parse_expression())

                    while self.peek().type == TokenType["COMMA"]:
                        self.advance()  # Consume ','
                        args.append(self.parse_expression())

                self.expect(TokenType["RPAREN"])
                return ASTNode(ASTNodeType["CALL_EXPR"], name=identifier, args=args)
            else:
                return ASTNode(ASTNodeType["IDENTIFIER"], name=identifier)

        elif token.type == TokenType["LPAREN"]:
            self.advance()  # Consume '('
            expr = self.parse_expression()
            self.expect(TokenType["RPAREN"])
            return expr

        else:
            raise SyntaxError(f"Unexpected token {token.type} at line {token.line}")


###############################################################################
############### INTERMEDIATE REPRESENTATION (IR) GENERATION ###############
###############################################################################

IROpType = {
    "LOAD_IMM": 0,
    "LOAD_VAR": 1,
    "STORE": 2,
    "ADD": 3,
    "SUB": 4,
    "MUL": 5,
    "DIV": 6,
    "JUMP": 7,
    "JUMP_IF_ZERO": 8,
    "JUMP_IF_NEG": 9,
    "LABEL": 10,
    "CALL": 11,
    "RETURN": 12,
    "ALLOCATE": 13,
    "DEALLOCATE": 14,
    "PRINT": 15,
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


class IRGenerator:
    def __init__(self):
        self.instructions = []
        self.label_counter = 0
        self.current_function = None
        self.var_offsets = {}  # Maps variable names to stack offsets
        self.next_offset = 0
        self.function_start_indices = {}
        self.string_literals = {}  # Maps string values to their labels
        self.string_counter = 0

    def add_string_literal(self, value: str) -> str:
        """Add a string literal and return its label"""
        if value in self.string_literals:
            return self.string_literals[value]

        label = f"str{self.string_counter}"
        self.string_counter += 1
        self.string_literals[value] = label
        return label

    def new_label(self) -> str:
        """Generate a new unique label"""
        label = f"L{self.label_counter}"
        self.label_counter += 1
        return label

    def allocate_var(self, name: str) -> int:
        """Allocate space for a variable and return its offset"""
        if name in self.var_offsets:
            return self.var_offsets[name]

        offset = self.next_offset
        self.next_offset += 8  # Use 8 bytes (64-bit) per variable for x86-64
        self.var_offsets[name] = offset
        return offset

    def generate(self, ast: ASTNode):
        """Generate IR from AST"""
        if ast.node_type == ASTNodeType["PROGRAM"]:
            for stmt in ast.statements:
                self.generate(stmt)

        elif ast.node_type == ASTNodeType["FUNCTION_DEF"]:
            # Save previous context
            prev_function = self.current_function
            prev_offsets = self.var_offsets
            prev_next_offset = self.next_offset

            # Set up new function context
            self.current_function = ast.name
            self.var_offsets = {}
            self.next_offset = 0

            function_start_idx = len(self.instructions)
            self.function_start_indices[ast.name] = function_start_idx

            # Add function label
            self.instructions.append(IRInstruction(IROpType["LABEL"], name=ast.name))

            param_offsets = {}
            for i, param in enumerate(ast.params):
                # For x86-64, first 6 parameters are in registers
                # Allocate stack space for them
                offset = self.allocate_var(param)
                param_offsets[param] = offset

            # Generate code for function body to discover all variables
            # (This is the first pass to determine total stack space needed)
            body_start_idx = len(self.instructions)
            self.generate(ast.body)

            # Calculate final stack size needed (next_offset contains it)
            stack_size = self.next_offset

            # Ensure stack is 16-byte aligned for x86-64 ABI
            if stack_size % 16 != 0:
                stack_size += 16 - (stack_size % 16)

            # Now insert function prologue at the beginning
            self.instructions.insert(
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
                    self.instructions.insert(
                        body_start_idx,  # Insert after ALLOCATE
                        IRInstruction(
                            IROpType["STORE"], dest=offset, src_reg=reg_mapping[i]
                        ),
                    )
                    body_start_idx += 1

            # Add implicit return if not present
            if len(self.instructions) == 0 or (
                self.instructions[-1].op_type != IROpType["RETURN"]
                and getattr(self.instructions[-1], "name", None)
                != self.current_function
            ):
                self.instructions.append(IRInstruction(IROpType["RETURN"]))

            # Add deallocate before return
            if stack_size > 0:
                return_index = len(self.instructions) - 1
                self.instructions.insert(
                    return_index,  # Insert before RETURN
                    IRInstruction(IROpType["DEALLOCATE"], size=stack_size),
                )

            # Restore previous context
            self.current_function = prev_function
            self.var_offsets = prev_offsets
            self.next_offset = prev_next_offset

        elif ast.node_type == ASTNodeType["BLOCK"]:
            for stmt in ast.statements:
                self.generate(stmt)

        elif ast.node_type == ASTNodeType["RETURN_STMT"]:
            if ast.expression:
                # Evaluate the return expression and leave result in r0
                self.generate_expr(ast.expression, 0)
            self.instructions.append(IRInstruction(IROpType["RETURN"]))

        elif ast.node_type == ASTNodeType["IF_STMT"]:
            else_label = self.new_label()
            end_label = self.new_label()

            # Generate condition code, result in r0
            self.generate_expr(ast.condition, 0)

            # Jump to else block if condition is false (0)
            self.instructions.append(
                IRInstruction(IROpType["JUMP_IF_ZERO"], dest=else_label, src_reg=0)
            )

            # Generate then block
            self.generate(ast.then_block)
            self.instructions.append(IRInstruction(IROpType["JUMP"], dest=end_label))

            # Generate else block if present
            self.instructions.append(IRInstruction(IROpType["LABEL"], name=else_label))
            if ast.else_block:
                self.generate(ast.else_block)

            self.instructions.append(IRInstruction(IROpType["LABEL"], name=end_label))

        elif ast.node_type == ASTNodeType["WHILE_STMT"]:
            start_label = self.new_label()
            end_label = self.new_label()

            # Generate loop start label
            self.instructions.append(IRInstruction(IROpType["LABEL"], name=start_label))

            # Generate condition code, result in r0
            self.generate_expr(ast.condition, 0)

            # Jump to end if condition is false (0)
            self.instructions.append(
                IRInstruction(IROpType["JUMP_IF_ZERO"], dest=end_label, src_reg=0)
            )

            # Generate loop body
            self.generate(ast.body)

            # Jump back to start
            self.instructions.append(IRInstruction(IROpType["JUMP"], dest=start_label))

            # End label
            self.instructions.append(IRInstruction(IROpType["LABEL"], name=end_label))

        elif ast.node_type == ASTNodeType["ASSIGNMENT"]:
            # Evaluate the expression and store result in the variable
            offset = self.allocate_var(ast.name)
            self.generate_expr(ast.value, 0)
            self.instructions.append(
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
            self.generate_expr(ast, 0)

        return self.instructions

    def generate_expr(self, expr: ASTNode, target_reg: int):
        """Generate code for an expression, leaving result in the target register"""
        if expr.node_type == ASTNodeType["NUMBER"]:
            self.instructions.append(
                IRInstruction(
                    IROpType["LOAD_IMM"], dest_reg=target_reg, value=expr.value
                )
            )

        elif expr.node_type == ASTNodeType["STRING"]:
            label = self.add_string_literal(expr.value)
            # For strings, we might want to load the address into a register
            # This would be handled differently in the code generator
            self.instructions.append(
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
            offset = self.var_offsets.get(expr.name)
            if offset is None:
                raise ValueError(f"Undefined variable: {expr.name}")
            self.instructions.append(
                IRInstruction(
                    IROpType["LOAD_VAR"], dest_reg=target_reg, src_offset=offset
                )
            )

        elif expr.node_type == ASTNodeType["BINARY_EXPR"]:
            # Generate code for left and right operands
            self.generate_expr(expr.left, target_reg)
            temp_reg = (target_reg + 1) % 4  # Use next register for right operand
            self.generate_expr(expr.right, temp_reg)

            # Generate operation
            if expr.operator == "+":
                self.instructions.append(
                    IRInstruction(
                        IROpType["ADD"], dest_reg=target_reg, src_reg=temp_reg
                    )
                )
            elif expr.operator == "-":
                self.instructions.append(
                    IRInstruction(
                        IROpType["SUB"], dest_reg=target_reg, src_reg=temp_reg
                    )
                )
            elif expr.operator == "*":
                self.instructions.append(
                    IRInstruction(
                        IROpType["MUL"], dest_reg=target_reg, src_reg=temp_reg
                    )
                )
            elif expr.operator == "/":
                self.instructions.append(
                    IRInstruction(
                        IROpType["DIV"], dest_reg=target_reg, src_reg=temp_reg
                    )
                )

        elif expr.node_type == ASTNodeType["CALL_EXPR"]:
            # Evaluate arguments and put in argument registers
            for i, arg in enumerate(expr.args):
                if i < 4:  # ARM uses r0-r3 for first 4 args
                    self.generate_expr(arg, i)
                else:
                    # Would need to push to stack for args > 4, simplifying for now
                    pass

            # Generate call instruction
            self.instructions.append(IRInstruction(IROpType["CALL"], name=expr.name))

            # Result is in r0, move to target register if needed
            if target_reg != 0:
                # This would be a MOV instruction in ARM
                # For simplicity, we'll use ADD with 0
                self.instructions.append(
                    IRInstruction(IROpType["ADD"], dest_reg=target_reg, src_reg=0)
                )


##### MACHINE CODE GENERATION #####
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
            self.add_string_literal(string_literal)

        if self.data_section:
            asm_lines.extend(["", ".section .data"])
            asm_lines.extend(self.data_section)

        asm_lines.append("")
        return "\n".join(asm_lines)


class MachineCodeGenerator:
    def __init__(self, architecture: str = "arm"):
        self.architecture = architecture.lower()
        self.string_literals = {}

    def generate(self, ir_instructions) -> str:
        """Generate machine code for the target architecture"""
        if self.architecture == "arm":
            gen = ARMGenerator()
            return gen.generate(ir_instructions)
        elif self.architecture == "x86_64":
            gen = X86_64Generator()
            return gen.generate(ir_instructions, self.string_literals)
        else:
            raise ValueError(f"Unsupported architecture: {self.architecture}")


def compile_code(source_code: str, architecture: str = "arm"):
    """Compile source code to target machine code"""
    # Stage 1: Lexing
    lexer = Lexer(source_code)
    tokens = lexer.tokenize()

    print("-----TOKENS-----")
    print(tokens)

    # Stage 2: Parsing
    parser = Parser(tokens)
    ast = parser.parse()

    print("-----AST-----")
    print(ast)

    # Stage 3: IR Generation
    ir_gen = IRGenerator()
    ir = ir_gen.generate(ast)

    print("-----IR-----")
    print(ir)

    # Stage 4: Target Code Generation
    code_gen = MachineCodeGenerator(architecture)
    code_gen.string_literals = ir_gen.string_literals
    target_code = code_gen.generate(ir)

    return target_code


def main():
    parser = argparse.ArgumentParser(prog="newlang")
    parser.add_argument("-t", "--target", choices=["arm", "x86_64"], default="arm")
    parser.add_argument("--debug", action="store_true", default=False)

    args = vars(parser.parse_args())

    source_code = ""

    with open("test/simple.pi") as fp:
        source_code = "".join(fp.readlines())

    # Compile to ARM assembly
    arm_code = compile_code(source_code, args["target"])
    print(arm_code)

    # write to file
    with open("simple.s", "+w") as fp:
        fp.write(arm_code)


if __name__ == "__main__":
    main()

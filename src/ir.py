from enum import Enum, auto
from typing import List, Dict, Union, Optional, Tuple
from src.parser import ASTNode, ASTNodeType


###############################################################################
# PART 3: INTERMEDIATE REPRESENTATION - Arch-independent instructions
###############################################################################


class IROpType(Enum):
    LOAD_IMM = auto()  # Load immediate value
    LOAD_VAR = auto()  # Load variable
    STORE = auto()  # Store to variable
    ADD = auto()  # Add
    SUB = auto()  # Subtract
    MUL = auto()  # Multiply
    DIV = auto()  # Divide
    JUMP = auto()  # Unconditional jump
    JUMP_IF_ZERO = auto()  # Jump if zero
    JUMP_IF_NEG = auto()  # Jump if negative
    LABEL = auto()  # Label definition
    CALL = auto()  # Function call
    RETURN = auto()  # Return from function
    ALLOCATE = auto()  # Allocate stack space
    DEALLOCATE = auto()  # Deallocate stack space


class IRInstruction:
    def __init__(self, op_type: IROpType, **kwargs):
        self.op_type = op_type
        for key, value in kwargs.items():
            setattr(self, key, value)

    def __repr__(self):
        attrs = ", ".join(
            f"{k}={v}" for k, v in self.__dict__.items() if k != "op_type"
        )
        return f"{self.op_type.name}({attrs})"


class IRGenerator:
    def __init__(self):
        self.instructions = []
        self.label_counter = 0
        self.current_function = None
        self.var_offsets = {}  # Maps variable names to stack offsets
        self.next_offset = 0

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
        self.next_offset += 4  # Assume 4 bytes per variable
        self.var_offsets[name] = offset
        return offset

    def generate(self, ast: ASTNode) -> List[IRInstruction]:
        """Generate IR from AST"""
        if ast.node_type == ASTNodeType.PROGRAM:
            for stmt in ast.statements:
                self.generate(stmt)

        elif ast.node_type == ASTNodeType.FUNCTION_DEF:
            # Save previous context
            prev_function = self.current_function
            prev_offsets = self.var_offsets
            prev_next_offset = self.next_offset

            # Set up new function context
            self.current_function = ast.name
            self.var_offsets = {}
            self.next_offset = 0

            # Add function label
            self.instructions.append(IRInstruction(IROpType.LABEL, name=ast.name))

            # Allocate space for parameters
            for i, param in enumerate(ast.params):
                offset = self.allocate_var(param)
                # Parameters are passed in registers r0-r3 for ARM
                if i < 4:
                    self.instructions.append(
                        IRInstruction(IROpType.STORE, dest=offset, src_reg=i)
                    )
                else:
                    # Would need to load from stack for params > 4, simplifying for now
                    pass

            # Calculate stack space needed
            stack_size = self.next_offset
            if stack_size > 0:
                self.instructions.append(
                    IRInstruction(IROpType.ALLOCATE, size=stack_size)
                )

            # Generate code for function body
            self.generate(ast.body)

            # Add implicit return if not present
            if (
                not self.instructions
                or self.instructions[-1].op_type != IROpType.RETURN
            ):
                self.instructions.append(IRInstruction(IROpType.RETURN))

            # Deallocate stack space
            if stack_size > 0:
                self.instructions[-1:-1] = [
                    IRInstruction(IROpType.DEALLOCATE, size=stack_size)
                ]

            # Restore previous context
            self.current_function = prev_function
            self.var_offsets = prev_offsets
            self.next_offset = prev_next_offset

        elif ast.node_type == ASTNodeType.BLOCK:
            for stmt in ast.statements:
                self.generate(stmt)

        elif ast.node_type == ASTNodeType.RETURN_STMT:
            if ast.expression:
                # Evaluate the return expression and leave result in r0
                self.generate_expr(ast.expression, 0)
            self.instructions.append(IRInstruction(IROpType.RETURN))

        elif ast.node_type == ASTNodeType.IF_STMT:
            else_label = self.new_label()
            end_label = self.new_label()

            # Generate condition code, result in r0
            self.generate_expr(ast.condition, 0)

            # Jump to else block if condition is false (0)
            self.instructions.append(
                IRInstruction(IROpType.JUMP_IF_ZERO, dest=else_label, src_reg=0)
            )

            # Generate then block
            self.generate(ast.then_block)
            self.instructions.append(IRInstruction(IROpType.JUMP, dest=end_label))

            # Generate else block if present
            self.instructions.append(IRInstruction(IROpType.LABEL, name=else_label))
            if ast.else_block:
                self.generate(ast.else_block)

            self.instructions.append(IRInstruction(IROpType.LABEL, name=end_label))

        elif ast.node_type == ASTNodeType.WHILE_STMT:
            start_label = self.new_label()
            end_label = self.new_label()

            # Generate loop start label
            self.instructions.append(IRInstruction(IROpType.LABEL, name=start_label))

            # Generate condition code, result in r0
            self.generate_expr(ast.condition, 0)

            # Jump to end if condition is false (0)
            self.instructions.append(
                IRInstruction(IROpType.JUMP_IF_ZERO, dest=end_label, src_reg=0)
            )

            # Generate loop body
            self.generate(ast.body)

            # Jump back to start
            self.instructions.append(IRInstruction(IROpType.JUMP, dest=start_label))

            # End label
            self.instructions.append(IRInstruction(IROpType.LABEL, name=end_label))

        elif ast.node_type == ASTNodeType.ASSIGNMENT:
            # Evaluate the expression and store result in the variable
            offset = self.allocate_var(ast.name)
            self.generate_expr(ast.value, 0)
            self.instructions.append(
                IRInstruction(IROpType.STORE, dest=offset, src_reg=0)
            )

        elif ast.node_type in (
            ASTNodeType.BINARY_EXPR,
            ASTNodeType.CALL_EXPR,
            ASTNodeType.IDENTIFIER,
            ASTNodeType.NUMBER,
            ASTNodeType.STRING,
        ):
            # For standalone expressions, evaluate and discard the result
            self.generate_expr(ast, 0)

        return self.instructions

    def generate_expr(self, expr: ASTNode, target_reg: int):
        """Generate code for an expression, leaving result in the target register"""
        if expr.node_type == ASTNodeType.NUMBER:
            self.instructions.append(
                IRInstruction(IROpType.LOAD_IMM, dest_reg=target_reg, value=expr.value)
            )

        elif expr.node_type == ASTNodeType.STRING:
            # String handling would require data segment, using simplified approach
            # In a real compiler, you'd store the string in a data section and load its address
            pass

        elif expr.node_type == ASTNodeType.IDENTIFIER:
            offset = self.var_offsets.get(expr.name)
            if offset is None:
                raise ValueError(f"Undefined variable: {expr.name}")
            self.instructions.append(
                IRInstruction(IROpType.LOAD_VAR, dest_reg=target_reg, src_offset=offset)
            )

        elif expr.node_type == ASTNodeType.BINARY_EXPR:
            # Generate code for left and right operands
            self.generate_expr(expr.left, target_reg)
            temp_reg = (target_reg + 1) % 4  # Use next register for right operand
            self.generate_expr(expr.right, temp_reg)

            # Generate operation
            if expr.operator == "+":
                self.instructions.append(
                    IRInstruction(IROpType.ADD, dest_reg=target_reg, src_reg=temp_reg)
                )
            elif expr.operator == "-":
                self.instructions.append(
                    IRInstruction(IROpType.SUB, dest_reg=target_reg, src_reg=temp_reg)
                )
            elif expr.operator == "*":
                self.instructions.append(
                    IRInstruction(IROpType.MUL, dest_reg=target_reg, src_reg=temp_reg)
                )
            elif expr.operator == "/":
                self.instructions.append(
                    IRInstruction(IROpType.DIV, dest_reg=target_reg, src_reg=temp_reg)
                )

        elif expr.node_type == ASTNodeType.CALL_EXPR:
            # Evaluate arguments and put in argument registers
            for i, arg in enumerate(expr.args):
                if i < 4:  # ARM uses r0-r3 for first 4 args
                    self.generate_expr(arg, i)
                else:
                    # Would need to push to stack for args > 4, simplifying for now
                    pass

            # Generate call instruction
            self.instructions.append(IRInstruction(IROpType.CALL, name=expr.name))

            # Result is in r0, move to target register if needed
            if target_reg != 0:
                # This would be a MOV instruction in ARM
                # For simplicity, we'll use ADD with 0
                self.instructions.append(
                    IRInstruction(IROpType.ADD, dest_reg=target_reg, src_reg=0)
                )

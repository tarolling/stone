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
        "%eax",
        "%esi",
        "%edx",
        "%ecx",
        "%edi",
        "%ebx",
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

from typing import List, Dict, Union, Optional, Tuple
from src.ir import IRInstruction, IROpType
from src.errors import IROpError


class X86_64Generator:
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

    def generate(self, ir_instructions: List[IRInstruction]) -> str:
        """Generate x84-64 assembly from IR instructions"""
        asm_lines = [f'\t.file\t"example.py"', "\t.text", "\t.globl\tmain"]

        # Process instructions
        for ir in ir_instructions:
            match ir.op_type:
                case IROpType.LABEL:
                    asm_lines.append(f"{ir.name}:")
                case IROpType.LOAD_IMM:
                    asm_lines.append(
                        f"\tmovl\t${ir.value}, {register_map['x86_64'][ir.dest_reg]}"
                    )
                case IROpType.LOAD_VAR:
                    asm_lines.append(
                        f"\tmovl\t-{ir.src_offset+4}(%rbp), {register_map['x86_64'][ir.dest_reg]}"
                    )
                case IROpType.STORE:
                    if hasattr(ir, "src_reg"):
                        asm_lines.append(
                            f"\tmovl\t{register_map['x86_64'][ir.src_reg]}, -{ir.dest+8}(%rbp)"
                        )
                    else:
                        # This would be a store immediate, which ARM doesn't support directly
                        asm_lines.append(f"\tmov\t%r4, ${ir.value}")
                        asm_lines.append(f"\tstr\t%r4, -{ir.dest+8}(%rbp)")
                case IROpType.ADD:
                    asm_lines.append(
                        f"\taddl\t{register_map['x86_64'][ir.src_reg]}, {register_map['x86_64'][ir.dest_reg]}"
                    )
                case IROpType.SUB:
                    asm_lines.append(
                        f"\tsubl\t{register_map['x86_64'][ir.src_reg]}, {register_map['x86_64'][ir.dest_reg]}"
                    )
                case IROpType.MUL:
                    asm_lines.append(
                        f"\timul\t{register_map['x86_64'][ir.src_reg]}, {register_map['x86_64'][ir.dest_reg]}"
                    )
                case IROpType.DIV:
                    asm_lines.append(
                        f"\tmovl\t{register_map['x86_64'][ir.dest_reg]}, %eax"
                    )
                    asm_lines.append("\tcdq")  # Sign-extend EAX into EDX:EAX
                    asm_lines.append(f"\tidivl\t{register_map['x86_64'][ir.src_reg]}")
                    if ir.dest_reg != 0:  # If result doesn't belong in EAX
                        asm_lines.append(
                            f"\tmovl\t%eax, {register_map['x86_64'][ir.dest_reg]}"
                        )
                case IROpType.JUMP:
                    asm_lines.append(f"\tjmp\t{ir.dest}")
                case IROpType.JUMP_IF_ZERO:
                    asm_lines.append(
                        f"\ttestl\t{register_map['x86_64'][ir.src_reg]}, {register_map['x86_64'][ir.src_reg]}"
                    )
                    asm_lines.append(f"\tje\t{ir.dest}")
                case IROpType.JUMP_IF_NEG:
                    asm_lines.append(
                        f"\ttestl\t{register_map['x86_64'][ir.src_reg]}, {register_map['x86_64'][ir.src_reg]}"
                    )
                    asm_lines.append(f"\tjl\t{ir.dest}")
                case IROpType.CALL:
                    asm_lines.append(f"\tcall\t{ir.name}")
                case IROpType.RETURN:
                    asm_lines.append(f"\tret")
                case IROpType.ALLOCATE:
                    # x86-64 function prologue
                    asm_lines.append("\tpushq\t%rbp")  # Save base pointer
                    asm_lines.append("\tmovq\t%rsp, %rbp")  # Set new frame pointer
                    if ir.size > 0:
                        asm_lines.append(
                            f"\tsubq\t${ir.size}, %rsp"
                        )  # Allocate stack space
                        # Optional: add stack canary for security
                        # asm_lines.append("\tmovq\t%fs:40, %rax")
                        # asm_lines.append("\tmovq\t%rax, -8(%rbp)")
                        # asm_lines.append("\txorl\t%eax, %eax")
                case IROpType.DEALLOCATE:
                    # x86 function epilogue: restore stack pointer and restore frame pointer
                    asm_lines.append(f"\tleave")
                    # leave just an alias for following lines
                    # asm_lines.append("\tmovq\t%rbp, %rsp")  # Restore stack pointer
                    # asm_lines.append("\tpopq\t%rbp")        # Restore old base pointer
                case _:
                    raise IROpError("This don't exist twin")

        asm_lines.append("")
        return "\n".join(asm_lines)

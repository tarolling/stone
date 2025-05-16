from typing import List, Dict, Union, Optional, Tuple
from src.ir import IRInstruction, IROpType
from src.errors import IROpError


###############################################################################
# PART 4: ARM CODE GENERATOR - Convert IR to ARM assembly
###############################################################################


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

    def generate(self, ir_instructions: List[IRInstruction]) -> str:
        """Generate ARM assembly from IR instructions"""
        asm_lines = [".syntax unified", ".arch armv7-a", ".global main", ""]

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
                    raise IROpError("This don't exist twin")

        # Add data section if needed
        if self.data_section:
            asm_lines.append("")
            asm_lines.append(".data")
            asm_lines.extend(self.data_section)

        return "\n".join(asm_lines)

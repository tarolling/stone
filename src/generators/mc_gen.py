from enum import Enum, auto
from typing import List, Dict, Union, Optional, Tuple
from src.ir import IRInstruction
from src.generators.arm import ARMGenerator
from src.generators.x86_64 import X86_64Generator

###############################################################################
# PART 5: ABSTRACT MACHINE CODE GENERATOR - Architecture-neutral interface
###############################################################################


class MachineCodeGenerator:
    def __init__(self, architecture: str = "arm"):
        self.architecture = architecture.lower()

    def generate(self, ir_instructions: List[IRInstruction]) -> str:
        """Generate machine code for the target architecture"""
        if self.architecture == "arm":
            gen = ARMGenerator()
            return gen.generate(ir_instructions)
        elif self.architecture == "x86_64":
            gen = X86_64Generator()
            return gen.generate(ir_instructions)
        else:
            raise ValueError(f"Unsupported architecture: {self.architecture}")

//! Generates x86-64 and arm64 assembly for any text that parses, then assembles and links it
//! with the built-in assembler and linker, all in memory. Code generation must succeed or
//! return an error, never panic, and whatever assembly it returns must assemble and link.

#![no_main]

use libfuzzer_sys::fuzz_target;
use stone::codegen::{Architecture, asm, elf};

fuzz_target!(|source: &str| {
    if let Ok(module) = stone::driver::parse(source) {
        for arch in Architecture::ALL {
            if let Ok(text) = arch.generator().assemble(&module) {
                let linked = asm::assemble(&text, arch).and_then(|object| elf::link(&object, arch));
                if let Err(error) = linked {
                    panic!("{arch} assembly failed to assemble: {error}\n{text}");
                }
            }
        }
    }
});

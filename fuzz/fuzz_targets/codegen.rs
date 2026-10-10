//! Generates assembly for every target for any text that parses, then assembles and links it
//! with the built-in assembler and linkers, all in memory. Code generation must succeed or
//! return an error, never panic, and whatever assembly it returns must assemble and link.

#![no_main]

use libfuzzer_sys::fuzz_target;
use stone::codegen::{Target, executable};

fuzz_target!(|source: &str| {
    if let Ok(module) = stone::driver::parse(source) {
        for target in Target::ALL {
            if let Ok(text) = target.generator().assemble(&module) {
                if let Err(error) = executable(&text, target, "out") {
                    panic!("{target} assembly failed to assemble: {error}\n{text}");
                }
            }
        }
    }
});

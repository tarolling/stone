//! Feeds grammar-generated programs, which reach deeper than raw bytes usually do, through the
//! interpreter, and through loading (linking and checking), the code generator of every target,
//! and the built-in assembler and linkers. The interpreter may stop with an error, but every
//! generated program must build.

#![no_main]

use libfuzzer_sys::arbitrary::Unstructured;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let Ok(source) = stone_fuzz::generate(&mut Unstructured::new(data)) else {
        return;
    };
    let _ = stone_fuzz::interpret(&source);
    // linked like `stone build` links it, so `use math` resolves
    if let Err(e) = stone_fuzz::build_every_target(&source) {
        panic!("generated program {e}:\n{source}");
    }
});

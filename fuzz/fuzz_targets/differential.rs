//! Runs grammar-generated programs through both `stone run` and `stone build` and checks that
//! they print the same thing. Slow, since every input invokes gcc.

#![no_main]

use libfuzzer_sys::arbitrary::Unstructured;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = stone_fuzz::generate(&mut Unstructured::new(data)) {
        stone_fuzz::differential(&source);
    }
});

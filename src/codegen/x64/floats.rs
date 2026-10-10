//! The x86-64 runtime's float text routines, which write a float the way `stdlib::format_float`
//! does and read one the way `stdlib::parse_float` does, without libc's `snprintf` or `strtod`.
//!
//! Both work exactly, on a small library of bignums: unsigned integers of up to [`BIG_LIMBS`]
//! 64-bit limbs, each stored in `.bss` as its length followed by its limbs, lowest first, with
//! no zero limbs on top. For example, 2^64 + 5 is `[2, 5, 1]`, and 0 is `[0]`.

use super::builtins::jump_if_space;
use crate::codegen::AssemblyGenerator;
use crate::codegen::context::{BIG_LIMBS, EXPONENT_LIMIT, MAX_DIGITS};

/// The bignums the float routines work in. Formatting uses all of them, and reading a number
/// uses `stone.big_mant` for its dividend and `stone.big_scale` for its divisor.
const BIGS: [&str; 8] = [
    "stone.big_mant",
    "stone.big_minus",
    "stone.big_plus",
    "stone.big_scale",
    "stone.big_scale2",
    "stone.big_scale4",
    "stone.big_scale8",
    "stone.big_sum",
];

/// Emits code that points `reg` at the bignum `big`.
fn big(r#gen: &mut dyn AssemblyGenerator, reg: &str, big: &str) {
    r#gen.emit(&format!("\tlea\t{reg}, [rip + {big}]"));
}

/// Emits the bignum library and the tables of powers of ten the float routines share. The
/// routines take bignums in `rdi` and `rsi` and clobber `rax`, `rcx`, `rdx`, `rsi`, `rdi`, and
/// `r8` to `r10`, but never `r11`.
///
/// - `stone.big_set` sets `rdi` to the 64-bit value `rsi`
/// - `stone.big_copy` sets `rdi` to `rsi`
/// - `stone.big_mul_add` sets `rdi` to `rdi * rsi + rdx`, where `rsi` is not 0
/// - `stone.big_mul_pow10` multiplies `rdi` by 10 to the power `rsi`
/// - `stone.big_shl` multiplies `rdi` by 2 to the power `rsi`
/// - `stone.big_add` adds `rsi` to `rdi`
/// - `stone.big_sub` subtracts `rsi` from `rdi`, which must be at least as large
/// - `stone.big_cmp` returns in `eax` -1, 0, or 1 as `rdi` is less than, equal to, or greater
///   than `rsi`
/// - `stone.big_bitlen` returns how many bits `rdi` needs, or 0 for 0
pub fn bignum_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit("\t.p2align\t3");
    r#gen.emit(".Lstone_pow10:");
    for power in 0..20 {
        r#gen.emit(&format!("\t.quad\t{:#x}", 10u64.pow(power)));
    }
    // every power of ten up to 1e22 is exactly a float
    r#gen.emit(".Lstone_pow10_float:");
    for power in 0..=22 {
        let value: f64 = format!("1e{power}").parse().unwrap();
        r#gen.emit(&format!("\t.quad\t{:#x}", value.to_bits()));
    }
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t3");
    for label in BIGS {
        r#gen.emit(&format!("{label}:"));
        r#gen.emit(&format!("\t.zero\t{}", 8 * (BIG_LIMBS + 1)));
    }
    r#gen.emit("\t.text");

    r#gen.emit("stone.big_set:");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\ttest\trsi, rsi");
    r#gen.emit("\tsetnz\tal");
    r#gen.emit("\tmov\tQWORD PTR [rdi], rax");
    r#gen.emit("\tmov\tQWORD PTR [rdi + 8], rsi");
    r#gen.emit("\tret");

    r#gen.emit("stone.big_copy:");
    r#gen.emit("\tmov\trcx, QWORD PTR [rsi]");
    r#gen.emit("\tinc\trcx"); // the length too
    r#gen.emit("\trep\tmovsq");
    r#gen.emit("\tret");

    // r8 holds the multiplier, r9 the carry, and r10 the length
    r#gen.emit("stone.big_mul_add:");
    r#gen.emit("\tmov\tr8, rsi");
    r#gen.emit("\tmov\tr9, rdx");
    r#gen.emit("\tmov\tr10, QWORD PTR [rdi]");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Lbig_mul_add_loop:");
    r#gen.emit("\tcmp\trcx, r10");
    r#gen.emit("\tje\t.Lbig_mul_add_end");
    r#gen.emit("\tmov\trax, QWORD PTR [rdi + 8 + rcx * 8]");
    r#gen.emit("\tmul\tr8");
    r#gen.emit("\tadd\trax, r9");
    r#gen.emit("\tadc\trdx, 0");
    r#gen.emit("\tmov\tQWORD PTR [rdi + 8 + rcx * 8], rax");
    r#gen.emit("\tmov\tr9, rdx");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tjmp\t.Lbig_mul_add_loop");
    // a carry out of the top becomes a new limb
    r#gen.emit(".Lbig_mul_add_end:");
    r#gen.emit("\ttest\tr9, r9");
    r#gen.emit("\tjz\t.Lbig_mul_add_done");
    r#gen.emit("\tmov\tQWORD PTR [rdi + 8 + r10 * 8], r9");
    r#gen.emit("\tinc\tr10");
    r#gen.emit("\tmov\tQWORD PTR [rdi], r10");
    r#gen.emit(".Lbig_mul_add_done:");
    r#gen.emit("\tret");

    // 10^19 is the largest power of ten in a limb, and r11 counts down what is left
    r#gen.emit("stone.big_mul_pow10:");
    r#gen.emit("\tmov\tr11, rsi");
    r#gen.emit(".Lbig_mul_pow10_loop:");
    r#gen.emit("\tcmp\tr11, 19");
    r#gen.emit("\tjb\t.Lbig_mul_pow10_last");
    r#gen.emit(&format!("\tmovabs\trsi, {:#x}", 10u64.pow(19)));
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tcall\tstone.big_mul_add");
    r#gen.emit("\tsub\tr11, 19");
    r#gen.emit("\tjmp\t.Lbig_mul_pow10_loop");
    r#gen.emit(".Lbig_mul_pow10_last:");
    r#gen.emit("\tlea\trax, [rip + .Lstone_pow10]");
    r#gen.emit("\tmov\trsi, QWORD PTR [rax + r11 * 8]");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tjmp\tstone.big_mul_add");

    // shifts by cl bits within the limbs from the top down, then moves them up rsi / 64 limbs,
    // with r8 holding the length
    r#gen.emit("stone.big_shl:");
    r#gen.emit("\tmov\tr8, QWORD PTR [rdi]");
    r#gen.emit("\ttest\tr8, r8");
    r#gen.emit("\tjz\t.Lbig_shl_done");
    r#gen.emit("\tmov\tecx, esi");
    r#gen.emit("\tand\tecx, 63");
    r#gen.emit("\tjz\t.Lbig_shl_limbs");
    // the top limb's high bits become a new limb, if any are set
    r#gen.emit("\tmov\trax, QWORD PTR [rdi + r8 * 8]");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tshld\trdx, rax, cl");
    r#gen.emit("\tmov\tQWORD PTR [rdi + 8 + r8 * 8], rdx");
    r#gen.emit("\tlea\tr9, [r8 - 1]");
    r#gen.emit(".Lbig_shl_bits:");
    r#gen.emit("\ttest\tr9, r9");
    r#gen.emit("\tjz\t.Lbig_shl_low");
    r#gen.emit("\tmov\trax, QWORD PTR [rdi + 8 + r9 * 8]");
    r#gen.emit("\tmov\tr10, QWORD PTR [rdi + r9 * 8]");
    r#gen.emit("\tshld\trax, r10, cl");
    r#gen.emit("\tmov\tQWORD PTR [rdi + 8 + r9 * 8], rax");
    r#gen.emit("\tdec\tr9");
    r#gen.emit("\tjmp\t.Lbig_shl_bits");
    r#gen.emit(".Lbig_shl_low:");
    r#gen.emit("\tshl\tQWORD PTR [rdi + 8], cl");
    r#gen.emit("\ttest\trdx, rdx");
    r#gen.emit("\tjz\t.Lbig_shl_limbs");
    r#gen.emit("\tinc\tr8");
    r#gen.emit(".Lbig_shl_limbs:");
    r#gen.emit("\tshr\trsi, 6");
    r#gen.emit("\tjz\t.Lbig_shl_store");
    r#gen.emit("\tmov\tr9, r8");
    r#gen.emit(".Lbig_shl_move:");
    r#gen.emit("\tdec\tr9");
    r#gen.emit("\tmov\trax, QWORD PTR [rdi + 8 + r9 * 8]");
    r#gen.emit("\tlea\tr10, [r9 + rsi]");
    r#gen.emit("\tmov\tQWORD PTR [rdi + 8 + r10 * 8], rax");
    r#gen.emit("\ttest\tr9, r9");
    r#gen.emit("\tjnz\t.Lbig_shl_move");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tmov\tr9, rsi");
    r#gen.emit(".Lbig_shl_zero:");
    r#gen.emit("\tdec\tr9");
    r#gen.emit("\tmov\tQWORD PTR [rdi + 8 + r9 * 8], rax");
    r#gen.emit("\tjnz\t.Lbig_shl_zero");
    r#gen.emit("\tadd\tr8, rsi");
    r#gen.emit(".Lbig_shl_store:");
    r#gen.emit("\tmov\tQWORD PTR [rdi], r8");
    r#gen.emit(".Lbig_shl_done:");
    r#gen.emit("\tret");

    // r8 holds the length of rdi and r9 of rsi; inc and dec leave the carry flag alone
    r#gen.emit("stone.big_add:");
    r#gen.emit("\tmov\tr8, QWORD PTR [rdi]");
    r#gen.emit("\tmov\tr9, QWORD PTR [rsi]");
    r#gen.emit("\tcmp\tr8, r9");
    r#gen.emit("\tjae\t.Lbig_add_sum");
    r#gen.emit(".Lbig_add_widen:");
    r#gen.emit("\tmov\tQWORD PTR [rdi + 8 + r8 * 8], 0");
    r#gen.emit("\tinc\tr8");
    r#gen.emit("\tcmp\tr8, r9");
    r#gen.emit("\tjb\t.Lbig_add_widen");
    r#gen.emit(".Lbig_add_sum:");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tmov\tr10, r9");
    r#gen.emit("\ttest\tr10, r10"); // clears the carry
    r#gen.emit("\tjz\t.Lbig_add_carry");
    r#gen.emit(".Lbig_add_loop:");
    r#gen.emit("\tmov\trax, QWORD PTR [rsi + 8 + rcx * 8]");
    r#gen.emit("\tadc\tQWORD PTR [rdi + 8 + rcx * 8], rax");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tdec\tr10");
    r#gen.emit("\tjnz\t.Lbig_add_loop");
    r#gen.emit(".Lbig_add_carry:");
    r#gen.emit("\tjnc\t.Lbig_add_done");
    r#gen.emit(".Lbig_add_ripple:");
    r#gen.emit("\tcmp\trcx, r8");
    r#gen.emit("\tje\t.Lbig_add_extend");
    r#gen.emit("\tadd\tQWORD PTR [rdi + 8 + rcx * 8], 1");
    r#gen.emit("\tjnc\t.Lbig_add_done");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tjmp\t.Lbig_add_ripple");
    r#gen.emit(".Lbig_add_extend:");
    r#gen.emit("\tmov\tQWORD PTR [rdi + 8 + r8 * 8], 1");
    r#gen.emit("\tinc\tr8");
    r#gen.emit(".Lbig_add_done:");
    r#gen.emit("\tmov\tQWORD PTR [rdi], r8");
    r#gen.emit("\tret");

    // the borrow stops within rdi's limbs, and then any zero limbs on top are dropped
    r#gen.emit("stone.big_sub:");
    r#gen.emit("\tmov\tr8, QWORD PTR [rdi]");
    r#gen.emit("\tmov\tr10, QWORD PTR [rsi]");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\ttest\tr10, r10"); // clears the borrow
    r#gen.emit("\tjz\t.Lbig_sub_borrow");
    r#gen.emit(".Lbig_sub_loop:");
    r#gen.emit("\tmov\trax, QWORD PTR [rsi + 8 + rcx * 8]");
    r#gen.emit("\tsbb\tQWORD PTR [rdi + 8 + rcx * 8], rax");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tdec\tr10");
    r#gen.emit("\tjnz\t.Lbig_sub_loop");
    r#gen.emit(".Lbig_sub_borrow:");
    r#gen.emit("\tjnc\t.Lbig_sub_trim");
    r#gen.emit(".Lbig_sub_ripple:");
    r#gen.emit("\tsub\tQWORD PTR [rdi + 8 + rcx * 8], 1");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tjc\t.Lbig_sub_ripple");
    r#gen.emit(".Lbig_sub_trim:");
    r#gen.emit("\ttest\tr8, r8");
    r#gen.emit("\tjz\t.Lbig_sub_done");
    r#gen.emit("\tcmp\tQWORD PTR [rdi + r8 * 8], 0");
    r#gen.emit("\tjne\t.Lbig_sub_done");
    r#gen.emit("\tdec\tr8");
    r#gen.emit("\tjmp\t.Lbig_sub_trim");
    r#gen.emit(".Lbig_sub_done:");
    r#gen.emit("\tmov\tQWORD PTR [rdi], r8");
    r#gen.emit("\tret");

    // the longer is larger, and otherwise the first limb that differs from the top decides
    r#gen.emit("stone.big_cmp:");
    r#gen.emit("\tmov\trcx, QWORD PTR [rdi]");
    r#gen.emit("\tcmp\trcx, QWORD PTR [rsi]");
    r#gen.emit("\tjne\t.Lbig_cmp_differ");
    r#gen.emit(".Lbig_cmp_loop:");
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit("\tjz\t.Lbig_cmp_equal");
    r#gen.emit("\tmov\trax, QWORD PTR [rdi + rcx * 8]");
    r#gen.emit("\tcmp\trax, QWORD PTR [rsi + rcx * 8]");
    r#gen.emit("\tjne\t.Lbig_cmp_differ");
    r#gen.emit("\tdec\trcx");
    r#gen.emit("\tjmp\t.Lbig_cmp_loop");
    r#gen.emit(".Lbig_cmp_equal:");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tret");
    r#gen.emit(".Lbig_cmp_differ:");
    r#gen.emit("\tmov\teax, 1");
    r#gen.emit("\tja\t.Lbig_cmp_done");
    r#gen.emit("\tmov\teax, -1");
    r#gen.emit(".Lbig_cmp_done:");
    r#gen.emit("\tret");

    r#gen.emit("stone.big_bitlen:");
    r#gen.emit("\tmov\trcx, QWORD PTR [rdi]");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit("\tjz\t.Lbig_bitlen_done");
    r#gen.emit("\tbsr\trax, QWORD PTR [rdi + rcx * 8]");
    r#gen.emit("\tinc\trax");
    r#gen.emit("\tdec\trcx");
    r#gen.emit("\tshl\trcx, 6");
    r#gen.emit("\tadd\trax, rcx");
    r#gen.emit(".Lbig_bitlen_done:");
    r#gen.emit("\tret");
}

/// Emits `stone.format_float`, which writes the float whose bits are in `rdi` into the 64-byte
/// buffer at `rsi` the way `stdlib::format_float` formats it, such as `1.0`, `0.1`, `1e+16`, or
/// `nan`, plus `stone.print_float`, which prints it. They need [`bignum_runtime`], and
/// `stone.print_float` needs `print`'s routines.
///
/// `format_float` takes as many digits as the shortest text that reads back as the same float,
/// then writes the float's exact value rounded half to even to that many digits. The count
/// comes from the same steps as Rust's `flt2dec::strategy::dragon::format_shortest`: with `v`
/// as `mant / scale` times a power of ten, and its neighbors' midpoints `minus` below and
/// `plus` above it in the same units, it takes digits until the ones so far are within those
/// bounds, which count when the float's mantissa is even. Since every digit but the last is
/// already exact, the remainder then decides how the last one rounds.
pub fn float_runtime(r#gen: &mut dyn AssemblyGenerator) {
    let [mant, minus, plus, scale, scale2, scale4, scale8, sum] = BIGS;

    // rbx holds the binary exponent and then which way the digits stopped, r12 where the text
    // goes, r13 the decimal exponent, r14 how many digits there are, and r15 1 if the bounds
    // count and 0 if not; the digits go at [rbp - 80]
    let saved = ["rbx", "r12", "r13", "r14", "r15"];
    r#gen.emit("stone.format_float:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in saved {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tsub\trsp, 40");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tbtr\trax, 63");
    r#gen.emit("\tmovabs\trcx, 0x7ff0000000000000");
    r#gen.emit("\tcmp\trax, rcx");
    r#gen.emit("\tjbe\t.Lformat_float_number");
    r#gen.emit("\tmov\tDWORD PTR [r12], 0x6e616e"); // "nan", whatever its sign
    r#gen.emit("\tjmp\t.Lformat_float_done");
    r#gen.emit(".Lformat_float_number:");
    r#gen.emit("\ttest\trdi, rdi");
    r#gen.emit("\tjns\t.Lformat_float_positive");
    r#gen.emit("\tmov\tbyte ptr [r12], 45"); // '-'
    r#gen.emit("\tinc\tr12");
    r#gen.emit(".Lformat_float_positive:");
    r#gen.emit("\tcmp\trax, rcx");
    r#gen.emit("\tjne\t.Lformat_float_finite");
    r#gen.emit("\tmov\tDWORD PTR [r12], 0x666e69"); // "inf"
    r#gen.emit("\tjmp\t.Lformat_float_done");
    r#gen.emit(".Lformat_float_finite:");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjnz\t.Lformat_float_nonzero");
    r#gen.emit("\tmov\tDWORD PTR [r12], 0x302e30"); // "0.0"
    r#gen.emit("\tjmp\t.Lformat_float_done");

    // decoded as Rust's flt2dec::decode does: v = mant * 2^rbx, with plus in r8 and minus 1
    r#gen.emit(".Lformat_float_nonzero:");
    r#gen.emit("\tmov\trdx, rax");
    r#gen.emit("\tshl\trdx, 12");
    r#gen.emit("\tshr\trdx, 12"); // the fraction
    r#gen.emit("\tshr\trax, 52"); // the biased exponent
    r#gen.emit("\tmov\tr8d, 1");
    r#gen.emit("\tmov\tr15d, 1");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjnz\t.Lformat_float_normal");
    // a subnormal's mantissa is its fraction doubled, which is always even
    r#gen.emit("\tlea\trsi, [rdx + rdx]");
    r#gen.emit("\tmov\trbx, -1075");
    r#gen.emit("\tjmp\t.Lformat_float_decoded");
    r#gen.emit(".Lformat_float_normal:");
    r#gen.emit("\tlea\trbx, [rax - 1075]");
    r#gen.emit("\tmov\trsi, rdx");
    r#gen.emit("\tbts\trsi, 52");
    r#gen.emit("\tmov\teax, esi");
    r#gen.emit("\tnot\teax");
    r#gen.emit("\tand\teax, 1");
    r#gen.emit("\tmov\tr15d, eax");
    r#gen.emit("\ttest\trdx, rdx");
    r#gen.emit("\tjnz\t.Lformat_float_symmetric");
    // the gap below a power of two is half the gap above it
    r#gen.emit("\tshl\trsi, 2");
    r#gen.emit("\tsub\trbx, 2");
    r#gen.emit("\tmov\tr8d, 2");
    r#gen.emit("\tjmp\t.Lformat_float_decoded");
    r#gen.emit(".Lformat_float_symmetric:");
    r#gen.emit("\tadd\trsi, rsi");
    r#gen.emit("\tdec\trbx");
    r#gen.emit(".Lformat_float_decoded:");
    // estimates the decimal exponent k from mant + plus as Rust's estimate_scaling_factor does,
    // which is never too high and at most one too low
    r#gen.emit("\tlea\trax, [rsi + r8 - 1]");
    r#gen.emit("\tbsr\trax, rax");
    r#gen.emit("\tinc\trax");
    r#gen.emit("\tadd\trax, rbx");
    r#gen.emit("\timul\trax, rax, 1292913986"); // floor(2^32 * log10(2))
    r#gen.emit("\tsar\trax, 32");
    r#gen.emit("\tmov\tr13, rax");
    big(r#gen, "rdi", mant);
    r#gen.emit("\tcall\tstone.big_set");
    big(r#gen, "rdi", plus);
    r#gen.emit("\tmov\trsi, r8");
    r#gen.emit("\tcall\tstone.big_set");
    for one in [minus, scale] {
        big(r#gen, "rdi", one);
        r#gen.emit("\tmov\tesi, 1");
        r#gen.emit("\tcall\tstone.big_set");
    }
    // then v = mant / scale, as whole numbers
    r#gen.emit("\ttest\trbx, rbx");
    r#gen.emit("\tjs\t.Lformat_float_fraction");
    for each in [mant, minus, plus] {
        big(r#gen, "rdi", each);
        r#gen.emit("\tmov\trsi, rbx");
        r#gen.emit("\tcall\tstone.big_shl");
    }
    r#gen.emit("\tjmp\t.Lformat_float_powers");
    r#gen.emit(".Lformat_float_fraction:");
    big(r#gen, "rdi", scale);
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit("\tneg\trsi");
    r#gen.emit("\tcall\tstone.big_shl");
    // and v / 10^k = mant / scale
    r#gen.emit(".Lformat_float_powers:");
    r#gen.emit("\ttest\tr13, r13");
    r#gen.emit("\tjs\t.Lformat_float_small");
    big(r#gen, "rdi", scale);
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit("\tcall\tstone.big_mul_pow10");
    r#gen.emit("\tjmp\t.Lformat_float_estimated");
    r#gen.emit(".Lformat_float_small:");
    for each in [mant, minus, plus] {
        big(r#gen, "rdi", each);
        r#gen.emit("\tmov\trsi, r13");
        r#gen.emit("\tneg\trsi");
        r#gen.emit("\tcall\tstone.big_mul_pow10");
    }
    // if mant + plus passes scale, k was one too low, and otherwise the first digit is the
    // next one down
    r#gen.emit(".Lformat_float_estimated:");
    r#gen.emit("\tcall\t.Lformat_float_high");
    r#gen.emit("\tcmp\teax, r15d");
    r#gen.emit("\tjge\t.Lformat_float_times_ten");
    r#gen.emit("\tinc\tr13");
    r#gen.emit("\tjmp\t.Lformat_float_start");
    r#gen.emit(".Lformat_float_times_ten:");
    r#gen.emit("\tcall\t.Lformat_float_next");
    r#gen.emit(".Lformat_float_start:");
    for (double, from) in [(scale2, scale), (scale4, scale2), (scale8, scale4)] {
        big(r#gen, "rdi", double);
        big(r#gen, "rsi", from);
        r#gen.emit("\tcall\tstone.big_copy");
        big(r#gen, "rdi", double);
        r#gen.emit("\tmov\tesi, 1");
        r#gen.emit("\tcall\tstone.big_shl");
    }
    r#gen.emit("\txor\tr14d, r14d");
    // each digit is floor(mant / scale), leaving the remainder in mant
    r#gen.emit(".Lformat_float_digit:");
    r#gen.emit("\tcall\t.Lformat_float_extract");
    r#gen.emit("\tadd\tal, 48"); // '0'
    r#gen.emit("\tmov\tbyte ptr [rbp - 80 + r14], al");
    r#gen.emit("\tinc\tr14");
    // bit 0 of rbx says the digits are within the lower bound, and bit 1 that rounding them
    // up is within the upper one
    big(r#gen, "rdi", mant);
    big(r#gen, "rsi", minus);
    r#gen.emit("\tcall\tstone.big_cmp");
    r#gen.emit("\tcmp\teax, r15d");
    r#gen.emit("\tsetl\tbl");
    r#gen.emit("\tmovzx\tebx, bl");
    r#gen.emit("\tcall\t.Lformat_float_high");
    r#gen.emit("\tcmp\teax, r15d");
    r#gen.emit("\tsetl\tal");
    r#gen.emit("\tmovzx\teax, al");
    r#gen.emit("\tlea\tebx, [rbx + rax * 2]");
    r#gen.emit("\ttest\tebx, ebx");
    r#gen.emit("\tjnz\t.Lformat_float_stop");
    r#gen.emit("\tcall\t.Lformat_float_next");
    r#gen.emit("\tjmp\t.Lformat_float_digit");
    // the shortest digits round up when only that is within bounds, or when both are and the
    // remainder is at least half; then 99...9 becomes 100...0, one digit longer
    r#gen.emit(".Lformat_float_stop:");
    r#gen.emit("\ttest\tebx, 2");
    r#gen.emit("\tjz\t.Lformat_float_exact");
    r#gen.emit("\ttest\tebx, 1");
    r#gen.emit("\tjz\t.Lformat_float_nines");
    r#gen.emit("\tcall\t.Lformat_float_twice");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjs\t.Lformat_float_exact");
    r#gen.emit(".Lformat_float_nines:");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Lformat_float_nine:");
    r#gen.emit("\tcmp\tbyte ptr [rbp - 80 + rcx], 57"); // '9'
    r#gen.emit("\tjne\t.Lformat_float_exact");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tcmp\trcx, r14");
    r#gen.emit("\tjb\t.Lformat_float_nine");
    r#gen.emit("\tcall\t.Lformat_float_next");
    r#gen.emit("\tcall\t.Lformat_float_extract");
    r#gen.emit("\tadd\tal, 48");
    r#gen.emit("\tmov\tbyte ptr [rbp - 80 + r14], al");
    r#gen.emit("\tinc\tr14");
    // the exact value rounds half to even at the last digit, which can carry through 9s
    r#gen.emit(".Lformat_float_exact:");
    r#gen.emit("\tcall\t.Lformat_float_twice");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjs\t.Lformat_float_layout");
    r#gen.emit("\tjnz\t.Lformat_float_carry");
    r#gen.emit("\ttest\tbyte ptr [rbp - 81 + r14], 1"); // odd, as '0' is even
    r#gen.emit("\tjz\t.Lformat_float_layout");
    r#gen.emit(".Lformat_float_carry:");
    r#gen.emit("\tmov\trcx, r14");
    r#gen.emit(".Lformat_float_carry_loop:");
    r#gen.emit("\tdec\trcx");
    r#gen.emit("\tjs\t.Lformat_float_overflow");
    r#gen.emit("\tcmp\tbyte ptr [rbp - 80 + rcx], 57");
    r#gen.emit("\tjne\t.Lformat_float_increment");
    r#gen.emit("\tmov\tbyte ptr [rbp - 80 + rcx], 48");
    r#gen.emit("\tjmp\t.Lformat_float_carry_loop");
    r#gen.emit(".Lformat_float_increment:");
    r#gen.emit("\tinc\tbyte ptr [rbp - 80 + rcx]");
    r#gen.emit("\tjmp\t.Lformat_float_layout");
    r#gen.emit(".Lformat_float_overflow:");
    r#gen.emit("\tmov\tbyte ptr [rbp - 80], 49"); // '1', then the 0s
    r#gen.emit("\tinc\tr13");

    // r13 becomes the exponent of the first digit, and rsi walks the digits
    r#gen.emit(".Lformat_float_layout:");
    r#gen.emit("\tdec\tr13");
    r#gen.emit("\tlea\trsi, [rbp - 80]");
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tcmp\tr13, -4");
    r#gen.emit("\tjl\t.Lformat_float_scientific");
    r#gen.emit("\tcmp\tr13, 16");
    r#gen.emit("\tjge\t.Lformat_float_scientific");
    r#gen.emit("\ttest\tr13, r13");
    r#gen.emit("\tjns\t.Lformat_float_whole");
    // 0.000ddd, with -exponent - 1 zeros
    r#gen.emit("\tmov\tWORD PTR [rdi], 0x2e30"); // "0."
    r#gen.emit("\tadd\trdi, 2");
    r#gen.emit("\tmov\trcx, r13");
    r#gen.emit("\tnot\trcx");
    r#gen.emit("\tmov\tal, 48");
    r#gen.emit("\trep\tstosb");
    r#gen.emit("\tmov\trcx, r14");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tjmp\t.Lformat_float_end");
    // rdx digits go before the point
    r#gen.emit(".Lformat_float_whole:");
    r#gen.emit("\tlea\trdx, [r13 + 1]");
    r#gen.emit("\tcmp\tr14, rdx");
    r#gen.emit("\tjg\t.Lformat_float_point");
    r#gen.emit("\tmov\trcx, r14");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tmov\trcx, rdx");
    r#gen.emit("\tsub\trcx, r14");
    r#gen.emit("\tmov\tal, 48");
    r#gen.emit("\trep\tstosb");
    r#gen.emit("\tmov\tWORD PTR [rdi], 0x302e"); // ".0"
    r#gen.emit("\tadd\trdi, 2");
    r#gen.emit("\tjmp\t.Lformat_float_end");
    r#gen.emit(".Lformat_float_point:");
    r#gen.emit("\tmov\trcx, rdx");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tmov\tbyte ptr [rdi], 46"); // '.'
    r#gen.emit("\tinc\trdi");
    r#gen.emit("\tmov\trcx, r14");
    r#gen.emit("\tsub\trcx, rdx");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tjmp\t.Lformat_float_end");
    // d.ddde+XX, with at least two digits of exponent
    r#gen.emit(".Lformat_float_scientific:");
    r#gen.emit("\tmovsb");
    r#gen.emit("\tcmp\tr14, 1");
    r#gen.emit("\tje\t.Lformat_float_e");
    r#gen.emit("\tmov\tbyte ptr [rdi], 46");
    r#gen.emit("\tinc\trdi");
    r#gen.emit("\tlea\trcx, [r14 - 1]");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit(".Lformat_float_e:");
    r#gen.emit("\tmov\tWORD PTR [rdi], 0x2b65"); // "e+"
    r#gen.emit("\tmov\trax, r13");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjns\t.Lformat_float_exponent");
    r#gen.emit("\tmov\tbyte ptr [rdi + 1], 45"); // '-'
    r#gen.emit("\tneg\trax");
    r#gen.emit(".Lformat_float_exponent:");
    r#gen.emit("\tadd\trdi, 2");
    r#gen.emit("\tmov\tecx, 10");
    r#gen.emit("\tcmp\trax, 100");
    r#gen.emit("\tjb\t.Lformat_float_two_digits");
    r#gen.emit("\tmov\tecx, 100");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tdiv\trcx");
    r#gen.emit("\tadd\tal, 48");
    r#gen.emit("\tmov\tbyte ptr [rdi], al");
    r#gen.emit("\tinc\trdi");
    r#gen.emit("\tmov\trax, rdx");
    r#gen.emit("\tmov\tecx, 10");
    r#gen.emit(".Lformat_float_two_digits:");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tdiv\trcx");
    r#gen.emit("\tadd\tal, 48");
    r#gen.emit("\tadd\tdl, 48");
    r#gen.emit("\tmov\tbyte ptr [rdi], al");
    r#gen.emit("\tmov\tbyte ptr [rdi + 1], dl");
    r#gen.emit("\tadd\trdi, 2");
    r#gen.emit(".Lformat_float_end:");
    r#gen.emit("\tmov\tbyte ptr [rdi], 0");
    r#gen.emit(".Lformat_float_done:");
    r#gen.emit(&format!("\tlea\trsp, [rbp - {}]", 8 * saved.len()));
    for reg in saved.iter().rev() {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");

    // returns in eax how scale compares with mant + plus
    r#gen.emit(".Lformat_float_high:");
    big(r#gen, "rdi", sum);
    big(r#gen, "rsi", mant);
    r#gen.emit("\tcall\tstone.big_copy");
    big(r#gen, "rdi", sum);
    big(r#gen, "rsi", plus);
    r#gen.emit("\tcall\tstone.big_add");
    big(r#gen, "rdi", scale);
    big(r#gen, "rsi", sum);
    r#gen.emit("\tjmp\tstone.big_cmp");

    // returns in eax how 2 * mant compares with scale
    r#gen.emit(".Lformat_float_twice:");
    big(r#gen, "rdi", sum);
    big(r#gen, "rsi", mant);
    r#gen.emit("\tcall\tstone.big_copy");
    big(r#gen, "rdi", sum);
    r#gen.emit("\tmov\tesi, 1");
    r#gen.emit("\tcall\tstone.big_shl");
    big(r#gen, "rdi", sum);
    big(r#gen, "rsi", scale);
    r#gen.emit("\tjmp\tstone.big_cmp");

    // multiplies mant, minus, and plus by 10 for the next digit
    r#gen.emit(".Lformat_float_next:");
    for each in [mant, minus, plus] {
        big(r#gen, "rdi", each);
        r#gen.emit("\tmov\tesi, 10");
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit("\tcall\tstone.big_mul_add");
    }
    r#gen.emit("\tret");

    // returns in eax floor(mant / scale), which is below 16, taking 8, 4, 2, and 1 times scale
    // from mant where they fit, and counting them at [rsp]
    r#gen.emit(".Lformat_float_extract:");
    r#gen.emit("\tpush\t0");
    for (multiple, times) in [(scale8, 8), (scale4, 4), (scale2, 2), (scale, 1)] {
        let skip = format!(".Lformat_float_extract_{times}");
        big(r#gen, "rdi", mant);
        big(r#gen, "rsi", multiple);
        r#gen.emit("\tcall\tstone.big_cmp");
        r#gen.emit("\ttest\teax, eax");
        r#gen.emit(&format!("\tjs\t{skip}"));
        big(r#gen, "rdi", mant);
        big(r#gen, "rsi", multiple);
        r#gen.emit("\tcall\tstone.big_sub");
        r#gen.emit(&format!("\tadd\tQWORD PTR [rsp], {times}"));
        r#gen.emit(&format!("{skip}:"));
    }
    r#gen.emit("\tpop\trax");
    r#gen.emit("\tret");

    // the text goes in 64 bytes of stack
    r#gen.emit("stone.print_float:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tsub\trsp, 64");
    r#gen.emit("\tmov\trsi, rsp");
    r#gen.emit("\tcall\tstone.format_float");
    r#gen.emit("\tmov\trdi, rsp");
    r#gen.emit("\tcall\tstone.print_str");
    r#gen.emit("\tleave");
    r#gen.emit("\tret");
}

/// Emits `stone.decimal_to_float`, which returns the bits of the float nearest the number in the
/// string in `rdi`, which must already fit the grammar `stdlib::parse_float` accepts. Ties go to
/// the even float, as Rust's `parse` does. It needs [`bignum_runtime`].
///
/// A number of at most 15 digits, times a power of ten up to 22, is one exact multiplication or
/// division of floats, which rounds once. Any other is the ratio of two bignums, `N / S`: the
/// digits and the power of ten go in `N` or `S`, which are lined up so that `1 <= N / S < 2`,
/// then 54 bits of the quotient (fewer for a subnormal) are found by long division. The last
/// of them and whether anything remains decide the rounding.
pub fn decimal_runtime(r#gen: &mut dyn AssemblyGenerator) {
    let [mant, _, _, scale, ..] = BIGS;
    // rbx holds the cursor, r12 the number of digits kept, r13 the decimal exponent, r14 the
    // sign bit, and r15 the digits not yet in mant; [rbp - 48] counts those, [rbp - 56] is 1
    // after the point, and [rbp - 64] is not 0 once a digit past MAX_DIGITS is not 0
    let saved = ["rbx", "r12", "r13", "r14", "r15"];
    r#gen.emit("stone.decimal_to_float:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in saved {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tsub\trsp, 24");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit(".Ldecimal_lead:");
    r#gen.emit("\tmovzx\tecx, byte ptr [rbx]");
    r#gen.emit("\tinc\trbx");
    jump_if_space(r#gen, "ecx", ".Ldecimal_lead");
    r#gen.emit("\tdec\trbx");
    r#gen.emit("\txor\tr14d, r14d");
    r#gen.emit("\tcmp\tecx, 43"); // '+'
    r#gen.emit("\tje\t.Ldecimal_sign");
    r#gen.emit("\tcmp\tecx, 45"); // '-'
    r#gen.emit("\tjne\t.Ldecimal_named");
    r#gen.emit("\tbts\tr14, 63");
    r#gen.emit(".Ldecimal_sign:");
    r#gen.emit("\tinc\trbx");
    r#gen.emit("\tmovzx\tecx, byte ptr [rbx]");
    // the grammar allows only inf, infinity, and nan to start with a letter
    r#gen.emit(".Ldecimal_named:");
    r#gen.emit("\tor\tecx, 32");
    r#gen.emit("\tcmp\tecx, 105"); // 'i'
    r#gen.emit("\tje\t.Ldecimal_infinity");
    r#gen.emit("\tcmp\tecx, 110"); // 'n'
    r#gen.emit("\tje\t.Ldecimal_nan");
    big(r#gen, "rdi", mant);
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit("\tcall\tstone.big_set");
    r#gen.emit("\txor\tr12d, r12d");
    r#gen.emit("\txor\tr13d, r13d");
    r#gen.emit("\txor\tr15d, r15d");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 48], 0");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 56], 0");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 64], 0");
    r#gen.emit(".Ldecimal_digit:");
    r#gen.emit("\tmovzx\tecx, byte ptr [rbx]");
    r#gen.emit("\tcmp\tecx, 46"); // '.'
    r#gen.emit("\tjne\t.Ldecimal_not_point");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 56], 1");
    r#gen.emit("\tinc\trbx");
    r#gen.emit("\tjmp\t.Ldecimal_digit");
    r#gen.emit(".Ldecimal_not_point:");
    r#gen.emit("\tsub\tecx, 48"); // '0'
    r#gen.emit("\tcmp\tecx, 9");
    r#gen.emit("\tja\t.Ldecimal_digits_done");
    r#gen.emit("\tinc\trbx");
    // each digit after the point divides by 10, and 0s before the first other digit add nothing
    r#gen.emit("\tsub\tr13, QWORD PTR [rbp - 56]");
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tor\trax, rcx");
    r#gen.emit("\tjz\t.Ldecimal_digit");
    r#gen.emit(&format!("\tcmp\tr12, {MAX_DIGITS}"));
    r#gen.emit("\tjae\t.Ldecimal_drop");
    r#gen.emit("\tinc\tr12");
    r#gen.emit("\timul\tr15, r15, 10");
    r#gen.emit("\tadd\tr15, rcx");
    r#gen.emit("\tinc\tQWORD PTR [rbp - 48]");
    r#gen.emit("\tcmp\tQWORD PTR [rbp - 48], 19");
    r#gen.emit("\tjne\t.Ldecimal_digit");
    r#gen.emit("\tcall\t.Ldecimal_flush");
    r#gen.emit("\tjmp\t.Ldecimal_digit");
    // a dropped digit multiplies by 10 instead
    r#gen.emit(".Ldecimal_drop:");
    r#gen.emit("\tinc\tr13");
    r#gen.emit("\tor\tQWORD PTR [rbp - 64], rcx");
    r#gen.emit("\tjmp\t.Ldecimal_digit");
    r#gen.emit(".Ldecimal_digits_done:");
    r#gen.emit("\tcall\t.Ldecimal_flush");
    // dropped digits that are not all 0 make the number a little more than the kept ones
    r#gen.emit("\tcmp\tQWORD PTR [rbp - 64], 0");
    r#gen.emit("\tje\t.Ldecimal_exponent");
    big(r#gen, "rdi", mant);
    r#gen.emit("\tmov\tesi, 10");
    r#gen.emit("\tmov\tedx, 1");
    r#gen.emit("\tcall\tstone.big_mul_add");
    r#gen.emit("\tinc\tr12");
    r#gen.emit("\tdec\tr13");
    // r8 is 1 for a negative exponent, and rax holds it as it is read
    r#gen.emit(".Ldecimal_exponent:");
    r#gen.emit("\tmovzx\tecx, byte ptr [rbx]");
    r#gen.emit("\tor\tecx, 32");
    r#gen.emit("\tcmp\tecx, 101"); // 'e'
    r#gen.emit("\tjne\t.Ldecimal_value");
    r#gen.emit("\tinc\trbx");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\tmovzx\tecx, byte ptr [rbx]");
    r#gen.emit("\tcmp\tecx, 43"); // '+'
    r#gen.emit("\tje\t.Ldecimal_exponent_sign");
    r#gen.emit("\tcmp\tecx, 45"); // '-'
    r#gen.emit("\tjne\t.Ldecimal_exponent_digits");
    r#gen.emit("\tmov\tr8d, 1");
    r#gen.emit(".Ldecimal_exponent_sign:");
    r#gen.emit("\tinc\trbx");
    r#gen.emit(".Ldecimal_exponent_digits:");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit(".Ldecimal_exponent_digit:");
    r#gen.emit("\tmovzx\tecx, byte ptr [rbx]");
    r#gen.emit("\tsub\tecx, 48");
    r#gen.emit("\tcmp\tecx, 9");
    r#gen.emit("\tja\t.Ldecimal_exponent_end");
    r#gen.emit("\tinc\trbx");
    r#gen.emit("\timul\trax, rax, 10");
    r#gen.emit("\tadd\trax, rcx");
    r#gen.emit(&format!("\tcmp\trax, {EXPONENT_LIMIT}"));
    r#gen.emit("\tjbe\t.Ldecimal_exponent_digit");
    r#gen.emit(&format!("\tmov\teax, {EXPONENT_LIMIT}"));
    r#gen.emit("\tjmp\t.Ldecimal_exponent_digit");
    r#gen.emit(".Ldecimal_exponent_end:");
    r#gen.emit("\ttest\tr8d, r8d");
    r#gen.emit("\tjz\t.Ldecimal_add_exponent");
    r#gen.emit("\tneg\trax");
    r#gen.emit(".Ldecimal_add_exponent:");
    r#gen.emit("\tadd\tr13, rax");

    // the number is mant * 10^r13, with r12 digits
    r#gen.emit(".Ldecimal_value:");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjz\t.Ldecimal_zero");
    r#gen.emit("\tcmp\tr12, 15");
    r#gen.emit("\tja\t.Ldecimal_big");
    r#gen.emit("\tcmp\tr13, 22");
    r#gen.emit("\tjg\t.Ldecimal_big");
    r#gen.emit("\tcmp\tr13, -22");
    r#gen.emit("\tjl\t.Ldecimal_big");
    // both the digits and the power of ten are exact floats
    big(r#gen, "rcx", mant);
    r#gen.emit("\tcvtsi2sd\txmm0, QWORD PTR [rcx + 8]");
    r#gen.emit("\tlea\trcx, [rip + .Lstone_pow10_float]");
    r#gen.emit("\ttest\tr13, r13");
    r#gen.emit("\tjs\t.Ldecimal_divide");
    r#gen.emit("\tmulsd\txmm0, QWORD PTR [rcx + r13 * 8]");
    r#gen.emit("\tjmp\t.Ldecimal_rounded");
    r#gen.emit(".Ldecimal_divide:");
    r#gen.emit("\tneg\tr13");
    r#gen.emit("\tdivsd\txmm0, QWORD PTR [rcx + r13 * 8]");
    r#gen.emit(".Ldecimal_rounded:");
    r#gen.emit("\tmovq\trax, xmm0");
    r#gen.emit("\tor\trax, r14");
    r#gen.emit("\tjmp\t.Ldecimal_done");
    // past 10^310 is infinite, and below 10^-324 is less than half the smallest float
    r#gen.emit(".Ldecimal_big:");
    r#gen.emit("\tlea\trax, [r12 + r13]");
    r#gen.emit("\tcmp\trax, 310");
    r#gen.emit("\tjg\t.Ldecimal_infinity");
    r#gen.emit("\tcmp\trax, -324");
    r#gen.emit("\tjle\t.Ldecimal_zero");
    big(r#gen, "rdi", scale);
    r#gen.emit("\tmov\tesi, 1");
    r#gen.emit("\tcall\tstone.big_set");
    r#gen.emit("\ttest\tr13, r13");
    r#gen.emit("\tjs\t.Ldecimal_negative_exponent");
    big(r#gen, "rdi", mant);
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit("\tcall\tstone.big_mul_pow10");
    r#gen.emit("\tjmp\t.Ldecimal_ratio");
    r#gen.emit(".Ldecimal_negative_exponent:");
    big(r#gen, "rdi", scale);
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit("\tneg\trsi");
    r#gen.emit("\tcall\tstone.big_mul_pow10");
    // N = mant and S = scale get the same length, then N doubles if it is below S, so that
    // 1 <= N / S < 2 and the number is N / S * 2^r13
    r#gen.emit(".Ldecimal_ratio:");
    big(r#gen, "rdi", mant);
    r#gen.emit("\tcall\tstone.big_bitlen");
    r#gen.emit("\tmov\tr13, rax");
    big(r#gen, "rdi", scale);
    r#gen.emit("\tcall\tstone.big_bitlen");
    r#gen.emit("\tsub\tr13, rax");
    r#gen.emit("\tjle\t.Ldecimal_shift_dividend");
    big(r#gen, "rdi", scale);
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit("\tcall\tstone.big_shl");
    r#gen.emit("\tjmp\t.Ldecimal_lined_up");
    r#gen.emit(".Ldecimal_shift_dividend:");
    big(r#gen, "rdi", mant);
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit("\tneg\trsi");
    r#gen.emit("\tcall\tstone.big_shl");
    r#gen.emit(".Ldecimal_lined_up:");
    big(r#gen, "rdi", mant);
    big(r#gen, "rsi", scale);
    r#gen.emit("\tcall\tstone.big_cmp");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjns\t.Ldecimal_scaled");
    big(r#gen, "rdi", mant);
    r#gen.emit("\tmov\tesi, 1");
    r#gen.emit("\tcall\tstone.big_shl");
    r#gen.emit("\tdec\tr13");
    r#gen.emit(".Ldecimal_scaled:");
    r#gen.emit("\tcmp\tr13, 1023");
    r#gen.emit("\tjg\t.Ldecimal_infinity");
    // r12 counts the quotient's bits: 54 for a normal float, 53 places of significand and one
    // to round with, and fewer below 2^-1022, down to none below 2^-1075
    r#gen.emit("\tlea\tr12, [r13 + 1076]");
    r#gen.emit("\tmov\teax, 54");
    r#gen.emit("\tcmp\tr12, rax");
    r#gen.emit("\tcmovg\tr12, rax");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjle\t.Ldecimal_zero");
    r#gen.emit("\txor\tr15d, r15d");
    r#gen.emit(".Ldecimal_quotient:");
    r#gen.emit("\tadd\tr15, r15");
    big(r#gen, "rdi", mant);
    big(r#gen, "rsi", scale);
    r#gen.emit("\tcall\tstone.big_cmp");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjs\t.Ldecimal_doubled");
    big(r#gen, "rdi", mant);
    big(r#gen, "rsi", scale);
    r#gen.emit("\tcall\tstone.big_sub");
    r#gen.emit("\tinc\tr15");
    r#gen.emit(".Ldecimal_doubled:");
    big(r#gen, "rdi", mant);
    r#gen.emit("\tmov\tesi, 1");
    r#gen.emit("\tcall\tstone.big_shl");
    r#gen.emit("\tdec\tr12");
    r#gen.emit("\tjnz\t.Ldecimal_quotient");
    // the last bit is half a unit, which rounds up if anything remains or the rest is odd
    r#gen.emit("\tmov\trax, r15");
    r#gen.emit("\tshr\trax, 1");
    r#gen.emit("\ttest\tr15, 1");
    r#gen.emit("\tjz\t.Ldecimal_compose");
    big(r#gen, "rcx", mant);
    r#gen.emit("\tcmp\tQWORD PTR [rcx], 0");
    r#gen.emit("\tjne\t.Ldecimal_round_up");
    r#gen.emit("\ttest\trax, 1");
    r#gen.emit("\tjz\t.Ldecimal_compose");
    r#gen.emit(".Ldecimal_round_up:");
    r#gen.emit("\tinc\trax");
    // a normal float's significand has its leading bit, which carries into the exponent
    // field, so adding the exponent less one gives the bits, even when rounding carried
    r#gen.emit(".Ldecimal_compose:");
    r#gen.emit("\tlea\trcx, [r13 + 1022]");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit("\tcmovs\trcx, rdx");
    r#gen.emit("\tshl\trcx, 52");
    r#gen.emit("\tadd\trax, rcx");
    r#gen.emit("\tor\trax, r14");
    r#gen.emit("\tjmp\t.Ldecimal_done");
    r#gen.emit(".Ldecimal_infinity:");
    r#gen.emit("\tmovabs\trax, 0x7ff0000000000000");
    r#gen.emit("\tor\trax, r14");
    r#gen.emit("\tjmp\t.Ldecimal_done");
    r#gen.emit(".Ldecimal_nan:");
    r#gen.emit("\tmovabs\trax, 0x7ff8000000000000");
    r#gen.emit("\tor\trax, r14");
    r#gen.emit("\tjmp\t.Ldecimal_done");
    r#gen.emit(".Ldecimal_zero:");
    r#gen.emit("\tmov\trax, r14");
    r#gen.emit(".Ldecimal_done:");
    r#gen.emit(&format!("\tlea\trsp, [rbp - {}]", 8 * saved.len()));
    for reg in saved.iter().rev() {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");

    // mant = mant * 10^count + r15 for the count digits in r15, which start again from none
    r#gen.emit(".Ldecimal_flush:");
    big(r#gen, "rdi", mant);
    r#gen.emit("\tmov\trcx, QWORD PTR [rbp - 48]");
    r#gen.emit("\tlea\trax, [rip + .Lstone_pow10]");
    r#gen.emit("\tmov\trsi, QWORD PTR [rax + rcx * 8]");
    r#gen.emit("\tmov\trdx, r15");
    r#gen.emit("\tcall\tstone.big_mul_add");
    r#gen.emit("\txor\tr15d, r15d");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 48], 0");
    r#gen.emit("\tret");
}

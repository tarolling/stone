//! The arm64 runtime's float text routines, which mirror `x64/floats.rs` routine for routine:
//! a bignum library, `stone.format_float`, `stone.print_float`, and `stone.decimal_to_float`,
//! plus `stone.fmod`, which takes the place of libm's `fmod`.

use super::builtins::{copy_bytes, jump_if_space};
use super::{address, pop_frame, push_frame};
use crate::codegen::AssemblyGenerator;
use crate::codegen::context::{BIG_LIMBS, EXPONENT_LIMIT, MAX_DIGITS};

/// The bignums the float routines work in, as in `x64/floats.rs`.
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

/// Emits a `movz` and the `movk`s that put the 64-bit `value` in `reg`, skipping any 16-bit
/// piece that is 0 past the first.
///
/// For example, `move_wide(gen, "x14", 100_000)` emits `movz x14, #0x86a0` and
/// `movk x14, #0x1, lsl #16`.
fn move_wide(r#gen: &mut dyn AssemblyGenerator, reg: &str, value: u64) {
    r#gen.emit(&format!("\tmovz\t{reg}, #{:#x}", value & 0xffff));
    for shift in [16, 32, 48] {
        let piece = (value >> shift) & 0xffff;
        if piece != 0 {
            r#gen.emit(&format!("\tmovk\t{reg}, #{piece:#x}, lsl #{shift}"));
        }
    }
}

/// Emits the bignum library and the tables of powers of ten, under the labels and with the
/// contracts of x64's `bignum_runtime`, taking bignums in `x0` and `x1` and other arguments in
/// `x1` and `x2`. Every routine clobbers only `x0` to `x3`, `x9` to `x17`, and `x30`.
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
    r#gen.emit("\tcmp\tx1, #0");
    r#gen.emit("\tcset\tx9, ne");
    r#gen.emit("\tstp\tx9, x1, [x0]");
    r#gen.emit("\tret");

    // copies the length and then the limbs
    r#gen.emit("stone.big_copy:");
    r#gen.emit("\tldr\tx9, [x1]");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit("\tmov\tx10, #0");
    r#gen.emit(".Lbig_copy_loop:");
    r#gen.emit("\tldr\tx11, [x1, x10, lsl #3]");
    r#gen.emit("\tstr\tx11, [x0, x10, lsl #3]");
    r#gen.emit("\tadd\tx10, x10, #1");
    r#gen.emit("\tcmp\tx10, x9");
    r#gen.emit("\tb.lo\t.Lbig_copy_loop");
    r#gen.emit("\tret");

    // x9 holds the length, x10 the carry, x11 the limb, and x12 how many are left
    r#gen.emit("stone.big_mul_add:");
    r#gen.emit("\tldr\tx9, [x0]");
    r#gen.emit("\tmov\tx10, x2");
    r#gen.emit("\tadd\tx11, x0, #8");
    r#gen.emit("\tmov\tx12, x9");
    r#gen.emit(".Lbig_mul_add_loop:");
    r#gen.emit("\tcbz\tx12, .Lbig_mul_add_end");
    r#gen.emit("\tldr\tx13, [x11]");
    r#gen.emit("\tmul\tx14, x13, x1");
    r#gen.emit("\tumulh\tx15, x13, x1");
    r#gen.emit("\tadds\tx14, x14, x10");
    r#gen.emit("\tadc\tx10, x15, xzr");
    r#gen.emit("\tstr\tx14, [x11], #8");
    r#gen.emit("\tsub\tx12, x12, #1");
    r#gen.emit("\tb\t.Lbig_mul_add_loop");
    // a carry out of the top becomes a new limb
    r#gen.emit(".Lbig_mul_add_end:");
    r#gen.emit("\tcbz\tx10, .Lbig_mul_add_done");
    r#gen.emit("\tstr\tx10, [x11]");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit("\tstr\tx9, [x0]");
    r#gen.emit(".Lbig_mul_add_done:");
    r#gen.emit("\tret");

    // 10^19 is the largest power of ten in a limb, and x3 counts down what is left
    r#gen.emit("stone.big_mul_pow10:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    r#gen.emit("\tmov\tx29, sp");
    r#gen.emit("\tmov\tx3, x1");
    r#gen.emit(".Lbig_mul_pow10_loop:");
    r#gen.emit("\tcmp\tx3, #19");
    r#gen.emit("\tb.lo\t.Lbig_mul_pow10_last");
    move_wide(r#gen, "x1", 10u64.pow(19));
    r#gen.emit("\tmov\tx2, #0");
    r#gen.emit("\tbl\tstone.big_mul_add");
    r#gen.emit("\tsub\tx3, x3, #19");
    r#gen.emit("\tb\t.Lbig_mul_pow10_loop");
    r#gen.emit(".Lbig_mul_pow10_last:");
    address(r#gen, "x9", ".Lstone_pow10");
    r#gen.emit("\tldr\tx1, [x9, x3, lsl #3]");
    r#gen.emit("\tmov\tx2, #0");
    r#gen.emit("\tbl\tstone.big_mul_add");
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    r#gen.emit("\tret");

    // x9 holds the length, x10 the bits within a limb and x11 the rest of 64, x12 the limbs,
    // and x15 what the top limb shifts out
    r#gen.emit("stone.big_shl:");
    r#gen.emit("\tldr\tx9, [x0]");
    r#gen.emit("\tcbz\tx9, .Lbig_shl_done");
    r#gen.emit("\tadd\tx12, x0, #8");
    r#gen.emit("\tand\tx10, x1, #63");
    r#gen.emit("\tcbz\tx10, .Lbig_shl_limbs");
    r#gen.emit("\tmov\tx11, #64");
    r#gen.emit("\tsub\tx11, x11, x10");
    r#gen.emit("\tsub\tx13, x9, #1");
    r#gen.emit("\tldr\tx14, [x12, x13, lsl #3]");
    r#gen.emit("\tlsr\tx15, x14, x11");
    r#gen.emit("\tstr\tx15, [x12, x9, lsl #3]");
    r#gen.emit(".Lbig_shl_bits:");
    r#gen.emit("\tcbz\tx13, .Lbig_shl_low");
    r#gen.emit("\tldr\tx14, [x12, x13, lsl #3]");
    r#gen.emit("\tsub\tx16, x13, #1");
    r#gen.emit("\tldr\tx17, [x12, x16, lsl #3]");
    r#gen.emit("\tlsl\tx14, x14, x10");
    r#gen.emit("\tlsr\tx17, x17, x11");
    r#gen.emit("\torr\tx14, x14, x17");
    r#gen.emit("\tstr\tx14, [x12, x13, lsl #3]");
    r#gen.emit("\tmov\tx13, x16");
    r#gen.emit("\tb\t.Lbig_shl_bits");
    r#gen.emit(".Lbig_shl_low:");
    r#gen.emit("\tldr\tx14, [x12]");
    r#gen.emit("\tlsl\tx14, x14, x10");
    r#gen.emit("\tstr\tx14, [x12]");
    r#gen.emit("\tcbz\tx15, .Lbig_shl_limbs");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit(".Lbig_shl_limbs:");
    r#gen.emit("\tlsr\tx10, x1, #6");
    r#gen.emit("\tcbz\tx10, .Lbig_shl_store");
    r#gen.emit("\tmov\tx13, x9");
    r#gen.emit(".Lbig_shl_move:");
    r#gen.emit("\tsub\tx13, x13, #1");
    r#gen.emit("\tldr\tx14, [x12, x13, lsl #3]");
    r#gen.emit("\tadd\tx16, x13, x10");
    r#gen.emit("\tstr\tx14, [x12, x16, lsl #3]");
    r#gen.emit("\tcbnz\tx13, .Lbig_shl_move");
    r#gen.emit("\tmov\tx13, x10");
    r#gen.emit(".Lbig_shl_zero:");
    r#gen.emit("\tsub\tx13, x13, #1");
    r#gen.emit("\tstr\txzr, [x12, x13, lsl #3]");
    r#gen.emit("\tcbnz\tx13, .Lbig_shl_zero");
    r#gen.emit("\tadd\tx9, x9, x10");
    r#gen.emit(".Lbig_shl_store:");
    r#gen.emit("\tstr\tx9, [x0]");
    r#gen.emit(".Lbig_shl_done:");
    r#gen.emit("\tret");

    // x9 holds the length of x0 and x10 of x1, x11 and x12 their limbs, and x13 the index;
    // add, sub, str, and cbnz leave the carry flag alone
    r#gen.emit("stone.big_add:");
    r#gen.emit("\tldr\tx9, [x0]");
    r#gen.emit("\tldr\tx10, [x1]");
    r#gen.emit("\tadd\tx11, x0, #8");
    r#gen.emit("\tadd\tx12, x1, #8");
    r#gen.emit(".Lbig_add_widen:");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.hs\t.Lbig_add_sum");
    r#gen.emit("\tstr\txzr, [x11, x9, lsl #3]");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit("\tb\t.Lbig_add_widen");
    r#gen.emit(".Lbig_add_sum:");
    r#gen.emit("\tmov\tx13, #0");
    r#gen.emit("\tcmn\txzr, xzr"); // clears the carry
    r#gen.emit("\tcbz\tx10, .Lbig_add_carry");
    r#gen.emit(".Lbig_add_loop:");
    r#gen.emit("\tldr\tx14, [x11, x13, lsl #3]");
    r#gen.emit("\tldr\tx15, [x12, x13, lsl #3]");
    r#gen.emit("\tadcs\tx14, x14, x15");
    r#gen.emit("\tstr\tx14, [x11, x13, lsl #3]");
    r#gen.emit("\tadd\tx13, x13, #1");
    r#gen.emit("\tsub\tx10, x10, #1");
    r#gen.emit("\tcbnz\tx10, .Lbig_add_loop");
    r#gen.emit(".Lbig_add_carry:");
    r#gen.emit("\tb.cc\t.Lbig_add_done");
    r#gen.emit(".Lbig_add_ripple:");
    r#gen.emit("\tcmp\tx13, x9");
    r#gen.emit("\tb.eq\t.Lbig_add_extend");
    r#gen.emit("\tldr\tx14, [x11, x13, lsl #3]");
    r#gen.emit("\tadds\tx14, x14, #1");
    r#gen.emit("\tstr\tx14, [x11, x13, lsl #3]");
    r#gen.emit("\tb.cc\t.Lbig_add_done");
    r#gen.emit("\tadd\tx13, x13, #1");
    r#gen.emit("\tb\t.Lbig_add_ripple");
    r#gen.emit(".Lbig_add_extend:");
    r#gen.emit("\tmov\tx14, #1");
    r#gen.emit("\tstr\tx14, [x11, x9, lsl #3]");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit(".Lbig_add_done:");
    r#gen.emit("\tstr\tx9, [x0]");
    r#gen.emit("\tret");

    // the carry flag is set when nothing is borrowed, and the borrow stops within x0's limbs
    r#gen.emit("stone.big_sub:");
    r#gen.emit("\tldr\tx9, [x0]");
    r#gen.emit("\tldr\tx10, [x1]");
    r#gen.emit("\tadd\tx11, x0, #8");
    r#gen.emit("\tadd\tx12, x1, #8");
    r#gen.emit("\tmov\tx13, #0");
    r#gen.emit("\tcmp\txzr, xzr"); // sets the carry: no borrow
    r#gen.emit("\tcbz\tx10, .Lbig_sub_borrow");
    r#gen.emit(".Lbig_sub_loop:");
    r#gen.emit("\tldr\tx14, [x11, x13, lsl #3]");
    r#gen.emit("\tldr\tx15, [x12, x13, lsl #3]");
    r#gen.emit("\tsbcs\tx14, x14, x15");
    r#gen.emit("\tstr\tx14, [x11, x13, lsl #3]");
    r#gen.emit("\tadd\tx13, x13, #1");
    r#gen.emit("\tsub\tx10, x10, #1");
    r#gen.emit("\tcbnz\tx10, .Lbig_sub_loop");
    r#gen.emit(".Lbig_sub_borrow:");
    r#gen.emit("\tb.cs\t.Lbig_sub_trim");
    r#gen.emit(".Lbig_sub_ripple:");
    r#gen.emit("\tldr\tx14, [x11, x13, lsl #3]");
    r#gen.emit("\tsubs\tx14, x14, #1");
    r#gen.emit("\tstr\tx14, [x11, x13, lsl #3]");
    r#gen.emit("\tadd\tx13, x13, #1");
    r#gen.emit("\tb.cc\t.Lbig_sub_ripple");
    r#gen.emit(".Lbig_sub_trim:");
    r#gen.emit("\tcbz\tx9, .Lbig_sub_done");
    r#gen.emit("\tsub\tx13, x9, #1");
    r#gen.emit("\tldr\tx14, [x11, x13, lsl #3]");
    r#gen.emit("\tcbnz\tx14, .Lbig_sub_done");
    r#gen.emit("\tmov\tx9, x13");
    r#gen.emit("\tb\t.Lbig_sub_trim");
    r#gen.emit(".Lbig_sub_done:");
    r#gen.emit("\tstr\tx9, [x0]");
    r#gen.emit("\tret");

    // the longer is larger, and otherwise the first limb that differs from the top decides
    r#gen.emit("stone.big_cmp:");
    r#gen.emit("\tldr\tx9, [x0]");
    r#gen.emit("\tldr\tx10, [x1]");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.ne\t.Lbig_cmp_differ");
    r#gen.emit("\tadd\tx11, x0, #8");
    r#gen.emit("\tadd\tx12, x1, #8");
    r#gen.emit(".Lbig_cmp_loop:");
    r#gen.emit("\tcbz\tx9, .Lbig_cmp_equal");
    r#gen.emit("\tsub\tx9, x9, #1");
    r#gen.emit("\tldr\tx13, [x11, x9, lsl #3]");
    r#gen.emit("\tldr\tx14, [x12, x9, lsl #3]");
    r#gen.emit("\tcmp\tx13, x14");
    r#gen.emit("\tb.eq\t.Lbig_cmp_loop");
    r#gen.emit(".Lbig_cmp_differ:");
    r#gen.emit("\tmov\tx0, #1");
    r#gen.emit("\tb.hi\t.Lbig_cmp_done");
    r#gen.emit("\tmov\tx0, #-1");
    r#gen.emit(".Lbig_cmp_done:");
    r#gen.emit("\tret");
    r#gen.emit(".Lbig_cmp_equal:");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tret");

    // the top limb is at [x0 + 8 * length]
    r#gen.emit("stone.big_bitlen:");
    r#gen.emit("\tldr\tx9, [x0]");
    r#gen.emit("\tcbz\tx9, .Lbig_bitlen_zero");
    r#gen.emit("\tldr\tx10, [x0, x9, lsl #3]");
    r#gen.emit("\tclz\tx10, x10");
    r#gen.emit("\tlsl\tx9, x9, #6");
    r#gen.emit("\tsub\tx0, x9, x10");
    r#gen.emit("\tret");
    r#gen.emit(".Lbig_bitlen_zero:");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tret");
}

/// Emits code that calls the bignum routine `routine` on the bignums `first` and `second`.
fn call2(r#gen: &mut dyn AssemblyGenerator, routine: &str, first: &str, second: &str) {
    address(r#gen, "x0", first);
    address(r#gen, "x1", second);
    r#gen.emit(&format!("\tbl\t{routine}"));
}

/// Emits code that calls the bignum routine `routine` on the bignum `big` and the number in
/// `value`, a register or an immediate such as `#1`.
fn call_with(r#gen: &mut dyn AssemblyGenerator, routine: &str, big: &str, value: &str) {
    address(r#gen, "x0", big);
    r#gen.emit(&format!("\tmov\tx1, {value}"));
    r#gen.emit(&format!("\tbl\t{routine}"));
}

/// Emits `stone.format_float`, which writes the float whose bits are in `x0` into the 64-byte
/// buffer at `x1`, and `stone.print_float`, which prints the float in `x0`, the same way as
/// x64's `float_runtime`.
pub fn float_runtime(r#gen: &mut dyn AssemblyGenerator) {
    let [mant, minus, plus, scale, scale2, scale4, scale8, sum] = BIGS;

    // x19 holds the binary exponent and then which way the digits stopped, x20 where the text
    // goes, x21 the decimal exponent, x22 how many digits there are, x23 1 if the bounds count
    // and 0 if not, and x24 the digits, in 32 bytes of stack
    let saved = ["x19", "x20", "x21", "x22", "x23", "x24"];
    r#gen.emit("stone.format_float:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tsub\tsp, sp, #32");
    r#gen.emit("\tmov\tx24, sp");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tand\tx9, x0, #0x7fffffffffffffff");
    r#gen.emit("\tmovz\tx10, #0x7ff0, lsl #48");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.ls\t.Lformat_float_number");
    move_wide(r#gen, "x11", 0x6e616e); // "nan", whatever its sign
    r#gen.emit("\tstr\tw11, [x20]");
    r#gen.emit("\tb\t.Lformat_float_done");
    r#gen.emit(".Lformat_float_number:");
    r#gen.emit("\ttbz\tx0, #63, .Lformat_float_positive");
    r#gen.emit("\tmov\tw11, #45"); // '-'
    r#gen.emit("\tstrb\tw11, [x20], #1");
    r#gen.emit(".Lformat_float_positive:");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.ne\t.Lformat_float_finite");
    move_wide(r#gen, "x11", 0x666e69); // "inf"
    r#gen.emit("\tstr\tw11, [x20]");
    r#gen.emit("\tb\t.Lformat_float_done");
    r#gen.emit(".Lformat_float_finite:");
    r#gen.emit("\tcbnz\tx9, .Lformat_float_nonzero");
    move_wide(r#gen, "x11", 0x302e30); // "0.0"
    r#gen.emit("\tstr\tw11, [x20]");
    r#gen.emit("\tb\t.Lformat_float_done");

    // decoded as Rust's flt2dec::decode does: v = mant * 2^x19, with mant in x4 and plus in x5
    r#gen.emit(".Lformat_float_nonzero:");
    r#gen.emit("\tubfx\tx10, x9, #52, #11");
    r#gen.emit("\tand\tx11, x9, #0xfffffffffffff");
    r#gen.emit("\tmov\tx5, #1");
    r#gen.emit("\tmov\tx23, #1");
    r#gen.emit("\tcbnz\tx10, .Lformat_float_normal");
    // a subnormal's mantissa is its fraction doubled, which is always even
    r#gen.emit("\tlsl\tx4, x11, #1");
    r#gen.emit("\tmov\tx19, #-1075");
    r#gen.emit("\tb\t.Lformat_float_decoded");
    r#gen.emit(".Lformat_float_normal:");
    r#gen.emit("\tsub\tx19, x10, #1075");
    r#gen.emit("\torr\tx4, x11, #0x10000000000000");
    r#gen.emit("\tmvn\tx9, x4");
    r#gen.emit("\tand\tx23, x9, #1");
    r#gen.emit("\tcbnz\tx11, .Lformat_float_symmetric");
    // the gap below a power of two is half the gap above it
    r#gen.emit("\tlsl\tx4, x4, #2");
    r#gen.emit("\tsub\tx19, x19, #2");
    r#gen.emit("\tmov\tx5, #2");
    r#gen.emit("\tb\t.Lformat_float_decoded");
    r#gen.emit(".Lformat_float_symmetric:");
    r#gen.emit("\tlsl\tx4, x4, #1");
    r#gen.emit("\tsub\tx19, x19, #1");
    r#gen.emit(".Lformat_float_decoded:");
    // estimates the decimal exponent k from mant + plus as Rust's estimate_scaling_factor does
    r#gen.emit("\tadd\tx9, x4, x5");
    r#gen.emit("\tsub\tx9, x9, #1");
    r#gen.emit("\tclz\tx9, x9");
    r#gen.emit("\tmov\tx10, #64");
    r#gen.emit("\tsub\tx9, x10, x9");
    r#gen.emit("\tadd\tx9, x9, x19");
    move_wide(r#gen, "x10", 1292913986); // floor(2^32 * log10(2))
    r#gen.emit("\tmul\tx9, x9, x10");
    r#gen.emit("\tasr\tx21, x9, #32");
    call_with(r#gen, "stone.big_set", mant, "x4");
    call_with(r#gen, "stone.big_set", plus, "x5");
    call_with(r#gen, "stone.big_set", minus, "#1");
    call_with(r#gen, "stone.big_set", scale, "#1");
    // then v = mant / scale, as whole numbers
    r#gen.emit("\ttbnz\tx19, #63, .Lformat_float_fraction");
    for each in [mant, minus, plus] {
        call_with(r#gen, "stone.big_shl", each, "x19");
    }
    r#gen.emit("\tb\t.Lformat_float_powers");
    r#gen.emit(".Lformat_float_fraction:");
    r#gen.emit("\tneg\tx4, x19");
    call_with(r#gen, "stone.big_shl", scale, "x4");
    // and v / 10^k = mant / scale
    r#gen.emit(".Lformat_float_powers:");
    r#gen.emit("\ttbnz\tx21, #63, .Lformat_float_small");
    call_with(r#gen, "stone.big_mul_pow10", scale, "x21");
    r#gen.emit("\tb\t.Lformat_float_estimated");
    r#gen.emit(".Lformat_float_small:");
    r#gen.emit("\tneg\tx4, x21");
    for each in [mant, minus, plus] {
        call_with(r#gen, "stone.big_mul_pow10", each, "x4");
    }
    // if mant + plus passes scale, k was one too low, and otherwise the first digit is the
    // next one down
    r#gen.emit(".Lformat_float_estimated:");
    r#gen.emit("\tbl\t.Lformat_float_high");
    r#gen.emit("\tcmp\tx0, x23");
    r#gen.emit("\tb.ge\t.Lformat_float_times_ten");
    r#gen.emit("\tadd\tx21, x21, #1");
    r#gen.emit("\tb\t.Lformat_float_start");
    r#gen.emit(".Lformat_float_times_ten:");
    r#gen.emit("\tbl\t.Lformat_float_next");
    r#gen.emit(".Lformat_float_start:");
    for (double, from) in [(scale2, scale), (scale4, scale2), (scale8, scale4)] {
        call2(r#gen, "stone.big_copy", double, from);
        call_with(r#gen, "stone.big_shl", double, "#1");
    }
    r#gen.emit("\tmov\tx22, #0");
    // each digit is floor(mant / scale), leaving the remainder in mant
    r#gen.emit(".Lformat_float_digit:");
    r#gen.emit("\tbl\t.Lformat_float_extract");
    r#gen.emit("\tadd\tw0, w0, #48"); // '0'
    r#gen.emit("\tstrb\tw0, [x24, x22]");
    r#gen.emit("\tadd\tx22, x22, #1");
    // bit 0 of x19 says the digits are within the lower bound, and bit 1 that rounding them
    // up is within the upper one
    call2(r#gen, "stone.big_cmp", mant, minus);
    r#gen.emit("\tcmp\tx0, x23");
    r#gen.emit("\tcset\tx19, lt");
    r#gen.emit("\tbl\t.Lformat_float_high");
    r#gen.emit("\tcmp\tx0, x23");
    r#gen.emit("\tcset\tx9, lt");
    r#gen.emit("\torr\tx19, x19, x9, lsl #1");
    r#gen.emit("\tcbnz\tx19, .Lformat_float_stop");
    r#gen.emit("\tbl\t.Lformat_float_next");
    r#gen.emit("\tb\t.Lformat_float_digit");
    // the shortest digits round up when only that is within bounds, or when both are and the
    // remainder is at least half; then 99...9 becomes 100...0, one digit longer
    r#gen.emit(".Lformat_float_stop:");
    r#gen.emit("\ttbz\tx19, #1, .Lformat_float_exact");
    r#gen.emit("\ttbz\tx19, #0, .Lformat_float_nines");
    r#gen.emit("\tbl\t.Lformat_float_twice");
    r#gen.emit("\ttbnz\tx0, #63, .Lformat_float_exact");
    r#gen.emit(".Lformat_float_nines:");
    r#gen.emit("\tmov\tx9, #0");
    r#gen.emit(".Lformat_float_nine:");
    r#gen.emit("\tldrb\tw10, [x24, x9]");
    r#gen.emit("\tcmp\tw10, #57"); // '9'
    r#gen.emit("\tb.ne\t.Lformat_float_exact");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit("\tcmp\tx9, x22");
    r#gen.emit("\tb.lo\t.Lformat_float_nine");
    r#gen.emit("\tbl\t.Lformat_float_next");
    r#gen.emit("\tbl\t.Lformat_float_extract");
    r#gen.emit("\tadd\tw0, w0, #48");
    r#gen.emit("\tstrb\tw0, [x24, x22]");
    r#gen.emit("\tadd\tx22, x22, #1");
    // the exact value rounds half to even at the last digit, which can carry through 9s
    r#gen.emit(".Lformat_float_exact:");
    r#gen.emit("\tbl\t.Lformat_float_twice");
    r#gen.emit("\ttbnz\tx0, #63, .Lformat_float_layout");
    r#gen.emit("\tcbnz\tx0, .Lformat_float_carry");
    r#gen.emit("\tsub\tx9, x22, #1");
    r#gen.emit("\tldrb\tw10, [x24, x9]");
    r#gen.emit("\ttbz\tw10, #0, .Lformat_float_layout"); // even, as '0' is even
    r#gen.emit(".Lformat_float_carry:");
    r#gen.emit("\tmov\tx9, x22");
    r#gen.emit(".Lformat_float_carry_loop:");
    r#gen.emit("\tsubs\tx9, x9, #1");
    r#gen.emit("\tb.mi\t.Lformat_float_overflow");
    r#gen.emit("\tldrb\tw10, [x24, x9]");
    r#gen.emit("\tcmp\tw10, #57");
    r#gen.emit("\tb.ne\t.Lformat_float_increment");
    r#gen.emit("\tmov\tw10, #48");
    r#gen.emit("\tstrb\tw10, [x24, x9]");
    r#gen.emit("\tb\t.Lformat_float_carry_loop");
    r#gen.emit(".Lformat_float_increment:");
    r#gen.emit("\tadd\tw10, w10, #1");
    r#gen.emit("\tstrb\tw10, [x24, x9]");
    r#gen.emit("\tb\t.Lformat_float_layout");
    r#gen.emit(".Lformat_float_overflow:");
    r#gen.emit("\tmov\tw10, #49"); // '1', then the 0s
    r#gen.emit("\tstrb\tw10, [x24]");
    r#gen.emit("\tadd\tx21, x21, #1");

    // x21 becomes the exponent of the first digit, x9 walks the digits, and x20 the text
    r#gen.emit(".Lformat_float_layout:");
    r#gen.emit("\tsub\tx21, x21, #1");
    r#gen.emit("\tmov\tx9, x24");
    r#gen.emit("\tcmn\tx21, #4");
    r#gen.emit("\tb.lt\t.Lformat_float_scientific");
    r#gen.emit("\tcmp\tx21, #16");
    r#gen.emit("\tb.ge\t.Lformat_float_scientific");
    r#gen.emit("\ttbz\tx21, #63, .Lformat_float_whole");
    // 0.000ddd, with -exponent - 1 zeros
    r#gen.emit("\tmov\tw10, #0x2e30"); // "0."
    r#gen.emit("\tstrh\tw10, [x20], #2");
    r#gen.emit("\tmvn\tx11, x21");
    r#gen.emit("\tmov\tw10, #48");
    r#gen.emit(".Lformat_float_zeros:");
    r#gen.emit("\tcbz\tx11, .Lformat_float_fraction_digits");
    r#gen.emit("\tstrb\tw10, [x20], #1");
    r#gen.emit("\tsub\tx11, x11, #1");
    r#gen.emit("\tb\t.Lformat_float_zeros");
    r#gen.emit(".Lformat_float_fraction_digits:");
    copy_bytes(r#gen, "x20", "x9", "x22", ".Lformat_float_copy_fraction");
    r#gen.emit("\tb\t.Lformat_float_end");
    // x11 digits go before the point
    r#gen.emit(".Lformat_float_whole:");
    r#gen.emit("\tadd\tx11, x21, #1");
    r#gen.emit("\tcmp\tx22, x11");
    r#gen.emit("\tb.gt\t.Lformat_float_point");
    r#gen.emit("\tsub\tx12, x11, x22");
    copy_bytes(r#gen, "x20", "x9", "x22", ".Lformat_float_copy_whole");
    r#gen.emit("\tmov\tw10, #48");
    r#gen.emit(".Lformat_float_trailing:");
    r#gen.emit("\tcbz\tx12, .Lformat_float_point_zero");
    r#gen.emit("\tstrb\tw10, [x20], #1");
    r#gen.emit("\tsub\tx12, x12, #1");
    r#gen.emit("\tb\t.Lformat_float_trailing");
    r#gen.emit(".Lformat_float_point_zero:");
    r#gen.emit("\tmov\tw10, #0x302e"); // ".0"
    r#gen.emit("\tstrh\tw10, [x20], #2");
    r#gen.emit("\tb\t.Lformat_float_end");
    r#gen.emit(".Lformat_float_point:");
    r#gen.emit("\tsub\tx12, x22, x11");
    copy_bytes(r#gen, "x20", "x9", "x11", ".Lformat_float_copy_before");
    r#gen.emit("\tmov\tw10, #46"); // '.'
    r#gen.emit("\tstrb\tw10, [x20], #1");
    copy_bytes(r#gen, "x20", "x9", "x12", ".Lformat_float_copy_after");
    r#gen.emit("\tb\t.Lformat_float_end");
    // d.ddde+XX, with at least two digits of exponent
    r#gen.emit(".Lformat_float_scientific:");
    r#gen.emit("\tldrb\tw10, [x9], #1");
    r#gen.emit("\tstrb\tw10, [x20], #1");
    r#gen.emit("\tsub\tx12, x22, #1");
    r#gen.emit("\tcbz\tx12, .Lformat_float_e");
    r#gen.emit("\tmov\tw10, #46");
    r#gen.emit("\tstrb\tw10, [x20], #1");
    copy_bytes(r#gen, "x20", "x9", "x12", ".Lformat_float_copy_rest");
    r#gen.emit(".Lformat_float_e:");
    r#gen.emit("\tmov\tw10, #0x2b65"); // "e+"
    r#gen.emit("\tstrh\tw10, [x20]");
    r#gen.emit("\tmov\tx11, x21");
    r#gen.emit("\ttbz\tx11, #63, .Lformat_float_exponent");
    r#gen.emit("\tmov\tw10, #45"); // '-'
    r#gen.emit("\tstrb\tw10, [x20, #1]");
    r#gen.emit("\tneg\tx11, x11");
    r#gen.emit(".Lformat_float_exponent:");
    r#gen.emit("\tadd\tx20, x20, #2");
    r#gen.emit("\tcmp\tx11, #100");
    r#gen.emit("\tb.lo\t.Lformat_float_two_digits");
    r#gen.emit("\tmov\tx12, #100");
    r#gen.emit("\tudiv\tx13, x11, x12");
    r#gen.emit("\tmsub\tx11, x13, x12, x11");
    r#gen.emit("\tadd\tw13, w13, #48");
    r#gen.emit("\tstrb\tw13, [x20], #1");
    r#gen.emit(".Lformat_float_two_digits:");
    r#gen.emit("\tmov\tx12, #10");
    r#gen.emit("\tudiv\tx13, x11, x12");
    r#gen.emit("\tmsub\tx11, x13, x12, x11");
    r#gen.emit("\tadd\tw13, w13, #48");
    r#gen.emit("\tstrb\tw13, [x20], #1");
    r#gen.emit("\tadd\tw11, w11, #48");
    r#gen.emit("\tstrb\tw11, [x20], #1");
    r#gen.emit(".Lformat_float_end:");
    r#gen.emit("\tstrb\twzr, [x20]");
    r#gen.emit(".Lformat_float_done:");
    pop_frame(r#gen, &saved);

    // returns in x0 how scale compares with mant + plus
    r#gen.emit(".Lformat_float_high:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    call2(r#gen, "stone.big_copy", sum, mant);
    call2(r#gen, "stone.big_add", sum, plus);
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    address(r#gen, "x0", scale);
    address(r#gen, "x1", sum);
    r#gen.emit("\tb\tstone.big_cmp");

    // returns in x0 how 2 * mant compares with scale
    r#gen.emit(".Lformat_float_twice:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    call2(r#gen, "stone.big_copy", sum, mant);
    call_with(r#gen, "stone.big_shl", sum, "#1");
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    address(r#gen, "x0", sum);
    address(r#gen, "x1", scale);
    r#gen.emit("\tb\tstone.big_cmp");

    // multiplies mant, minus, and plus by 10 for the next digit
    r#gen.emit(".Lformat_float_next:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    for each in [mant, minus, plus] {
        address(r#gen, "x0", each);
        r#gen.emit("\tmov\tx1, #10");
        r#gen.emit("\tmov\tx2, #0");
        r#gen.emit("\tbl\tstone.big_mul_add");
    }
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    r#gen.emit("\tret");

    // returns in x0 floor(mant / scale), which is below 16, taking 8, 4, 2, and 1 times scale
    // from mant where they fit, and counting them in x4
    r#gen.emit(".Lformat_float_extract:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    r#gen.emit("\tmov\tx4, #0");
    for (multiple, times) in [(scale8, 8), (scale4, 4), (scale2, 2), (scale, 1)] {
        let skip = format!(".Lformat_float_extract_{times}");
        call2(r#gen, "stone.big_cmp", mant, multiple);
        r#gen.emit(&format!("\ttbnz\tx0, #63, {skip}"));
        call2(r#gen, "stone.big_sub", mant, multiple);
        r#gen.emit(&format!("\tadd\tx4, x4, #{times}"));
        r#gen.emit(&format!("{skip}:"));
    }
    r#gen.emit("\tmov\tx0, x4");
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    r#gen.emit("\tret");

    // the text goes in 64 bytes of stack
    r#gen.emit("stone.print_float:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-80]!");
    r#gen.emit("\tmov\tx29, sp");
    r#gen.emit("\tadd\tx1, sp, #16");
    r#gen.emit("\tbl\tstone.format_float");
    r#gen.emit("\tadd\tx0, sp, #16");
    r#gen.emit("\tbl\tstone.print_str");
    r#gen.emit("\tldp\tx29, x30, [sp], #80");
    r#gen.emit("\tret");
}

/// Emits `stone.decimal_to_float`, which returns the bits of the float nearest the number in the
/// string in `x0`, the same way as x64's `decimal_runtime`.
pub fn decimal_runtime(r#gen: &mut dyn AssemblyGenerator) {
    let [mant, _, _, scale, ..] = BIGS;
    // x19 holds the cursor, x20 the number of digits kept, x21 the decimal exponent, x22 the
    // sign bit, x23 the digits not yet in mant and x24 how many there are, x25 1 after the
    // point, and x26 not 0 once a digit past MAX_DIGITS is not 0
    let saved = ["x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26"];
    r#gen.emit("stone.decimal_to_float:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit(".Ldecimal_lead:");
    r#gen.emit("\tldrb\tw10, [x19], #1");
    jump_if_space(r#gen, "w10", ".Ldecimal_lead");
    r#gen.emit("\tsub\tx19, x19, #1");
    r#gen.emit("\tmov\tx22, #0");
    r#gen.emit("\tcmp\tw10, #43"); // '+'
    r#gen.emit("\tb.eq\t.Ldecimal_sign");
    r#gen.emit("\tcmp\tw10, #45"); // '-'
    r#gen.emit("\tb.ne\t.Ldecimal_named");
    r#gen.emit("\tmovz\tx22, #0x8000, lsl #48");
    r#gen.emit(".Ldecimal_sign:");
    r#gen.emit("\tadd\tx19, x19, #1");
    r#gen.emit("\tldrb\tw10, [x19]");
    // the grammar allows only inf, infinity, and nan to start with a letter
    r#gen.emit(".Ldecimal_named:");
    r#gen.emit("\torr\tw10, w10, #32");
    r#gen.emit("\tcmp\tw10, #105"); // 'i'
    r#gen.emit("\tb.eq\t.Ldecimal_infinity");
    r#gen.emit("\tcmp\tw10, #110"); // 'n'
    r#gen.emit("\tb.eq\t.Ldecimal_nan");
    call_with(r#gen, "stone.big_set", mant, "#0");
    for reg in ["x20", "x21", "x23", "x24", "x25", "x26"] {
        r#gen.emit(&format!("\tmov\t{reg}, #0"));
    }
    r#gen.emit(".Ldecimal_digit:");
    r#gen.emit("\tldrb\tw10, [x19]");
    r#gen.emit("\tcmp\tw10, #46"); // '.'
    r#gen.emit("\tb.ne\t.Ldecimal_not_point");
    r#gen.emit("\tmov\tx25, #1");
    r#gen.emit("\tadd\tx19, x19, #1");
    r#gen.emit("\tb\t.Ldecimal_digit");
    r#gen.emit(".Ldecimal_not_point:");
    r#gen.emit("\tsub\tw10, w10, #48"); // '0'
    r#gen.emit("\tcmp\tw10, #9");
    r#gen.emit("\tb.hi\t.Ldecimal_digits_done");
    r#gen.emit("\tadd\tx19, x19, #1");
    // each digit after the point divides by 10, and 0s before the first other digit add nothing
    r#gen.emit("\tsub\tx21, x21, x25");
    r#gen.emit("\torr\tx9, x20, x10");
    r#gen.emit("\tcbz\tx9, .Ldecimal_digit");
    r#gen.emit(&format!("\tcmp\tx20, #{MAX_DIGITS}"));
    r#gen.emit("\tb.hs\t.Ldecimal_drop");
    r#gen.emit("\tadd\tx20, x20, #1");
    r#gen.emit("\tmov\tx9, #10");
    r#gen.emit("\tmadd\tx23, x23, x9, x10");
    r#gen.emit("\tadd\tx24, x24, #1");
    r#gen.emit("\tcmp\tx24, #19");
    r#gen.emit("\tb.ne\t.Ldecimal_digit");
    r#gen.emit("\tbl\t.Ldecimal_flush");
    r#gen.emit("\tb\t.Ldecimal_digit");
    // a dropped digit multiplies by 10 instead
    r#gen.emit(".Ldecimal_drop:");
    r#gen.emit("\tadd\tx21, x21, #1");
    r#gen.emit("\torr\tx26, x26, x10");
    r#gen.emit("\tb\t.Ldecimal_digit");
    r#gen.emit(".Ldecimal_digits_done:");
    r#gen.emit("\tbl\t.Ldecimal_flush");
    // dropped digits that are not all 0 make the number a little more than the kept ones
    r#gen.emit("\tcbz\tx26, .Ldecimal_exponent");
    address(r#gen, "x0", mant);
    r#gen.emit("\tmov\tx1, #10");
    r#gen.emit("\tmov\tx2, #1");
    r#gen.emit("\tbl\tstone.big_mul_add");
    r#gen.emit("\tadd\tx20, x20, #1");
    r#gen.emit("\tsub\tx21, x21, #1");
    // x11 is 1 for a negative exponent, and x12 holds it as it is read
    r#gen.emit(".Ldecimal_exponent:");
    r#gen.emit("\tldrb\tw10, [x19]");
    r#gen.emit("\torr\tw10, w10, #32");
    r#gen.emit("\tcmp\tw10, #101"); // 'e'
    r#gen.emit("\tb.ne\t.Ldecimal_value");
    r#gen.emit("\tadd\tx19, x19, #1");
    r#gen.emit("\tmov\tx11, #0");
    r#gen.emit("\tldrb\tw10, [x19]");
    r#gen.emit("\tcmp\tw10, #43"); // '+'
    r#gen.emit("\tb.eq\t.Ldecimal_exponent_sign");
    r#gen.emit("\tcmp\tw10, #45"); // '-'
    r#gen.emit("\tb.ne\t.Ldecimal_exponent_digits");
    r#gen.emit("\tmov\tx11, #1");
    r#gen.emit(".Ldecimal_exponent_sign:");
    r#gen.emit("\tadd\tx19, x19, #1");
    r#gen.emit(".Ldecimal_exponent_digits:");
    r#gen.emit("\tmov\tx12, #0");
    r#gen.emit("\tmov\tx13, #10");
    move_wide(r#gen, "x14", EXPONENT_LIMIT as u64);
    r#gen.emit(".Ldecimal_exponent_digit:");
    r#gen.emit("\tldrb\tw10, [x19]");
    r#gen.emit("\tsub\tw10, w10, #48");
    r#gen.emit("\tcmp\tw10, #9");
    r#gen.emit("\tb.hi\t.Ldecimal_exponent_end");
    r#gen.emit("\tadd\tx19, x19, #1");
    r#gen.emit("\tmadd\tx12, x12, x13, x10");
    r#gen.emit("\tcmp\tx12, x14");
    r#gen.emit("\tcsel\tx12, x12, x14, ls");
    r#gen.emit("\tb\t.Ldecimal_exponent_digit");
    r#gen.emit(".Ldecimal_exponent_end:");
    r#gen.emit("\tcbz\tx11, .Ldecimal_add_exponent");
    r#gen.emit("\tneg\tx12, x12");
    r#gen.emit(".Ldecimal_add_exponent:");
    r#gen.emit("\tadd\tx21, x21, x12");

    // the number is mant * 10^x21, with x20 digits
    r#gen.emit(".Ldecimal_value:");
    r#gen.emit("\tcbz\tx20, .Ldecimal_zero");
    r#gen.emit("\tcmp\tx20, #15");
    r#gen.emit("\tb.hi\t.Ldecimal_big");
    r#gen.emit("\tcmp\tx21, #22");
    r#gen.emit("\tb.gt\t.Ldecimal_big");
    r#gen.emit("\tcmn\tx21, #22");
    r#gen.emit("\tb.lt\t.Ldecimal_big");
    // both the digits and the power of ten are exact floats
    address(r#gen, "x9", mant);
    r#gen.emit("\tldr\tx9, [x9, #8]");
    r#gen.emit("\tscvtf\td0, x9");
    address(r#gen, "x10", ".Lstone_pow10_float");
    r#gen.emit("\ttbnz\tx21, #63, .Ldecimal_divide");
    r#gen.emit("\tldr\td1, [x10, x21, lsl #3]");
    r#gen.emit("\tfmul\td0, d0, d1");
    r#gen.emit("\tb\t.Ldecimal_rounded");
    r#gen.emit(".Ldecimal_divide:");
    r#gen.emit("\tneg\tx9, x21");
    r#gen.emit("\tldr\td1, [x10, x9, lsl #3]");
    r#gen.emit("\tfdiv\td0, d0, d1");
    r#gen.emit(".Ldecimal_rounded:");
    r#gen.emit("\tfmov\tx0, d0");
    r#gen.emit("\torr\tx0, x0, x22");
    r#gen.emit("\tb\t.Ldecimal_done");
    // past 10^310 is infinite, and below 10^-324 is less than half the smallest float
    r#gen.emit(".Ldecimal_big:");
    r#gen.emit("\tadd\tx9, x20, x21");
    r#gen.emit("\tcmp\tx9, #310");
    r#gen.emit("\tb.gt\t.Ldecimal_infinity");
    r#gen.emit("\tcmn\tx9, #324");
    r#gen.emit("\tb.le\t.Ldecimal_zero");
    call_with(r#gen, "stone.big_set", scale, "#1");
    r#gen.emit("\ttbnz\tx21, #63, .Ldecimal_negative_exponent");
    call_with(r#gen, "stone.big_mul_pow10", mant, "x21");
    r#gen.emit("\tb\t.Ldecimal_ratio");
    r#gen.emit(".Ldecimal_negative_exponent:");
    r#gen.emit("\tneg\tx4, x21");
    call_with(r#gen, "stone.big_mul_pow10", scale, "x4");
    // N = mant and S = scale get the same length, then N doubles if it is below S, so that
    // 1 <= N / S < 2 and the number is N / S * 2^x21
    r#gen.emit(".Ldecimal_ratio:");
    address(r#gen, "x0", mant);
    r#gen.emit("\tbl\tstone.big_bitlen");
    r#gen.emit("\tmov\tx21, x0");
    address(r#gen, "x0", scale);
    r#gen.emit("\tbl\tstone.big_bitlen");
    r#gen.emit("\tsubs\tx21, x21, x0");
    r#gen.emit("\tb.le\t.Ldecimal_shift_dividend");
    call_with(r#gen, "stone.big_shl", scale, "x21");
    r#gen.emit("\tb\t.Ldecimal_lined_up");
    r#gen.emit(".Ldecimal_shift_dividend:");
    r#gen.emit("\tneg\tx4, x21");
    call_with(r#gen, "stone.big_shl", mant, "x4");
    r#gen.emit(".Ldecimal_lined_up:");
    call2(r#gen, "stone.big_cmp", mant, scale);
    r#gen.emit("\ttbz\tx0, #63, .Ldecimal_scaled");
    call_with(r#gen, "stone.big_shl", mant, "#1");
    r#gen.emit("\tsub\tx21, x21, #1");
    r#gen.emit(".Ldecimal_scaled:");
    r#gen.emit("\tcmp\tx21, #1023");
    r#gen.emit("\tb.gt\t.Ldecimal_infinity");
    // x20 counts the quotient's bits: 54 for a normal float, 53 places of significand and one
    // to round with, and fewer below 2^-1022, down to none below 2^-1075
    r#gen.emit("\tadd\tx20, x21, #1076");
    r#gen.emit("\tmov\tx9, #54");
    r#gen.emit("\tcmp\tx20, x9");
    r#gen.emit("\tcsel\tx20, x20, x9, le");
    r#gen.emit("\tcmp\tx20, #0");
    r#gen.emit("\tb.le\t.Ldecimal_zero");
    r#gen.emit("\tmov\tx23, #0");
    r#gen.emit(".Ldecimal_quotient:");
    r#gen.emit("\tlsl\tx23, x23, #1");
    call2(r#gen, "stone.big_cmp", mant, scale);
    r#gen.emit("\ttbnz\tx0, #63, .Ldecimal_doubled");
    call2(r#gen, "stone.big_sub", mant, scale);
    r#gen.emit("\tadd\tx23, x23, #1");
    r#gen.emit(".Ldecimal_doubled:");
    call_with(r#gen, "stone.big_shl", mant, "#1");
    r#gen.emit("\tsubs\tx20, x20, #1");
    r#gen.emit("\tb.ne\t.Ldecimal_quotient");
    // the last bit is half a unit, which rounds up if anything remains or the rest is odd
    r#gen.emit("\tlsr\tx0, x23, #1");
    r#gen.emit("\ttbz\tx23, #0, .Ldecimal_compose");
    address(r#gen, "x9", mant);
    r#gen.emit("\tldr\tx9, [x9]");
    r#gen.emit("\tcbnz\tx9, .Ldecimal_round_up");
    r#gen.emit("\ttbz\tx0, #0, .Ldecimal_compose");
    r#gen.emit(".Ldecimal_round_up:");
    r#gen.emit("\tadd\tx0, x0, #1");
    // a normal float's significand has its leading bit, which carries into the exponent
    // field, so adding the exponent less one gives the bits, even when rounding carried
    r#gen.emit(".Ldecimal_compose:");
    r#gen.emit("\tadd\tx9, x21, #1022");
    r#gen.emit("\tcmp\tx9, #0");
    r#gen.emit("\tcsel\tx9, x9, xzr, gt");
    r#gen.emit("\tadd\tx0, x0, x9, lsl #52");
    r#gen.emit("\torr\tx0, x0, x22");
    r#gen.emit("\tb\t.Ldecimal_done");
    r#gen.emit(".Ldecimal_infinity:");
    r#gen.emit("\tmovz\tx0, #0x7ff0, lsl #48");
    r#gen.emit("\torr\tx0, x0, x22");
    r#gen.emit("\tb\t.Ldecimal_done");
    r#gen.emit(".Ldecimal_nan:");
    r#gen.emit("\tmovz\tx0, #0x7ff8, lsl #48");
    r#gen.emit("\torr\tx0, x0, x22");
    r#gen.emit("\tb\t.Ldecimal_done");
    r#gen.emit(".Ldecimal_zero:");
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit(".Ldecimal_done:");
    pop_frame(r#gen, &saved);

    // mant = mant * 10^count + x23 for the x24 digits in x23, which start again from none
    r#gen.emit(".Ldecimal_flush:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    address(r#gen, "x0", mant);
    address(r#gen, "x9", ".Lstone_pow10");
    r#gen.emit("\tldr\tx1, [x9, x24, lsl #3]");
    r#gen.emit("\tmov\tx2, x23");
    r#gen.emit("\tbl\tstone.big_mul_add");
    r#gen.emit("\tmov\tx23, #0");
    r#gen.emit("\tmov\tx24, #0");
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    r#gen.emit("\tret");
}

/// Emits `stone.fmod`, which sets `d0` to the remainder of `d0` divided by `d1`, which is not 0,
/// with the dividend's sign, as Rust's `%` does. It is exact, working on the floats' bits the
/// way musl's `fmod` does: both significands are lined up, and the divisor's is subtracted from
/// the dividend's wherever it fits, one place at a time.
///
/// Float `%` is emitted inline rather than as a call, so like the free routines this preserves
/// every register a vreg can be in, using only `x8` to `x11`, `x16`, `x17`, and `d2`.
pub fn fmod_runtime(r#gen: &mut dyn AssemblyGenerator) {
    // x9 holds the dividend's bits and then significand, x10 the divisor's, x11 and x16 their
    // exponents, x8 the dividend's sign, and x17 a difference
    r#gen.emit("stone.fmod:");
    r#gen.emit("\tfmov\tx9, d0");
    r#gen.emit("\tfmov\tx10, d1");
    r#gen.emit("\tand\tx8, x9, #0x8000000000000000");
    r#gen.emit("\tubfx\tx11, x9, #52, #11");
    r#gen.emit("\tubfx\tx16, x10, #52, #11");
    // an infinite or nan dividend, or a nan divisor, gives nan
    r#gen.emit("\tcmp\tx11, #0x7ff");
    r#gen.emit("\tb.eq\t.Lfmod_nan");
    r#gen.emit("\tcmp\tx16, #0x7ff");
    r#gen.emit("\tb.ne\t.Lfmod_number");
    r#gen.emit("\ttst\tx10, #0xfffffffffffff");
    r#gen.emit("\tb.ne\t.Lfmod_nan");
    // a dividend no larger than the divisor is its own remainder, unless they are equal
    r#gen.emit(".Lfmod_number:");
    r#gen.emit("\tlsl\tx17, x9, #1");
    r#gen.emit("\tcmp\tx17, x10, lsl #1");
    r#gen.emit("\tb.hi\t.Lfmod_reduce");
    r#gen.emit("\tb.eq\t.Lfmod_zero");
    r#gen.emit("\tret");
    // each significand gets its leading bit at bit 52, with a subnormal's exponent lowered to
    // match
    r#gen.emit(".Lfmod_reduce:");
    for (bits, exponent, label) in [("x9", "x11", "dividend"), ("x10", "x16", "divisor")] {
        r#gen.emit(&format!("\tcbnz\t{exponent}, .Lfmod_{label}_normal"));
        r#gen.emit(&format!("\tlsl\tx17, {bits}, #12"));
        r#gen.emit(&format!(".Lfmod_{label}_count:"));
        r#gen.emit(&format!("\ttbnz\tx17, #63, .Lfmod_{label}_shift"));
        r#gen.emit(&format!("\tsub\t{exponent}, {exponent}, #1"));
        r#gen.emit("\tlsl\tx17, x17, #1");
        r#gen.emit(&format!("\tb\t.Lfmod_{label}_count"));
        r#gen.emit(&format!(".Lfmod_{label}_shift:"));
        r#gen.emit("\tmov\tx17, #1");
        r#gen.emit(&format!("\tsub\tx17, x17, {exponent}"));
        r#gen.emit(&format!("\tlsl\t{bits}, {bits}, x17"));
        r#gen.emit(&format!("\tb\t.Lfmod_{label}_done"));
        r#gen.emit(&format!(".Lfmod_{label}_normal:"));
        r#gen.emit(&format!("\tand\t{bits}, {bits}, #0xfffffffffffff"));
        r#gen.emit(&format!("\torr\t{bits}, {bits}, #0x10000000000000"));
        r#gen.emit(&format!(".Lfmod_{label}_done:"));
    }
    r#gen.emit(".Lfmod_loop:");
    r#gen.emit("\tcmp\tx11, x16");
    r#gen.emit("\tb.le\t.Lfmod_last");
    r#gen.emit("\tsubs\tx17, x9, x10");
    r#gen.emit("\tb.mi\t.Lfmod_keep");
    r#gen.emit("\tb.eq\t.Lfmod_zero");
    r#gen.emit("\tmov\tx9, x17");
    r#gen.emit(".Lfmod_keep:");
    r#gen.emit("\tlsl\tx9, x9, #1");
    r#gen.emit("\tsub\tx11, x11, #1");
    r#gen.emit("\tb\t.Lfmod_loop");
    r#gen.emit(".Lfmod_last:");
    r#gen.emit("\tsubs\tx17, x9, x10");
    r#gen.emit("\tb.mi\t.Lfmod_normalize");
    r#gen.emit("\tb.eq\t.Lfmod_zero");
    r#gen.emit("\tmov\tx9, x17");
    // the remainder's leading bit goes back to bit 52, lowering the exponent
    r#gen.emit(".Lfmod_normalize:");
    r#gen.emit("\ttbnz\tx9, #52, .Lfmod_scale");
    r#gen.emit("\tlsl\tx9, x9, #1");
    r#gen.emit("\tsub\tx11, x11, #1");
    r#gen.emit("\tb\t.Lfmod_normalize");
    r#gen.emit(".Lfmod_scale:");
    r#gen.emit("\tcmp\tx11, #0");
    r#gen.emit("\tb.le\t.Lfmod_subnormal");
    r#gen.emit("\tand\tx9, x9, #0xfffffffffffff");
    r#gen.emit("\torr\tx9, x9, x11, lsl #52");
    r#gen.emit("\tb\t.Lfmod_sign");
    r#gen.emit(".Lfmod_subnormal:");
    r#gen.emit("\tmov\tx17, #1");
    r#gen.emit("\tsub\tx17, x17, x11");
    r#gen.emit("\tlsr\tx9, x9, x17");
    r#gen.emit(".Lfmod_sign:");
    r#gen.emit("\torr\tx9, x9, x8");
    r#gen.emit("\tfmov\td0, x9");
    r#gen.emit("\tret");
    // a zero remainder keeps the dividend's sign
    r#gen.emit(".Lfmod_zero:");
    r#gen.emit("\tfmov\td0, x8");
    r#gen.emit("\tret");
    r#gen.emit(".Lfmod_nan:");
    r#gen.emit("\tfmul\td2, d0, d1");
    r#gen.emit("\tfdiv\td0, d2, d2");
    r#gen.emit("\tret");
}

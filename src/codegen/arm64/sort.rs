//! The arm64 runtime's `sort`, a port of x64's (see `x64::sort` for how it works) under the same
//! labels and contracts, with arguments in `x0` onward.
//!
//! The quicksort picks the same pivots and partitions the same way as x64's, so an input that
//! takes one to its heapsort takes the other there too. Where x64 tests the carry flag, this
//! tests `lo` (unsigned lower), which `cmp` sets the same way.

use super::{address, pop_frame, push_frame};
use crate::codegen::AssemblyGenerator;
use crate::codegen::context::{
    SORT_INSERTION_MAX, SORT_PART_MIN, SORT_RADIX_FACTOR, SORT_SPLIT_MIN,
};
use crate::stdlib::FLOAT_KEY_NAN;

/// The bits of positive infinity, which every NaN's bits without the sign are above.
const INFINITY_BITS: u64 = 0x7ff0_0000_0000_0000;

/// What a quicksort orders, which decides how it compares two elements.
#[derive(Clone, Copy)]
enum Elements {
    /// Unsigned 64-bit keys, compared with `cmp`.
    Keys,
    /// Strings, compared by `stone.sort_str_less`.
    Strs,
}

impl Elements {
    fn name(self) -> &'static str {
        match self {
            Elements::Keys => "keys",
            Elements::Strs => "strs",
        }
    }

    /// Emits code that sets the flags so that `lo` holds if the element in register `x` comes
    /// before the one in register `y`, changing no register but `x9`-`x15` and `x30`, which the
    /// quicksort keeps nothing in.
    fn less(self, r#gen: &mut dyn AssemblyGenerator, x: &str, y: &str) {
        match self {
            Elements::Keys => r#gen.emit(&format!("\tcmp\t{x}, {y}")),
            Elements::Strs => {
                r#gen.emit(&format!("\tmov\tx14, {x}"));
                r#gen.emit(&format!("\tmov\tx15, {y}"));
                r#gen.emit("\tbl\tstone.sort_str_less");
            }
        }
    }
}

/// Emits the sort routines among `labels`, under the same labels and contracts as x64's
/// `sort_runtime`, taking the list in `x0`.
pub fn sort_runtime(r#gen: &mut dyn AssemblyGenerator, labels: &[&str]) {
    let ints = labels.contains(&"stone.list_sort_int");
    let floats = labels.contains(&"stone.list_sort_float");
    if ints || floats {
        sort_numbers(r#gen, ints, floats);
        quicksort(r#gen, Elements::Keys);
        radix_sort(r#gen);
    }
    if labels.contains(&"stone.list_sort_str") {
        list_sort_str(r#gen);
        str_less(r#gen);
        quicksort(r#gen, Elements::Strs);
    }
}

/// Emits code that puts `2 * floor(log2(n))`, the levels a quicksort of `n` elements may take
/// before it falls back to heapsort, in `dst`, for `n` in `src`, using `scratch`.
fn depth_limit(r#gen: &mut dyn AssemblyGenerator, dst: &str, src: &str, scratch: &str) {
    r#gen.emit(&format!("\tclz\t{dst}, {src}"));
    r#gen.emit(&format!("\tmov\t{scratch}, #63"));
    r#gen.emit(&format!("\tsub\t{dst}, {scratch}, {dst}"));
    r#gen.emit(&format!("\tlsl\t{dst}, {dst}, #1"));
}

/// Emits code that puts eight times the number of bytes `src`'s highest set bit is in, which
/// must not be 0, in `dst`, using `scratch`: 8 for 1 through 255, 64 for anything with its top
/// bit set.
fn byte_shift(r#gen: &mut dyn AssemblyGenerator, dst: &str, src: &str, scratch: &str) {
    r#gen.emit(&format!("\tclz\t{dst}, {src}"));
    r#gen.emit(&format!("\tand\t{dst}, {dst}, #56"));
    r#gen.emit(&format!("\tmov\t{scratch}, #64"));
    r#gen.emit(&format!("\tsub\t{dst}, {scratch}, {dst}"));
}

/// Emits a loop that copies the `count` words at `src` to `dst`, using `index` and `value`.
fn copy_words(
    r#gen: &mut dyn AssemblyGenerator,
    label: &str,
    [dst, src, count, index, value]: [&str; 5],
) {
    r#gen.emit(&format!("\tmov\t{index}, #0"));
    r#gen.emit(&format!("{label}:"));
    r#gen.emit(&format!("\tcmp\t{index}, {count}"));
    r#gen.emit(&format!("\tb.hs\t{label}_done"));
    r#gen.emit(&format!("\tldr\t{value}, [{src}, {index}, lsl #3]"));
    r#gen.emit(&format!("\tstr\t{value}, [{dst}, {index}, lsl #3]"));
    r#gen.emit(&format!("\tadd\t{index}, {index}, #1"));
    r#gen.emit(&format!("\tb\t{label}"));
    r#gen.emit(&format!("{label}_done:"));
}

/// Emits a loop that zeroes the `count` words at `dst`, using `index`.
fn zero_words(r#gen: &mut dyn AssemblyGenerator, label: &str, [dst, count, index]: [&str; 3]) {
    r#gen.emit(&format!("\tmov\t{index}, #0"));
    r#gen.emit(&format!("{label}:"));
    r#gen.emit(&format!("\tstr\txzr, [{dst}, {index}, lsl #3]"));
    r#gen.emit(&format!("\tadd\t{index}, {index}, #1"));
    r#gen.emit(&format!("\tcmp\t{index}, {count}"));
    r#gen.emit(&format!("\tb.lo\t{label}"));
}

/// Emits `stone.list_sort_int` and `stone.list_sort_float` (the ones asked for) and
/// `stone.sort_numbers`, which they share: it sorts list `x0` of ints, or of floats if `x1` is 1,
/// by turning them into keys and back.
fn sort_numbers(r#gen: &mut dyn AssemblyGenerator, ints: bool, floats: bool) {
    if ints {
        r#gen.emit("stone.list_sort_int:");
        r#gen.emit("\tmov\tx1, #0");
        r#gen.emit("\tb\tstone.sort_numbers");
    }
    if floats {
        r#gen.emit("stone.list_sort_float:");
        r#gen.emit("\tmov\tx1, #1");
        r#gen.emit("\tb\tstone.sort_numbers");
    }
    // x19 holds the list, x20 whether it holds floats, x21 its length, and x22 its elements
    let saved = ["x19", "x20", "x21", "x22"];
    r#gen.emit("stone.sort_numbers:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tldr\tx21, [x19]");
    r#gen.emit("\tcmp\tx21, #2");
    r#gen.emit("\tb.lo\t.Lsort_numbers_out");
    r#gen.emit("\tldr\tx22, [x19, #16]");
    // write each key over its value, counting in x10 the keys smaller than the one before (x9),
    // and keeping the smallest key in x3 and the largest in x4
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit("\tmov\tx9, #0");
    r#gen.emit("\tmov\tx10, #0");
    r#gen.emit("\tmov\tx3, #-1");
    r#gen.emit("\tmov\tx4, #0");
    let tally = |r#gen: &mut dyn AssemblyGenerator, label: &str| {
        r#gen.emit("\tstr\tx0, [x22, x1, lsl #3]");
        r#gen.emit("\tcmp\tx0, x9");
        r#gen.emit("\tcsinc\tx10, x10, x10, hs");
        r#gen.emit("\tmov\tx9, x0");
        r#gen.emit("\tcmp\tx0, x3");
        r#gen.emit("\tcsel\tx3, x0, x3, lo");
        r#gen.emit("\tcmp\tx0, x4");
        r#gen.emit("\tcsel\tx4, x0, x4, hi");
        r#gen.emit("\tadd\tx1, x1, #1");
        r#gen.emit("\tcmp\tx1, x21");
        r#gen.emit(&format!("\tb.lo\t{label}"));
    };
    r#gen.emit("\tcbnz\tx20, .Lsort_numbers_floats");
    r#gen.emit(".Lsort_numbers_ints:");
    r#gen.emit("\tldr\tx0, [x22, x1, lsl #3]");
    r#gen.emit("\teor\tx0, x0, #0x8000000000000000");
    tally(r#gen, ".Lsort_numbers_ints");
    r#gen.emit("\tb\t.Lsort_numbers_keyed");
    r#gen.emit(".Lsort_numbers_floats:");
    r#gen.emit(&format!("\tmov\tx11, #{INFINITY_BITS:#x}"));
    r#gen.emit(&format!("\tmov\tx12, #{FLOAT_KEY_NAN:#x}"));
    r#gen.emit(".Lsort_numbers_float:");
    r#gen.emit("\tldr\tx0, [x22, x1, lsl #3]");
    r#gen.emit("\tand\tx2, x0, #0x7fffffffffffffff");
    // a negative float's bits all flip, a positive one's sign only
    r#gen.emit("\tasr\tx5, x0, #63");
    r#gen.emit("\torr\tx5, x5, #0x8000000000000000");
    r#gen.emit("\teor\tx0, x0, x5");
    r#gen.emit("\tcmp\tx2, x11");
    r#gen.emit("\tcsel\tx0, x12, x0, hi"); // every nan is one value
    tally(r#gen, ".Lsort_numbers_float");
    r#gen.emit(".Lsort_numbers_keyed:");
    r#gen.emit("\tcbz\tx10, .Lsort_numbers_unkey"); // already in order
    r#gen.emit("\tsub\tx0, x21, #1");
    r#gen.emit("\tcmp\tx10, x0");
    r#gen.emit("\tb.eq\t.Lsort_numbers_reverse"); // every key is smaller than the one before
    // radix pays once there are enough keys for each byte it sorts by
    r#gen.emit("\tsub\tx0, x4, x3");
    byte_shift(r#gen, "x0", "x0", "x5");
    r#gen.emit("\tmul\tx0, x0, x0");
    r#gen.emit(&format!("\tmov\tx5, #{SORT_RADIX_FACTOR}"));
    r#gen.emit("\tmul\tx0, x0, x5");
    r#gen.emit("\tlsr\tx0, x0, #6");
    r#gen.emit("\tcmp\tx21, x0");
    r#gen.emit("\tb.hs\t.Lsort_numbers_radix");
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tmov\tx1, x21");
    depth_limit(r#gen, "x2", "x21", "x5");
    r#gen.emit("\tmov\tx3, #0");
    r#gen.emit("\tbl\tstone.sort_keys");
    r#gen.emit("\tb\t.Lsort_numbers_unkey");
    r#gen.emit(".Lsort_numbers_radix:");
    // the sorted keys may end up in another buffer
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tmov\tx1, x3");
    r#gen.emit("\tmov\tx2, x4");
    r#gen.emit("\tbl\tstone.radix_keys");
    r#gen.emit("\tldr\tx22, [x19, #16]");
    r#gen.emit("\tb\t.Lsort_numbers_unkey");
    r#gen.emit(".Lsort_numbers_reverse:");
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit("\tsub\tx2, x21, #1");
    r#gen.emit(".Lsort_numbers_swap:");
    r#gen.emit("\tcmp\tx1, x2");
    r#gen.emit("\tb.hs\t.Lsort_numbers_unkey");
    r#gen.emit("\tldr\tx0, [x22, x1, lsl #3]");
    r#gen.emit("\tldr\tx5, [x22, x2, lsl #3]");
    r#gen.emit("\tstr\tx5, [x22, x1, lsl #3]");
    r#gen.emit("\tstr\tx0, [x22, x2, lsl #3]");
    r#gen.emit("\tadd\tx1, x1, #1");
    r#gen.emit("\tsub\tx2, x2, #1");
    r#gen.emit("\tb\t.Lsort_numbers_swap");
    // write each value back over its key
    r#gen.emit(".Lsort_numbers_unkey:");
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit("\tcbnz\tx20, .Lsort_numbers_unkey_floats");
    r#gen.emit(".Lsort_numbers_unkey_int:");
    r#gen.emit("\tldr\tx0, [x22, x1, lsl #3]");
    r#gen.emit("\teor\tx0, x0, #0x8000000000000000");
    r#gen.emit("\tstr\tx0, [x22, x1, lsl #3]");
    r#gen.emit("\tadd\tx1, x1, #1");
    r#gen.emit("\tcmp\tx1, x21");
    r#gen.emit("\tb.lo\t.Lsort_numbers_unkey_int");
    r#gen.emit("\tb\t.Lsort_numbers_out");
    r#gen.emit(".Lsort_numbers_unkey_floats:");
    r#gen.emit("\tldr\tx0, [x22, x1, lsl #3]");
    // a key with its top bit set was a positive float, whose sign only flips back
    r#gen.emit("\tasr\tx5, x0, #63");
    r#gen.emit("\tmvn\tx5, x5");
    r#gen.emit("\torr\tx5, x5, #0x8000000000000000");
    r#gen.emit("\teor\tx0, x0, x5");
    r#gen.emit("\tstr\tx0, [x22, x1, lsl #3]");
    r#gen.emit("\tadd\tx1, x1, #1");
    r#gen.emit("\tcmp\tx1, x21");
    r#gen.emit("\tb.lo\t.Lsort_numbers_unkey_floats");
    r#gen.emit(".Lsort_numbers_out:");
    r#gen.emit("\tmov\tx0, #0"); // sort returns none
    pop_frame(r#gen, &saved);
}

/// Emits `stone.list_sort_str`, which checks whether list `x0` of strings is already in order,
/// or in strictly reverse order, before handing it to the quicksort.
fn list_sort_str(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the elements and x20 the length
    let saved = ["x19", "x20"];
    r#gen.emit("stone.list_sort_str:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tldr\tx20, [x0]");
    r#gen.emit("\tldr\tx19, [x0, #16]");
    r#gen.emit("\tmov\tx1, #1");
    r#gen.emit(".Lsort_str_ascending:");
    r#gen.emit("\tcmp\tx1, x20");
    r#gen.emit("\tb.hs\t.Lsort_str_out"); // already in order, or shorter than 2
    r#gen.emit("\tsub\tx2, x1, #1");
    r#gen.emit("\tldr\tx5, [x19, x1, lsl #3]");
    r#gen.emit("\tldr\tx6, [x19, x2, lsl #3]");
    Elements::Strs.less(r#gen, "x5", "x6");
    r#gen.emit("\tb.lo\t.Lsort_str_descends");
    r#gen.emit("\tadd\tx1, x1, #1");
    r#gen.emit("\tb\t.Lsort_str_ascending");
    // only a list whose first two strings descend can be in reverse order
    r#gen.emit(".Lsort_str_descends:");
    r#gen.emit("\tcmp\tx1, #1");
    r#gen.emit("\tb.ne\t.Lsort_str_quicksort");
    r#gen.emit(".Lsort_str_descending:");
    r#gen.emit("\tadd\tx1, x1, #1");
    r#gen.emit("\tcmp\tx1, x20");
    r#gen.emit("\tb.hs\t.Lsort_str_reverse");
    r#gen.emit("\tsub\tx2, x1, #1");
    r#gen.emit("\tldr\tx5, [x19, x1, lsl #3]");
    r#gen.emit("\tldr\tx6, [x19, x2, lsl #3]");
    Elements::Strs.less(r#gen, "x5", "x6");
    r#gen.emit("\tb.lo\t.Lsort_str_descending");
    r#gen.emit(".Lsort_str_quicksort:");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tmov\tx1, x20");
    depth_limit(r#gen, "x2", "x20", "x5");
    r#gen.emit("\tmov\tx3, #0");
    r#gen.emit("\tbl\tstone.sort_strs");
    r#gen.emit("\tb\t.Lsort_str_out");
    r#gen.emit(".Lsort_str_reverse:");
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit("\tsub\tx2, x20, #1");
    r#gen.emit(".Lsort_str_swap:");
    r#gen.emit("\tcmp\tx1, x2");
    r#gen.emit("\tb.hs\t.Lsort_str_out");
    r#gen.emit("\tldr\tx0, [x19, x1, lsl #3]");
    r#gen.emit("\tldr\tx5, [x19, x2, lsl #3]");
    r#gen.emit("\tstr\tx5, [x19, x1, lsl #3]");
    r#gen.emit("\tstr\tx0, [x19, x2, lsl #3]");
    r#gen.emit("\tadd\tx1, x1, #1");
    r#gen.emit("\tsub\tx2, x2, #1");
    r#gen.emit("\tb\t.Lsort_str_swap");
    r#gen.emit(".Lsort_str_out:");
    r#gen.emit("\tmov\tx0, #0"); // sort returns none
    pop_frame(r#gen, &saved);
}

/// Emits `stone.sort_str_less`, which sets the flags so that `lo` holds if string `x14` comes
/// before string `x15`: at the first byte where they differ, the one with the smaller byte, or
/// else the shorter. It changes only `x9`-`x13`.
fn str_less(r#gen: &mut dyn AssemblyGenerator) {
    // x11 holds the shorter length and x12 the position
    r#gen.emit("stone.sort_str_less:");
    r#gen.emit("\tldur\tx9, [x14, #-16]");
    r#gen.emit("\tldur\tx10, [x15, #-16]");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tcsel\tx11, x9, x10, lo");
    r#gen.emit("\tmov\tx12, #0");
    // eight bytes at a time while both strings have them
    r#gen.emit(".Lsort_str_less_words:");
    r#gen.emit("\tadd\tx13, x12, #8");
    r#gen.emit("\tcmp\tx13, x11");
    r#gen.emit("\tb.hi\t.Lsort_str_less_bytes");
    r#gen.emit("\tldr\tx9, [x14, x12]");
    r#gen.emit("\tldr\tx10, [x15, x12]");
    r#gen.emit("\teor\tx9, x9, x10");
    r#gen.emit("\tcbnz\tx9, .Lsort_str_less_differ");
    r#gen.emit("\tmov\tx12, x13");
    r#gen.emit("\tb\t.Lsort_str_less_words");
    // the lowest set bit of the difference is in the first byte that differs
    r#gen.emit(".Lsort_str_less_differ:");
    r#gen.emit("\tneg\tx10, x9");
    r#gen.emit("\tand\tx9, x9, x10");
    r#gen.emit("\tclz\tx9, x9");
    r#gen.emit("\tmov\tx10, #63");
    r#gen.emit("\tsub\tx9, x10, x9");
    r#gen.emit("\tlsr\tx9, x9, #3");
    r#gen.emit("\tadd\tx12, x12, x9");
    r#gen.emit("\tldrb\tw9, [x14, x12]");
    r#gen.emit("\tldrb\tw10, [x15, x12]");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tret");
    r#gen.emit(".Lsort_str_less_bytes:");
    r#gen.emit("\tcmp\tx12, x11");
    r#gen.emit("\tb.hs\t.Lsort_str_less_lengths");
    r#gen.emit("\tldrb\tw9, [x14, x12]");
    r#gen.emit("\tldrb\tw10, [x15, x12]");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.ne\t.Lsort_str_less_out");
    r#gen.emit("\tadd\tx12, x12, #1");
    r#gen.emit("\tb\t.Lsort_str_less_bytes");
    r#gen.emit(".Lsort_str_less_lengths:");
    r#gen.emit("\tldur\tx9, [x14, #-16]");
    r#gen.emit("\tldur\tx10, [x15, #-16]");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit(".Lsort_str_less_out:");
    r#gen.emit("\tret");
}

/// Emits `code` with each `{}` replaced by the routine's label prefix.
fn emit_with(r#gen: &mut dyn AssemblyGenerator, prefix: &str, code: &str) {
    r#gen.emit(&code.replace("{}", prefix));
}

/// Emits code that swaps the elements at `a` and `b` if the one at `b` comes first, without a
/// branch.
fn sort2(r#gen: &mut dyn AssemblyGenerator, elements: Elements, a: &str, b: &str) {
    r#gen.emit(&format!("\tldr\tx5, {a}"));
    r#gen.emit(&format!("\tldr\tx6, {b}"));
    elements.less(r#gen, "x6", "x5");
    r#gen.emit("\tcsel\tx7, x6, x5, lo");
    r#gen.emit("\tcsel\tx6, x5, x6, lo");
    r#gen.emit(&format!("\tstr\tx7, {a}"));
    r#gen.emit(&format!("\tstr\tx6, {b}"));
}

/// Emits code that puts the elements at positions `x4`, `x3`, and `x1` of `x19` in order.
fn sort3(r#gen: &mut dyn AssemblyGenerator, elements: Elements) {
    let a = "[x19, x4, lsl #3]";
    let b = "[x19, x3, lsl #3]";
    let c = "[x19, x1, lsl #3]";
    sort2(r#gen, elements, a, b);
    sort2(r#gen, elements, b, c);
    sort2(r#gen, elements, a, b);
}

/// Emits `stone.sort_keys` or `stone.sort_strs`, a quicksort of the `x1` elements at `x0`,
/// falling back to heapsort after `x2` more levels. `x3` is 1 if the element before `x0` is
/// part of the list, and so no greater than any of them. It preserves `x19`-`x28`.
fn quicksort(r#gen: &mut dyn AssemblyGenerator, elements: Elements) {
    let name = elements.name();
    let prefix = format!(".Lsort_{name}");
    let emit = |r#gen: &mut dyn AssemblyGenerator, code: &str| emit_with(r#gen, &prefix, code);
    // x19 holds the first element, x20 the length, x21 the levels left, x22 whether the element
    // before the first is in the list, and x23 the pivot
    let saved = ["x19", "x20", "x21", "x22", "x23"];
    r#gen.emit(&format!("stone.sort_{name}:"));
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tmov\tx21, x2");
    r#gen.emit("\tmov\tx22, x3");
    emit(r#gen, "{}_loop:");
    r#gen.emit(&format!("\tcmp\tx20, #{SORT_INSERTION_MAX}"));
    emit(r#gen, "\tb.ls\t{}_insertion");
    emit(r#gen, "\tcbz\tx21, {}_heap");
    r#gen.emit("\tsub\tx21, x21, #1");
    // the pivot is the median of the first, middle, and last elements, or for a long range of
    // the medians of three triples around them, which ends up in the middle (x0)
    r#gen.emit("\tlsr\tx0, x20, #1");
    r#gen.emit("\tcmp\tx20, #128");
    emit(r#gen, "\tb.ls\t{}_median");
    r#gen.emit("\tmov\tx4, #0");
    r#gen.emit("\tmov\tx3, x0");
    r#gen.emit("\tsub\tx1, x20, #1");
    sort3(r#gen, elements);
    r#gen.emit("\tmov\tx4, #1");
    r#gen.emit("\tsub\tx3, x0, #1");
    r#gen.emit("\tsub\tx1, x20, #2");
    sort3(r#gen, elements);
    r#gen.emit("\tmov\tx4, #2");
    r#gen.emit("\tadd\tx3, x0, #1");
    r#gen.emit("\tsub\tx1, x20, #3");
    sort3(r#gen, elements);
    r#gen.emit("\tsub\tx4, x0, #1");
    r#gen.emit("\tmov\tx3, x0");
    r#gen.emit("\tadd\tx1, x0, #1");
    sort3(r#gen, elements);
    emit(r#gen, "\tb\t{}_pivot");
    emit(r#gen, "{}_median:");
    r#gen.emit("\tmov\tx4, #0");
    r#gen.emit("\tmov\tx3, x0");
    r#gen.emit("\tsub\tx1, x20, #1");
    sort3(r#gen, elements);
    // move the pivot to the front, out of the way of the partition
    emit(r#gen, "{}_pivot:");
    r#gen.emit("\tldr\tx5, [x19]");
    r#gen.emit("\tldr\tx23, [x19, x0, lsl #3]");
    r#gen.emit("\tstr\tx23, [x19]");
    r#gen.emit("\tstr\tx5, [x19, x0, lsl #3]");
    // a pivot equal to the element before the range is its smallest value
    emit(r#gen, "\tcbz\tx22, {}_partition");
    r#gen.emit("\tldur\tx5, [x19, #-8]");
    elements.less(r#gen, "x5", "x23");
    emit(r#gen, "\tb.hs\t{}_equal");
    // swap each element (x0) to the end of those before the pivot (x2) and count it there if
    // it is, so the comparison decides no branch
    emit(r#gen, "{}_partition:");
    r#gen.emit("\tmov\tx1, #1");
    r#gen.emit("\tmov\tx2, #1");
    emit(r#gen, "{}_partition_loop:");
    r#gen.emit("\tldr\tx0, [x19, x1, lsl #3]");
    r#gen.emit("\tldr\tx5, [x19, x2, lsl #3]");
    r#gen.emit("\tstr\tx5, [x19, x1, lsl #3]");
    r#gen.emit("\tstr\tx0, [x19, x2, lsl #3]");
    elements.less(r#gen, "x0", "x23");
    r#gen.emit("\tcsinc\tx2, x2, x2, hs");
    r#gen.emit("\tadd\tx1, x1, #1");
    r#gen.emit("\tcmp\tx1, x20");
    emit(r#gen, "\tb.lo\t{}_partition_loop");
    // the pivot goes between the two sides, at x2
    r#gen.emit("\tsub\tx2, x2, #1");
    r#gen.emit("\tldr\tx0, [x19, x2, lsl #3]");
    r#gen.emit("\tstr\tx0, [x19]");
    r#gen.emit("\tstr\tx23, [x19, x2, lsl #3]");
    r#gen.emit("\tsub\tx1, x20, x2");
    r#gen.emit("\tsub\tx1, x1, #1");
    // sort the shorter side by recursion and the longer by looping
    r#gen.emit("\tcmp\tx2, x1");
    emit(r#gen, "\tb.hi\t{}_right_first");
    r#gen.emit("\tstp\tx1, x2, [sp, #-16]!");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tmov\tx1, x2");
    r#gen.emit("\tmov\tx2, x21");
    r#gen.emit("\tmov\tx3, x22");
    r#gen.emit(&format!("\tbl\tstone.sort_{name}"));
    r#gen.emit("\tldp\tx1, x2, [sp], #16");
    r#gen.emit("\tadd\tx19, x19, x2, lsl #3");
    r#gen.emit("\tadd\tx19, x19, #8");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tmov\tx22, #1");
    emit(r#gen, "\tb\t{}_loop");
    emit(r#gen, "{}_right_first:");
    r#gen.emit("\tstp\tx1, x2, [sp, #-16]!");
    r#gen.emit("\tadd\tx0, x19, x2, lsl #3");
    r#gen.emit("\tadd\tx0, x0, #8");
    r#gen.emit("\tmov\tx2, x21");
    r#gen.emit("\tmov\tx3, #1");
    r#gen.emit(&format!("\tbl\tstone.sort_{name}"));
    r#gen.emit("\tldp\tx1, x2, [sp], #16");
    r#gen.emit("\tmov\tx20, x2");
    emit(r#gen, "\tb\t{}_loop");
    // gather every element no greater than the pivot, which are all equal to it, at the front,
    // where they are in order already
    emit(r#gen, "{}_equal:");
    r#gen.emit("\tmov\tx1, #1");
    r#gen.emit("\tmov\tx2, #1");
    emit(r#gen, "{}_equal_loop:");
    r#gen.emit("\tldr\tx0, [x19, x1, lsl #3]");
    r#gen.emit("\tldr\tx5, [x19, x2, lsl #3]");
    r#gen.emit("\tstr\tx5, [x19, x1, lsl #3]");
    r#gen.emit("\tstr\tx0, [x19, x2, lsl #3]");
    elements.less(r#gen, "x23", "x0");
    r#gen.emit("\tcsinc\tx2, x2, x2, lo");
    r#gen.emit("\tadd\tx1, x1, #1");
    r#gen.emit("\tcmp\tx1, x20");
    emit(r#gen, "\tb.lo\t{}_equal_loop");
    r#gen.emit("\tadd\tx19, x19, x2, lsl #3");
    r#gen.emit("\tsub\tx20, x20, x2");
    emit(r#gen, "\tb\t{}_loop");
    // insertion sort, moving each element (x0) down past the greater ones before it
    emit(r#gen, "{}_insertion:");
    r#gen.emit("\tmov\tx1, #1");
    emit(r#gen, "{}_insert:");
    r#gen.emit("\tcmp\tx1, x20");
    emit(r#gen, "\tb.hs\t{}_done");
    r#gen.emit("\tldr\tx0, [x19, x1, lsl #3]");
    r#gen.emit("\tmov\tx2, x1");
    emit(r#gen, "{}_insert_step:");
    emit(r#gen, "\tcbz\tx2, {}_insert_place");
    r#gen.emit("\tsub\tx3, x2, #1");
    r#gen.emit("\tldr\tx5, [x19, x3, lsl #3]");
    elements.less(r#gen, "x0", "x5");
    emit(r#gen, "\tb.hs\t{}_insert_place");
    r#gen.emit("\tstr\tx5, [x19, x2, lsl #3]");
    r#gen.emit("\tmov\tx2, x3");
    emit(r#gen, "\tb\t{}_insert_step");
    emit(r#gen, "{}_insert_place:");
    r#gen.emit("\tstr\tx0, [x19, x2, lsl #3]");
    r#gen.emit("\tadd\tx1, x1, #1");
    emit(r#gen, "\tb\t{}_insert");
    // heapsort: make a max-heap, then move its top to the end until it is empty
    emit(r#gen, "{}_heap:");
    r#gen.emit("\tlsr\tx1, x20, #1");
    emit(r#gen, "{}_heapify:");
    emit(r#gen, "\tcbz\tx1, {}_heap_ready");
    r#gen.emit("\tsub\tx1, x1, #1");
    r#gen.emit("\tmov\tx3, x1");
    r#gen.emit("\tmov\tx4, x20");
    emit(r#gen, "\tbl\t{}_sift");
    emit(r#gen, "\tb\t{}_heapify");
    emit(r#gen, "{}_heap_ready:");
    r#gen.emit("\tmov\tx1, x20");
    emit(r#gen, "{}_heap_pop:");
    r#gen.emit("\tsubs\tx1, x1, #1");
    emit(r#gen, "\tb.eq\t{}_done");
    r#gen.emit("\tldr\tx0, [x19]");
    r#gen.emit("\tldr\tx5, [x19, x1, lsl #3]");
    r#gen.emit("\tstr\tx5, [x19]");
    r#gen.emit("\tstr\tx0, [x19, x1, lsl #3]");
    r#gen.emit("\tmov\tx3, #0");
    r#gen.emit("\tmov\tx4, x1");
    emit(r#gen, "\tbl\t{}_sift");
    emit(r#gen, "\tb\t{}_heap_pop");
    emit(r#gen, "{}_done:");
    pop_frame(r#gen, &saved);
    // moves the element at x3 down the heap of the x4 elements at x19 until no child is
    // greater, preserving x1
    emit(r#gen, "{}_sift:");
    push_frame(r#gen, &[]);
    emit(r#gen, "{}_sift_down:");
    r#gen.emit("\tlsl\tx5, x3, #1");
    r#gen.emit("\tadd\tx5, x5, #1");
    r#gen.emit("\tcmp\tx5, x4");
    emit(r#gen, "\tb.hs\t{}_sift_done");
    r#gen.emit("\tadd\tx6, x5, #1");
    r#gen.emit("\tcmp\tx6, x4");
    emit(r#gen, "\tb.hs\t{}_sift_child");
    r#gen.emit("\tldr\tx0, [x19, x5, lsl #3]");
    r#gen.emit("\tldr\tx2, [x19, x6, lsl #3]");
    elements.less(r#gen, "x0", "x2");
    emit(r#gen, "\tb.hs\t{}_sift_child");
    r#gen.emit("\tmov\tx5, x6");
    emit(r#gen, "{}_sift_child:");
    r#gen.emit("\tldr\tx0, [x19, x3, lsl #3]");
    r#gen.emit("\tldr\tx2, [x19, x5, lsl #3]");
    elements.less(r#gen, "x0", "x2");
    emit(r#gen, "\tb.hs\t{}_sift_done");
    r#gen.emit("\tstr\tx2, [x19, x3, lsl #3]");
    r#gen.emit("\tstr\tx0, [x19, x5, lsl #3]");
    r#gen.emit("\tmov\tx3, x5");
    emit(r#gen, "\tb\t{}_sift_down");
    emit(r#gen, "{}_sift_done:");
    pop_frame(r#gen, &[]);
}

/// Emits `stone.radix_keys`, which sorts the keys of list `x0`, which are no smaller than `x1`
/// and no larger than `x2`, and not all the same, as x64's does.
fn radix_sort(r#gen: &mut dyn AssemblyGenerator) {
    // how many keys have each byte at each position, then where the next one goes, and where
    // each part of a split list ends
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t3");
    r#gen.emit("stone.sort_counts:");
    r#gen.emit("\t.zero\t16384");
    r#gen.emit("stone.sort_parts:");
    r#gen.emit("\t.zero\t2048");
    r#gen.emit("\t.text");
    radix_range(r#gen);
    // x19 holds the list, x20 its length, x21 its keys, x22 the smallest key, x23 the shift past
    // the highest byte to sort by, x24 the new buffer, and while the parts of a split list are
    // sorted, x25 the part and x26 where it starts
    let saved = ["x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26"];
    r#gen.emit("stone.radix_keys:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tldr\tx20, [x19]");
    r#gen.emit("\tldr\tx21, [x19, #16]");
    r#gen.emit("\tmov\tx22, x1");
    r#gen.emit("\tsub\tx2, x2, x1");
    byte_shift(r#gen, "x23", "x2", "x3");
    // the highest byte always takes a pass, since the smallest key has 0 there and the largest
    // does not
    r#gen.emit("\tlsl\tx0, x20, #3");
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tmov\tx24, x0");
    r#gen.emit("\tcmp\tx23, #8");
    r#gen.emit("\tb.eq\t.Lradix_whole");
    r#gen.emit(&format!("\tmov\tx0, #{SORT_SPLIT_MIN}"));
    r#gen.emit("\tcmp\tx20, x0");
    r#gen.emit("\tb.hs\t.Lradix_split");
    r#gen.emit(".Lradix_whole:");
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tmov\tx1, x24");
    r#gen.emit("\tmov\tx2, x20");
    r#gen.emit("\tmov\tx3, x22");
    r#gen.emit("\tmov\tx4, x23");
    r#gen.emit("\tbl\tstone.radix_range");
    r#gen.emit("\tcmp\tx0, x21");
    r#gen.emit("\tb.eq\t.Lradix_free");
    // the keys ended in the new buffer, so the list takes it
    r#gen.emit("\tstr\tx0, [x19, #16]");
    r#gen.emit("\tstr\tx20, [x19, #8]");
    r#gen.emit("\tmov\tx24, x21");
    r#gen.emit("\tb\t.Lradix_free");
    // split by the highest byte (shift x23), counting each part's keys
    r#gen.emit(".Lradix_split:");
    r#gen.emit("\tsub\tx23, x23, #8");
    address(r#gen, "x12", "stone.sort_counts");
    r#gen.emit("\tmov\tx11, #256");
    zero_words(r#gen, ".Lradix_split_zero", ["x12", "x11", "x10"]);
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit(".Lradix_split_count:");
    r#gen.emit("\tldr\tx0, [x21, x1, lsl #3]");
    r#gen.emit("\tsub\tx0, x0, x22");
    r#gen.emit("\tlsr\tx0, x0, x23");
    r#gen.emit("\tldr\tx5, [x12, x0, lsl #3]");
    r#gen.emit("\tadd\tx5, x5, #1");
    r#gen.emit("\tstr\tx5, [x12, x0, lsl #3]");
    r#gen.emit("\tadd\tx1, x1, #1");
    r#gen.emit("\tcmp\tx1, x20");
    r#gen.emit("\tb.lo\t.Lradix_split_count");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tmov\tx2, #0");
    r#gen.emit(".Lradix_split_offset:");
    r#gen.emit("\tldr\tx5, [x12, x2, lsl #3]");
    r#gen.emit("\tstr\tx0, [x12, x2, lsl #3]");
    r#gen.emit("\tadd\tx0, x0, x5");
    r#gen.emit("\tadd\tx2, x2, #1");
    r#gen.emit("\tcmp\tx2, #256");
    r#gen.emit("\tb.lo\t.Lradix_split_offset");
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit(".Lradix_split_move:");
    r#gen.emit("\tldr\tx0, [x21, x1, lsl #3]");
    r#gen.emit("\tsub\tx2, x0, x22");
    r#gen.emit("\tlsr\tx2, x2, x23");
    r#gen.emit("\tldr\tx6, [x12, x2, lsl #3]");
    r#gen.emit("\tstr\tx0, [x24, x6, lsl #3]");
    r#gen.emit("\tadd\tx6, x6, #1");
    r#gen.emit("\tstr\tx6, [x12, x2, lsl #3]");
    r#gen.emit("\tadd\tx1, x1, #1");
    r#gen.emit("\tcmp\tx1, x20");
    r#gen.emit("\tb.lo\t.Lradix_split_move");
    // each offset is now where its part ends; keep them, since sorting the parts counts again
    address(r#gen, "x13", "stone.sort_parts");
    r#gen.emit("\tmov\tx11, #256");
    copy_words(
        r#gen,
        ".Lradix_split_keep",
        ["x13", "x12", "x11", "x10", "x9"],
    );
    // sort each part back into the list's buffer
    r#gen.emit("\tmov\tx25, #0");
    r#gen.emit("\tmov\tx26, #0");
    r#gen.emit(".Lradix_part:");
    address(r#gen, "x13", "stone.sort_parts");
    r#gen.emit("\tldr\tx2, [x13, x25, lsl #3]");
    r#gen.emit("\tmov\tx0, x26");
    r#gen.emit("\tmov\tx26, x2");
    r#gen.emit("\tsubs\tx2, x2, x0"); // its length
    r#gen.emit("\tb.eq\t.Lradix_part_next");
    r#gen.emit("\tadd\tx1, x24, x0, lsl #3");
    r#gen.emit("\tadd\tx0, x21, x0, lsl #3");
    r#gen.emit(&format!("\tcmp\tx2, #{SORT_PART_MIN}"));
    r#gen.emit("\tb.hs\t.Lradix_part_radix");
    // a short part is copied back and sorted where it is
    copy_words(r#gen, ".Lradix_part_copy", ["x0", "x1", "x2", "x6", "x7"]);
    r#gen.emit("\tmov\tx1, x2");
    depth_limit(r#gen, "x2", "x1", "x5");
    r#gen.emit("\tmov\tx3, #0");
    r#gen.emit("\tbl\tstone.sort_keys");
    r#gen.emit("\tb\t.Lradix_part_next");
    r#gen.emit(".Lradix_part_radix:");
    r#gen.emit("\tstp\tx0, x2, [sp, #-16]!");
    r#gen.emit("\tmov\tx5, x0");
    r#gen.emit("\tmov\tx0, x1");
    r#gen.emit("\tmov\tx1, x5");
    r#gen.emit("\tmov\tx3, x22");
    r#gen.emit("\tmov\tx4, x23");
    r#gen.emit("\tbl\tstone.radix_range");
    r#gen.emit("\tldp\tx1, x2, [sp], #16");
    // a part that ended in the new buffer is copied back
    r#gen.emit("\tcmp\tx0, x1");
    r#gen.emit("\tb.eq\t.Lradix_part_next");
    copy_words(r#gen, ".Lradix_part_back", ["x1", "x0", "x2", "x6", "x7"]);
    r#gen.emit(".Lradix_part_next:");
    r#gen.emit("\tadd\tx25, x25, #1");
    r#gen.emit("\tcmp\tx25, #256");
    r#gen.emit("\tb.lo\t.Lradix_part");
    r#gen.emit(".Lradix_free:");
    r#gen.emit("\tmov\tx9, x24");
    r#gen.emit("\tbl\tstone.mem_free");
    pop_frame(r#gen, &saved);
}

/// Emits `stone.radix_range`, an LSD radix sort of the `x2` keys at `x0`, which may move them
/// to the as many places at `x1`, by the bytes below shift `x4` of each key minus `x3`, as x64's
/// does. It returns in `x0` whichever of `x0` and `x1` holds the sorted keys.
fn radix_range(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the keys, x20 the other buffer, x21 the length, x22 what to subtract, x23 the
    // shift past the last byte to sort by, x12 the counts, x1 the shift of this pass, and x13
    // its counts
    let saved = ["x19", "x20", "x21", "x22", "x23"];
    r#gen.emit("stone.radix_range:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tmov\tx21, x2");
    r#gen.emit("\tmov\tx22, x3");
    r#gen.emit("\tmov\tx23, x4");
    // clear the counts of the bytes it sorts by
    address(r#gen, "x12", "stone.sort_counts");
    r#gen.emit("\tlsl\tx11, x23, #5");
    zero_words(r#gen, ".Lradix_range_zero", ["x12", "x11", "x10"]);
    // count those bytes of every key in one pass
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit(".Lradix_range_count:");
    r#gen.emit("\tldr\tx0, [x19, x1, lsl #3]");
    r#gen.emit("\tsub\tx0, x0, x22");
    for byte in 0..8 {
        r#gen.emit("\tand\tx5, x0, #0xff");
        r#gen.emit("\tadd\tx6, x12, x5, lsl #3");
        r#gen.emit(&format!("\tldr\tx7, [x6, #{}]", byte * 2048));
        r#gen.emit("\tadd\tx7, x7, #1");
        r#gen.emit(&format!("\tstr\tx7, [x6, #{}]", byte * 2048));
        if byte < 7 {
            r#gen.emit(&format!("\tcmp\tx23, #{}", 8 * (byte + 1)));
            r#gen.emit("\tb.eq\t.Lradix_range_counted");
            r#gen.emit("\tlsr\tx0, x0, #8");
        }
    }
    r#gen.emit(".Lradix_range_counted:");
    r#gen.emit("\tadd\tx1, x1, #1");
    r#gen.emit("\tcmp\tx1, x21");
    r#gen.emit("\tb.lo\t.Lradix_range_count");
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit(".Lradix_range_pass:");
    r#gen.emit("\tadd\tx13, x12, x1, lsl #8");
    // a byte every key shares leaves the order as it is
    r#gen.emit("\tldr\tx0, [x19]");
    r#gen.emit("\tsub\tx0, x0, x22");
    r#gen.emit("\tlsr\tx0, x0, x1");
    r#gen.emit("\tand\tx0, x0, #0xff");
    r#gen.emit("\tldr\tx5, [x13, x0, lsl #3]");
    r#gen.emit("\tcmp\tx5, x21");
    r#gen.emit("\tb.eq\t.Lradix_range_next");
    // each count becomes where the first key with that byte goes
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tmov\tx2, #0");
    r#gen.emit(".Lradix_range_offset:");
    r#gen.emit("\tldr\tx5, [x13, x2, lsl #3]");
    r#gen.emit("\tstr\tx0, [x13, x2, lsl #3]");
    r#gen.emit("\tadd\tx0, x0, x5");
    r#gen.emit("\tadd\tx2, x2, #1");
    r#gen.emit("\tcmp\tx2, #256");
    r#gen.emit("\tb.lo\t.Lradix_range_offset");
    // move each key, in order, to the next place for its byte
    r#gen.emit("\tmov\tx2, #0");
    r#gen.emit(".Lradix_range_move:");
    r#gen.emit("\tldr\tx0, [x19, x2, lsl #3]");
    r#gen.emit("\tsub\tx5, x0, x22");
    r#gen.emit("\tlsr\tx5, x5, x1");
    r#gen.emit("\tand\tx5, x5, #0xff");
    r#gen.emit("\tldr\tx6, [x13, x5, lsl #3]");
    r#gen.emit("\tstr\tx0, [x20, x6, lsl #3]");
    r#gen.emit("\tadd\tx6, x6, #1");
    r#gen.emit("\tstr\tx6, [x13, x5, lsl #3]");
    r#gen.emit("\tadd\tx2, x2, #1");
    r#gen.emit("\tcmp\tx2, x21");
    r#gen.emit("\tb.lo\t.Lradix_range_move");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tmov\tx19, x20");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit(".Lradix_range_next:");
    r#gen.emit("\tadd\tx1, x1, #8");
    r#gen.emit("\tcmp\tx1, x23");
    r#gen.emit("\tb.lo\t.Lradix_range_pass");
    r#gen.emit("\tmov\tx0, x19");
    pop_frame(r#gen, &saved);
}

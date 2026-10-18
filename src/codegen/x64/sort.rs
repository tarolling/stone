//! The x86-64 runtime's `sort`, which puts a list of ints, floats, or strings in ascending order
//! in place.
//!
//! Ints and floats are sorted as unsigned 64-bit keys whose order is theirs: an int with its
//! sign bit flipped, and a float as [`stdlib::float_key`] makes it, every NaN becoming one key
//! above infinity. A first pass writes the keys over the elements and counts the descents, so a
//! list already in order, or in strictly reverse order, takes no more than that pass, a reversal,
//! and a last pass that writes the values back. The first pass also finds the smallest and
//! largest keys, and a list long enough for the bytes of their difference (see
//! [`SORT_RADIX_FACTOR`]) goes to a radix sort by the key minus the smallest, a pass per byte, so ints of a
//! small range take one or two passes rather than eight. Shorter lists go to the quicksort.
//!
//! Strings go to the same quicksort, comparing their bytes eight at a time.
//!
//! The quicksort follows pdqsort and Rust's ipnsort: it takes the median of three elements as
//! the pivot (or of three medians of three for long ranges), partitions without branching on the
//! comparisons, sorts short ranges by insertion, recurses into the shorter side and loops on the
//! longer, so its stack stays logarithmic, and falls back to heapsort past `2 * log2(n)` levels,
//! so it never takes more than `n log n` time. A range whose pivot equals the element just before
//! it, which is no greater than any element of the range, holds only copies of that value on its
//! left, which one pass moves aside, so lists of few distinct values sort in linear time.
//!
//! The order is total on every value a program can tell apart, so the result is the same as the
//! interpreter's, though neither sort is stable.
//!
//! [`stdlib::float_key`]: crate::stdlib::float_key

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

    /// Emits code that sets the carry flag if the element in `x` comes before the one in `y`,
    /// changing no register but `r10` and `r11`, which the quicksort keeps nothing in.
    fn less(self, r#gen: &mut dyn AssemblyGenerator, x: &str, y: &str) {
        match self {
            Elements::Keys => r#gen.emit(&format!("\tcmp\t{x}, {y}")),
            Elements::Strs => {
                r#gen.emit(&format!("\tmov\tr10, {x}"));
                r#gen.emit(&format!("\tmov\tr11, {y}"));
                r#gen.emit("\tcall\tstone.sort_str_less");
            }
        }
    }
}

/// Emits the sort routines among `labels`, which are the `context::LIST_ROUTINES` a program
/// calls. Each takes the list in `rdi`, which the caller has made unique, and follows the System V
/// calling convention.
///
/// - `stone.list_sort_int` sorts a list of ints.
/// - `stone.list_sort_float` sorts a list of floats, writing every NaN back as the quiet NaN
///   `0x7ff8000000000000`, which a program cannot tell from any other NaN.
/// - `stone.list_sort_str` sorts a list of strings by their bytes.
///
/// The radix sort borrows its buffer from `stone.mem_alloc`, so the allocator must be emitted.
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

/// Emits `stone.list_sort_int` and `stone.list_sort_float` (the ones asked for) and
/// `stone.sort_numbers`, which they share: it sorts list `rdi` of ints, or of floats if `rsi` is
/// 1, by turning them into keys and back.
fn sort_numbers(r#gen: &mut dyn AssemblyGenerator, ints: bool, floats: bool) {
    if ints {
        r#gen.emit("stone.list_sort_int:");
        r#gen.emit("\txor\tesi, esi");
        r#gen.emit("\tjmp\tstone.sort_numbers");
    }
    if floats {
        r#gen.emit("stone.list_sort_float:");
        r#gen.emit("\tmov\tesi, 1");
        r#gen.emit("\tjmp\tstone.sort_numbers");
    }
    // rbx holds the list, r12 whether it holds floats, r13 its length, and r14 its elements
    r#gen.emit("stone.sort_numbers:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tpush\tr14");
    r#gen.emit("\tpush\tr15");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\tr13, QWORD PTR [rbx]");
    r#gen.emit("\tcmp\tr13, 2");
    r#gen.emit("\tjb\t.Lsort_numbers_out");
    r#gen.emit("\tmov\tr14, QWORD PTR [rbx + 16]");
    // write each key over its value, counting in r10 the keys smaller than the one before (r9),
    // and keeping the smallest key in rsi and the largest in rdi
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\txor\tr9d, r9d");
    r#gen.emit("\txor\tr10d, r10d");
    r#gen.emit("\tmov\trsi, -1");
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjnz\t.Lsort_numbers_floats");
    r#gen.emit(".Lsort_numbers_ints:");
    r#gen.emit("\tmov\trax, QWORD PTR [r14 + rcx * 8]");
    r#gen.emit("\tbtc\trax, 63");
    r#gen.emit("\tmov\tQWORD PTR [r14 + rcx * 8], rax");
    r#gen.emit("\tcmp\trax, r9");
    r#gen.emit("\tadc\tr10, 0");
    r#gen.emit("\tmov\tr9, rax");
    r#gen.emit("\tcmp\trax, rsi");
    r#gen.emit("\tcmovb\trsi, rax");
    r#gen.emit("\tcmp\trax, rdi");
    r#gen.emit("\tcmova\trdi, rax");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tcmp\trcx, r13");
    r#gen.emit("\tjb\t.Lsort_numbers_ints");
    r#gen.emit("\tjmp\t.Lsort_numbers_keyed");
    r#gen.emit(".Lsort_numbers_floats:");
    r#gen.emit(&format!("\tmovabs\tr11, {INFINITY_BITS}"));
    r#gen.emit(&format!("\tmovabs\tr15, {}", FLOAT_KEY_NAN as i64));
    r#gen.emit(".Lsort_numbers_float:");
    r#gen.emit("\tmov\trax, QWORD PTR [r14 + rcx * 8]");
    r#gen.emit("\tmov\trdx, rax");
    r#gen.emit("\tbtr\trdx, 63");
    // a negative float's bits all flip, a positive one's sign only
    r#gen.emit("\tmov\tr8, rax");
    r#gen.emit("\tsar\tr8, 63");
    r#gen.emit("\tbts\tr8, 63");
    r#gen.emit("\txor\trax, r8");
    r#gen.emit("\tcmp\trdx, r11");
    r#gen.emit("\tcmova\trax, r15"); // every nan is one value
    r#gen.emit("\tmov\tQWORD PTR [r14 + rcx * 8], rax");
    r#gen.emit("\tcmp\trax, r9");
    r#gen.emit("\tadc\tr10, 0");
    r#gen.emit("\tmov\tr9, rax");
    r#gen.emit("\tcmp\trax, rsi");
    r#gen.emit("\tcmovb\trsi, rax");
    r#gen.emit("\tcmp\trax, rdi");
    r#gen.emit("\tcmova\trdi, rax");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tcmp\trcx, r13");
    r#gen.emit("\tjb\t.Lsort_numbers_float");
    r#gen.emit(".Lsort_numbers_keyed:");
    r#gen.emit("\ttest\tr10, r10");
    r#gen.emit("\tjz\t.Lsort_numbers_unkey"); // already in order
    r#gen.emit("\tlea\trax, [r13 - 1]");
    r#gen.emit("\tcmp\tr10, rax");
    r#gen.emit("\tje\t.Lsort_numbers_reverse"); // every key is smaller than the one before
    // radix pays once there are enough keys for each byte it sorts by
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tsub\trax, rsi");
    r#gen.emit("\tbsr\trax, rax");
    r#gen.emit("\tor\trax, 7");
    r#gen.emit("\tinc\trax");
    r#gen.emit("\timul\trax, rax");
    r#gen.emit(&format!("\timul\trax, rax, {SORT_RADIX_FACTOR}"));
    r#gen.emit("\tshr\trax, 6");
    r#gen.emit("\tcmp\tr13, rax");
    r#gen.emit("\tjae\t.Lsort_numbers_radix");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit("\tbsr\trdx, r13");
    r#gen.emit("\tadd\trdx, rdx");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tcall\tstone.sort_keys");
    r#gen.emit("\tjmp\t.Lsort_numbers_unkey");
    r#gen.emit(".Lsort_numbers_radix:");
    // the sorted keys may end up in another buffer
    r#gen.emit("\tmov\trdx, rdi");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tcall\tstone.radix_keys");
    r#gen.emit("\tmov\tr14, QWORD PTR [rbx + 16]");
    r#gen.emit("\tjmp\t.Lsort_numbers_unkey");
    r#gen.emit(".Lsort_numbers_reverse:");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tlea\trdx, [r13 - 1]");
    r#gen.emit(".Lsort_numbers_swap:");
    r#gen.emit("\tcmp\trcx, rdx");
    r#gen.emit("\tjae\t.Lsort_numbers_unkey");
    r#gen.emit("\tmov\trax, QWORD PTR [r14 + rcx * 8]");
    r#gen.emit("\tmov\tr8, QWORD PTR [r14 + rdx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [r14 + rcx * 8], r8");
    r#gen.emit("\tmov\tQWORD PTR [r14 + rdx * 8], rax");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tdec\trdx");
    r#gen.emit("\tjmp\t.Lsort_numbers_swap");
    // write each value back over its key
    r#gen.emit(".Lsort_numbers_unkey:");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjnz\t.Lsort_numbers_unkey_floats");
    r#gen.emit(".Lsort_numbers_unkey_int:");
    r#gen.emit("\tbtc\tQWORD PTR [r14 + rcx * 8], 63");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tcmp\trcx, r13");
    r#gen.emit("\tjb\t.Lsort_numbers_unkey_int");
    r#gen.emit("\tjmp\t.Lsort_numbers_out");
    r#gen.emit(".Lsort_numbers_unkey_floats:");
    r#gen.emit("\tmov\trax, QWORD PTR [r14 + rcx * 8]");
    // a key with its top bit set was a positive float, whose sign only flips back
    r#gen.emit("\tmov\tr8, rax");
    r#gen.emit("\tsar\tr8, 63");
    r#gen.emit("\tnot\tr8");
    r#gen.emit("\tbts\tr8, 63");
    r#gen.emit("\txor\trax, r8");
    r#gen.emit("\tmov\tQWORD PTR [r14 + rcx * 8], rax");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tcmp\trcx, r13");
    r#gen.emit("\tjb\t.Lsort_numbers_unkey_floats");
    r#gen.emit(".Lsort_numbers_out:");
    r#gen.emit("\tpop\tr15");
    r#gen.emit("\tpop\tr14");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\txor\teax, eax"); // sort returns none
    r#gen.emit("\tret");
}

/// Emits `stone.list_sort_str`, which checks whether list `rdi` of strings is already in order,
/// or in strictly reverse order, before handing it to the quicksort.
fn list_sort_str(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the elements and r12 the length
    r#gen.emit("stone.list_sort_str:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tmov\tr12, QWORD PTR [rdi]");
    r#gen.emit("\tmov\trbx, QWORD PTR [rdi + 16]");
    r#gen.emit("\tmov\tecx, 1");
    r#gen.emit(".Lsort_str_ascending:");
    r#gen.emit("\tcmp\trcx, r12");
    r#gen.emit("\tjae\t.Lsort_str_out"); // already in order, or shorter than 2
    Elements::Strs.less(
        r#gen,
        "QWORD PTR [rbx + rcx * 8]",
        "QWORD PTR [rbx + rcx * 8 - 8]",
    );
    r#gen.emit("\tjb\t.Lsort_str_descends");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tjmp\t.Lsort_str_ascending");
    // only a list whose first two strings descend can be in reverse order
    r#gen.emit(".Lsort_str_descends:");
    r#gen.emit("\tcmp\trcx, 1");
    r#gen.emit("\tjne\t.Lsort_str_quicksort");
    r#gen.emit(".Lsort_str_descending:");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tcmp\trcx, r12");
    r#gen.emit("\tjae\t.Lsort_str_reverse");
    Elements::Strs.less(
        r#gen,
        "QWORD PTR [rbx + rcx * 8]",
        "QWORD PTR [rbx + rcx * 8 - 8]",
    );
    r#gen.emit("\tjb\t.Lsort_str_descending");
    r#gen.emit(".Lsort_str_quicksort:");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tmov\trsi, r12");
    r#gen.emit("\tbsr\trdx, r12");
    r#gen.emit("\tadd\trdx, rdx");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tcall\tstone.sort_strs");
    r#gen.emit("\tjmp\t.Lsort_str_out");
    r#gen.emit(".Lsort_str_reverse:");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tlea\trdx, [r12 - 1]");
    r#gen.emit(".Lsort_str_swap:");
    r#gen.emit("\tcmp\trcx, rdx");
    r#gen.emit("\tjae\t.Lsort_str_out");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + rcx * 8]");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + rdx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rbx + rcx * 8], r8");
    r#gen.emit("\tmov\tQWORD PTR [rbx + rdx * 8], rax");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tdec\trdx");
    r#gen.emit("\tjmp\t.Lsort_str_swap");
    r#gen.emit(".Lsort_str_out:");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\txor\teax, eax"); // sort returns none
    r#gen.emit("\tret");
}

/// Emits `stone.sort_str_less`, which sets the carry flag if string `r10` comes before string
/// `r11`: at the first byte where they differ, the one with the smaller byte, or else the
/// shorter. It changes no register.
fn str_less(r#gen: &mut dyn AssemblyGenerator) {
    // rax holds the position, rcx and rdx the lengths, and r8 the shorter one
    r#gen.emit("stone.sort_str_less:");
    r#gen.emit("\tpush\trax");
    r#gen.emit("\tpush\trcx");
    r#gen.emit("\tpush\trdx");
    r#gen.emit("\tpush\tr8");
    r#gen.emit("\tpush\tr9");
    r#gen.emit("\tmov\trcx, QWORD PTR [r10 - 16]");
    r#gen.emit("\tmov\trdx, QWORD PTR [r11 - 16]");
    r#gen.emit("\tmov\tr8, rcx");
    r#gen.emit("\tcmp\trdx, rcx");
    r#gen.emit("\tcmovb\tr8, rdx");
    r#gen.emit("\txor\teax, eax");
    // eight bytes at a time while both strings have them
    r#gen.emit(".Lsort_str_less_words:");
    r#gen.emit("\tlea\tr9, [rax + 8]");
    r#gen.emit("\tcmp\tr9, r8");
    r#gen.emit("\tja\t.Lsort_str_less_bytes");
    r#gen.emit("\tmov\tr9, QWORD PTR [r10 + rax]");
    r#gen.emit("\txor\tr9, QWORD PTR [r11 + rax]");
    r#gen.emit("\tjnz\t.Lsort_str_less_differ");
    r#gen.emit("\tadd\trax, 8");
    r#gen.emit("\tjmp\t.Lsort_str_less_words");
    // the lowest set bit of the difference is in the first byte that differs
    r#gen.emit(".Lsort_str_less_differ:");
    r#gen.emit("\tbsf\tr9, r9");
    r#gen.emit("\tshr\tr9, 3");
    r#gen.emit("\tadd\trax, r9");
    r#gen.emit("\tmovzx\tr9d, BYTE PTR [r10 + rax]");
    r#gen.emit("\tcmp\tr9b, BYTE PTR [r11 + rax]");
    r#gen.emit("\tjmp\t.Lsort_str_less_out");
    r#gen.emit(".Lsort_str_less_bytes:");
    r#gen.emit("\tcmp\trax, r8");
    r#gen.emit("\tjae\t.Lsort_str_less_lengths");
    r#gen.emit("\tmovzx\tr9d, BYTE PTR [r10 + rax]");
    r#gen.emit("\tcmp\tr9b, BYTE PTR [r11 + rax]");
    r#gen.emit("\tjne\t.Lsort_str_less_out");
    r#gen.emit("\tinc\trax");
    r#gen.emit("\tjmp\t.Lsort_str_less_bytes");
    r#gen.emit(".Lsort_str_less_lengths:");
    r#gen.emit("\tcmp\trcx, rdx");
    // popping leaves the flags alone
    r#gen.emit(".Lsort_str_less_out:");
    r#gen.emit("\tpop\tr9");
    r#gen.emit("\tpop\tr8");
    r#gen.emit("\tpop\trdx");
    r#gen.emit("\tpop\trcx");
    r#gen.emit("\tpop\trax");
    r#gen.emit("\tret");
}

/// Emits `code` with each `{}` replaced by the routine's label prefix.
fn emit_with(r#gen: &mut dyn AssemblyGenerator, prefix: &str, code: &str) {
    r#gen.emit(&code.replace("{}", prefix));
}

/// Emits code that swaps the elements at `a` and `b` if the one at `b` comes first, without a
/// branch.
fn sort2(r#gen: &mut dyn AssemblyGenerator, elements: Elements, a: &str, b: &str) {
    r#gen.emit(&format!("\tmov\tr8, {a}"));
    r#gen.emit(&format!("\tmov\tr9, {b}"));
    elements.less(r#gen, "r9", "r8");
    r#gen.emit("\tmov\trdx, r8");
    r#gen.emit("\tcmovb\tr8, r9");
    r#gen.emit("\tcmovb\tr9, rdx");
    r#gen.emit(&format!("\tmov\t{a}, r8"));
    r#gen.emit(&format!("\tmov\t{b}, r9"));
}

/// Emits code that puts the elements at positions `rdi`, `rsi`, and `rcx` of `rbx` in order.
fn sort3(r#gen: &mut dyn AssemblyGenerator, elements: Elements) {
    let a = "QWORD PTR [rbx + rdi * 8]";
    let b = "QWORD PTR [rbx + rsi * 8]";
    let c = "QWORD PTR [rbx + rcx * 8]";
    sort2(r#gen, elements, a, b);
    sort2(r#gen, elements, b, c);
    sort2(r#gen, elements, a, b);
}

/// Emits `stone.sort_keys` or `stone.sort_strs`, a quicksort of the `rsi` elements at `rdi`,
/// falling back to heapsort after `rdx` more levels. `rcx` is 1 if the element before `rdi` is
/// part of the list, and so no greater than any of them. It preserves `rbx`, `rbp`, and
/// `r12`-`r15`.
fn quicksort(r#gen: &mut dyn AssemblyGenerator, elements: Elements) {
    let name = elements.name();
    let prefix = format!(".Lsort_{name}");
    let emit = |r#gen: &mut dyn AssemblyGenerator, code: &str| emit_with(r#gen, &prefix, code);
    // rbx holds the first element, r12 the length, r13 the levels left, r14 whether the element
    // before the first is in the list, and r15 the pivot
    r#gen.emit(&format!("stone.sort_{name}:"));
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tpush\tr14");
    r#gen.emit("\tpush\tr15");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\tr13, rdx");
    r#gen.emit("\tmov\tr14, rcx");
    emit(r#gen, "{}_loop:");
    r#gen.emit(&format!("\tcmp\tr12, {SORT_INSERTION_MAX}"));
    emit(r#gen, "\tjbe\t{}_insertion");
    r#gen.emit("\ttest\tr13, r13");
    emit(r#gen, "\tjz\t{}_heap");
    r#gen.emit("\tdec\tr13");
    // the pivot is the median of the first, middle, and last elements, or for a long range of
    // the medians of three triples around them, which ends up in the middle (rax)
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tshr\trax, 1");
    r#gen.emit("\tcmp\tr12, 128");
    emit(r#gen, "\tjbe\t{}_median");
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\tlea\trcx, [r12 - 1]");
    sort3(r#gen, elements);
    r#gen.emit("\tmov\tedi, 1");
    r#gen.emit("\tlea\trsi, [rax - 1]");
    r#gen.emit("\tlea\trcx, [r12 - 2]");
    sort3(r#gen, elements);
    r#gen.emit("\tmov\tedi, 2");
    r#gen.emit("\tlea\trsi, [rax + 1]");
    r#gen.emit("\tlea\trcx, [r12 - 3]");
    sort3(r#gen, elements);
    r#gen.emit("\tlea\trdi, [rax - 1]");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\tlea\trcx, [rax + 1]");
    sort3(r#gen, elements);
    emit(r#gen, "\tjmp\t{}_pivot");
    emit(r#gen, "{}_median:");
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\tlea\trcx, [r12 - 1]");
    sort3(r#gen, elements);
    // move the pivot to the front, out of the way of the partition
    emit(r#gen, "{}_pivot:");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx]");
    r#gen.emit("\tmov\tr15, QWORD PTR [rbx + rax * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rbx], r15");
    r#gen.emit("\tmov\tQWORD PTR [rbx + rax * 8], r8");
    // a pivot equal to the element before the range is its smallest value
    r#gen.emit("\ttest\tr14, r14");
    emit(r#gen, "\tjz\t{}_partition");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx - 8]");
    elements.less(r#gen, "r8", "r15");
    emit(r#gen, "\tjae\t{}_equal");
    // swap each element (rax) to the end of those before the pivot (rdx) and count it there if
    // it is, so the comparison decides no branch
    emit(r#gen, "{}_partition:");
    r#gen.emit("\tmov\tecx, 1");
    r#gen.emit("\tmov\tedx, 1");
    emit(r#gen, "{}_partition_loop:");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + rcx * 8]");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + rdx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rbx + rcx * 8], r8");
    r#gen.emit("\tmov\tQWORD PTR [rbx + rdx * 8], rax");
    elements.less(r#gen, "rax", "r15");
    r#gen.emit("\tadc\trdx, 0");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tcmp\trcx, r12");
    emit(r#gen, "\tjb\t{}_partition_loop");
    // the pivot goes between the two sides, at rdx
    r#gen.emit("\tdec\trdx");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + rdx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rbx], rax");
    r#gen.emit("\tmov\tQWORD PTR [rbx + rdx * 8], r15");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tsub\trcx, rdx");
    r#gen.emit("\tdec\trcx");
    // sort the shorter side by recursion and the longer by looping
    r#gen.emit("\tcmp\trdx, rcx");
    emit(r#gen, "\tja\t{}_right_first");
    r#gen.emit("\tpush\trdx");
    r#gen.emit("\tpush\trcx");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tmov\trsi, rdx");
    r#gen.emit("\tmov\trdx, r13");
    r#gen.emit("\tmov\trcx, r14");
    r#gen.emit(&format!("\tcall\tstone.sort_{name}"));
    r#gen.emit("\tpop\trcx");
    r#gen.emit("\tpop\trdx");
    r#gen.emit("\tlea\trbx, [rbx + rdx * 8 + 8]");
    r#gen.emit("\tmov\tr12, rcx");
    r#gen.emit("\tmov\tr14d, 1");
    emit(r#gen, "\tjmp\t{}_loop");
    emit(r#gen, "{}_right_first:");
    r#gen.emit("\tpush\trdx");
    r#gen.emit("\tpush\trcx");
    r#gen.emit("\tlea\trdi, [rbx + rdx * 8 + 8]");
    r#gen.emit("\tmov\trsi, rcx");
    r#gen.emit("\tmov\trdx, r13");
    r#gen.emit("\tmov\tecx, 1");
    r#gen.emit(&format!("\tcall\tstone.sort_{name}"));
    r#gen.emit("\tpop\trcx");
    r#gen.emit("\tpop\trdx");
    r#gen.emit("\tmov\tr12, rdx");
    emit(r#gen, "\tjmp\t{}_loop");
    // gather every element no greater than the pivot, which are all equal to it, at the front,
    // where they are in order already
    emit(r#gen, "{}_equal:");
    r#gen.emit("\tmov\tecx, 1");
    r#gen.emit("\tmov\tedx, 1");
    emit(r#gen, "{}_equal_loop:");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + rcx * 8]");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + rdx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rbx + rcx * 8], r8");
    r#gen.emit("\tmov\tQWORD PTR [rbx + rdx * 8], rax");
    elements.less(r#gen, "r15", "rax");
    r#gen.emit("\tsbb\tr9, r9");
    r#gen.emit("\tlea\trdx, [rdx + r9 + 1]");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tcmp\trcx, r12");
    emit(r#gen, "\tjb\t{}_equal_loop");
    r#gen.emit("\tlea\trbx, [rbx + rdx * 8]");
    r#gen.emit("\tsub\tr12, rdx");
    emit(r#gen, "\tjmp\t{}_loop");
    // insertion sort, moving each element (rax) down past the greater ones before it
    emit(r#gen, "{}_insertion:");
    r#gen.emit("\tmov\tecx, 1");
    emit(r#gen, "{}_insert:");
    r#gen.emit("\tcmp\trcx, r12");
    emit(r#gen, "\tjae\t{}_done");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + rcx * 8]");
    r#gen.emit("\tmov\trdx, rcx");
    emit(r#gen, "{}_insert_step:");
    r#gen.emit("\ttest\trdx, rdx");
    emit(r#gen, "\tjz\t{}_insert_place");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + rdx * 8 - 8]");
    elements.less(r#gen, "rax", "r8");
    emit(r#gen, "\tjae\t{}_insert_place");
    r#gen.emit("\tmov\tQWORD PTR [rbx + rdx * 8], r8");
    r#gen.emit("\tdec\trdx");
    emit(r#gen, "\tjmp\t{}_insert_step");
    emit(r#gen, "{}_insert_place:");
    r#gen.emit("\tmov\tQWORD PTR [rbx + rdx * 8], rax");
    r#gen.emit("\tinc\trcx");
    emit(r#gen, "\tjmp\t{}_insert");
    // heapsort: make a max-heap, then move its top to the end until it is empty
    emit(r#gen, "{}_heap:");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tshr\trcx, 1");
    emit(r#gen, "{}_heapify:");
    r#gen.emit("\ttest\trcx, rcx");
    emit(r#gen, "\tjz\t{}_heap_ready");
    r#gen.emit("\tdec\trcx");
    r#gen.emit("\tmov\trsi, rcx");
    r#gen.emit("\tmov\trdi, r12");
    emit(r#gen, "\tcall\t{}_sift");
    emit(r#gen, "\tjmp\t{}_heapify");
    emit(r#gen, "{}_heap_ready:");
    r#gen.emit("\tmov\trcx, r12");
    emit(r#gen, "{}_heap_pop:");
    r#gen.emit("\tdec\trcx");
    emit(r#gen, "\tjz\t{}_done");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx]");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + rcx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rbx], r8");
    r#gen.emit("\tmov\tQWORD PTR [rbx + rcx * 8], rax");
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit("\tmov\trdi, rcx");
    emit(r#gen, "\tcall\t{}_sift");
    emit(r#gen, "\tjmp\t{}_heap_pop");
    emit(r#gen, "{}_done:");
    r#gen.emit("\tpop\tr15");
    r#gen.emit("\tpop\tr14");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");
    // moves the element at rsi down the heap of the rdi elements at rbx until no child is
    // greater, preserving rcx
    emit(r#gen, "{}_sift:");
    r#gen.emit("\tmov\tr8, rsi");
    r#gen.emit("\tadd\tr8, r8");
    r#gen.emit("\tinc\tr8");
    r#gen.emit("\tcmp\tr8, rdi");
    emit(r#gen, "\tjae\t{}_sift_done");
    r#gen.emit("\tlea\tr9, [r8 + 1]");
    r#gen.emit("\tcmp\tr9, rdi");
    emit(r#gen, "\tjae\t{}_sift_child");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + r8 * 8]");
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + r9 * 8]");
    elements.less(r#gen, "rax", "rdx");
    emit(r#gen, "\tjae\t{}_sift_child");
    r#gen.emit("\tmov\tr8, r9");
    emit(r#gen, "{}_sift_child:");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + rsi * 8]");
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + r8 * 8]");
    elements.less(r#gen, "rax", "rdx");
    emit(r#gen, "\tjae\t{}_sift_done");
    r#gen.emit("\tmov\tQWORD PTR [rbx + rsi * 8], rdx");
    r#gen.emit("\tmov\tQWORD PTR [rbx + r8 * 8], rax");
    r#gen.emit("\tmov\trsi, r8");
    emit(r#gen, "\tjmp\t{}_sift");
    emit(r#gen, "{}_sift_done:");
    r#gen.emit("\tret");
}

/// Emits `stone.radix_keys`, which sorts the keys of list `rdi`, which are no smaller than
/// `rsi` and no larger than `rdx`, and not all the same, by the key minus the smallest, using
/// only the bytes up to the highest one the largest such difference has, so `[3, -1, 2]` takes
/// one pass. It borrows a buffer as long as the list from `stone.mem_alloc` and frees whichever
/// of the two the keys do not end in, and the list takes the new one if they end there.
///
/// A list of fewer than [`SORT_SPLIT_MIN`] keys, or one sorted by a single byte, is sorted by
/// [`stone.radix_range`](radix_range) a byte at a time from the lowest. A longer one is first
/// split by its highest byte into the new buffer, and each part, which fits in a cache, is then
/// sorted back into the list's buffer by its other bytes: by insertion if it is shorter than
/// [`SORT_PART_MIN`], or else by `stone.radix_range`, so each key leaves memory twice rather
/// than once per byte. It preserves `rbx`, `rbp`, and `r12`-`r15`.
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
    // rbx holds the list, r12 its length, r13 its keys, r14 the smallest key, r15 the shift past
    // the highest byte to sort by, and [rsp] the new buffer
    r#gen.emit("stone.radix_keys:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tpush\tr14");
    r#gen.emit("\tpush\tr15");
    r#gen.emit("\tsub\trsp, 24");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, QWORD PTR [rbx]");
    r#gen.emit("\tmov\tr13, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\tr14, rsi");
    r#gen.emit("\tsub\trdx, rsi");
    r#gen.emit("\tbsr\tr15, rdx");
    r#gen.emit("\tor\tr15, 7");
    r#gen.emit("\tinc\tr15");
    // the highest byte always takes a pass, since the smallest key has 0 there and the largest
    // does not
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tshl\trdi, 3");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tQWORD PTR [rsp], rax");
    r#gen.emit("\tcmp\tr15, 8");
    r#gen.emit("\tje\t.Lradix_whole");
    r#gen.emit(&format!("\tcmp\tr12, {SORT_SPLIT_MIN}"));
    r#gen.emit("\tjae\t.Lradix_split");
    r#gen.emit(".Lradix_whole:");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tmov\trsi, QWORD PTR [rsp]");
    r#gen.emit("\tmov\trdx, r12");
    r#gen.emit("\tmov\trcx, r14");
    r#gen.emit("\tmov\tr8, r15");
    r#gen.emit("\tcall\tstone.radix_range");
    r#gen.emit("\tcmp\trax, r13");
    r#gen.emit("\tje\t.Lradix_free");
    // the keys ended in the new buffer, so the list takes it
    r#gen.emit("\tmov\tQWORD PTR [rbx + 16], rax");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 8], r12");
    r#gen.emit("\tmov\tQWORD PTR [rsp], r13");
    r#gen.emit("\tjmp\t.Lradix_free");
    // split by the highest byte (shift r15), counting each part's keys
    r#gen.emit(".Lradix_split:");
    r#gen.emit("\tsub\tr15, 8");
    r#gen.emit("\tlea\trdi, [rip + stone.sort_counts]");
    r#gen.emit("\tmov\tecx, 2048");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\trep\tstosb");
    r#gen.emit("\tlea\trsi, [rip + stone.sort_counts]");
    r#gen.emit("\tmov\trcx, r15");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit(".Lradix_split_count:");
    r#gen.emit("\tmov\trax, QWORD PTR [r13 + r8 * 8]");
    r#gen.emit("\tsub\trax, r14");
    r#gen.emit("\tshr\trax, cl");
    r#gen.emit("\tinc\tQWORD PTR [rsi + rax * 8]");
    r#gen.emit("\tinc\tr8");
    r#gen.emit("\tcmp\tr8, r12");
    r#gen.emit("\tjb\t.Lradix_split_count");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit(".Lradix_split_offset:");
    r#gen.emit("\tmov\tr8, QWORD PTR [rsi + rdx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rsi + rdx * 8], rax");
    r#gen.emit("\tadd\trax, r8");
    r#gen.emit("\tinc\trdx");
    r#gen.emit("\tcmp\trdx, 256");
    r#gen.emit("\tjb\t.Lradix_split_offset");
    r#gen.emit("\tmov\tr10, QWORD PTR [rsp]");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit(".Lradix_split_move:");
    r#gen.emit("\tmov\trax, QWORD PTR [r13 + r8 * 8]");
    r#gen.emit("\tmov\trdx, rax");
    r#gen.emit("\tsub\trdx, r14");
    r#gen.emit("\tshr\trdx, cl");
    r#gen.emit("\tmov\tr9, QWORD PTR [rsi + rdx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [r10 + r9 * 8], rax");
    r#gen.emit("\tinc\tr9");
    r#gen.emit("\tmov\tQWORD PTR [rsi + rdx * 8], r9");
    r#gen.emit("\tinc\tr8");
    r#gen.emit("\tcmp\tr8, r12");
    r#gen.emit("\tjb\t.Lradix_split_move");
    // each offset is now where its part ends; keep them, since sorting the parts counts again
    r#gen.emit("\tlea\trdi, [rip + stone.sort_parts]");
    r#gen.emit("\tmov\tecx, 256");
    r#gen.emit("\trep\tmovsq");
    // sort each part back into the list's buffer: [rsp + 8] holds the part and [rsp + 16]
    // where it starts
    r#gen.emit("\tmov\tQWORD PTR [rsp + 8], 0");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 16], 0");
    r#gen.emit(".Lradix_part:");
    r#gen.emit("\tmov\trax, QWORD PTR [rsp + 8]");
    r#gen.emit("\tlea\trcx, [rip + stone.sort_parts]");
    r#gen.emit("\tmov\trdx, QWORD PTR [rcx + rax * 8]");
    r#gen.emit("\tmov\trax, QWORD PTR [rsp + 16]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 16], rdx");
    r#gen.emit("\tsub\trdx, rax"); // its length
    r#gen.emit("\tjz\t.Lradix_part_next");
    r#gen.emit("\tmov\trsi, QWORD PTR [rsp]");
    r#gen.emit("\tlea\trsi, [rsi + rax * 8]");
    r#gen.emit("\tlea\trdi, [r13 + rax * 8]");
    r#gen.emit(&format!("\tcmp\trdx, {SORT_PART_MIN}"));
    r#gen.emit("\tjae\t.Lradix_part_radix");
    // a short part is copied back and sorted where it is
    r#gen.emit("\tmov\trcx, rdx");
    r#gen.emit("\tmov\tr8, rdi");
    r#gen.emit("\trep\tmovsq");
    r#gen.emit("\tmov\trdi, r8");
    r#gen.emit("\tmov\trsi, rdx");
    r#gen.emit("\tbsr\trdx, rdx");
    r#gen.emit("\tadd\trdx, rdx");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tcall\tstone.sort_keys");
    r#gen.emit("\tjmp\t.Lradix_part_next");
    r#gen.emit(".Lradix_part_radix:");
    r#gen.emit("\tpush\trdi");
    r#gen.emit("\tpush\trdx");
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tmov\trdi, rsi");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\tmov\trcx, r14");
    r#gen.emit("\tmov\tr8, r15");
    r#gen.emit("\tcall\tstone.radix_range");
    r#gen.emit("\tpop\trcx");
    r#gen.emit("\tpop\trdi");
    // a part that ended in the new buffer is copied back
    r#gen.emit("\tcmp\trax, rdi");
    r#gen.emit("\tje\t.Lradix_part_next");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\trep\tmovsq");
    r#gen.emit(".Lradix_part_next:");
    r#gen.emit("\tinc\tQWORD PTR [rsp + 8]");
    r#gen.emit("\tcmp\tQWORD PTR [rsp + 8], 256");
    r#gen.emit("\tjb\t.Lradix_part");
    r#gen.emit(".Lradix_free:");
    r#gen.emit("\tmov\trax, QWORD PTR [rsp]");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tadd\trsp, 24");
    r#gen.emit("\tpop\tr15");
    r#gen.emit("\tpop\tr14");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");
}

/// Emits `stone.radix_range`, an LSD radix sort of the `rdx` keys at `rdi`, which may move
/// them to the as many places at `rsi`, by the bytes below shift `r8` of each key minus `rcx`.
/// It skips each byte at which every key has the same, and returns in `rax` whichever of `rdi`
/// and `rsi` holds the sorted keys. It preserves `rbx`, `rbp`, and `r12`-`r15`.
fn radix_range(r#gen: &mut dyn AssemblyGenerator) {
    // r12 holds the length, r13 the keys, r15 the other buffer, rbx what to subtract, r10 the
    // shift past the last byte to sort by, rcx the shift of this pass, and rsi its counts
    r#gen.emit("stone.radix_range:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tpush\tr15");
    r#gen.emit("\tmov\tr13, rdi");
    r#gen.emit("\tmov\tr15, rsi");
    r#gen.emit("\tmov\tr12, rdx");
    r#gen.emit("\tmov\trbx, rcx");
    r#gen.emit("\tmov\tr10, r8");
    // clear the counts of the bytes it sorts by
    r#gen.emit("\tlea\trdi, [rip + stone.sort_counts]");
    r#gen.emit("\tmov\trcx, r10");
    r#gen.emit("\tshl\trcx, 8");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\trep\tstosb");
    // count those bytes of every key in one pass
    r#gen.emit("\tlea\trsi, [rip + stone.sort_counts]");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Lradix_range_count:");
    r#gen.emit("\tmov\trax, QWORD PTR [r13 + rcx * 8]");
    r#gen.emit("\tsub\trax, rbx");
    for byte in 0..8 {
        r#gen.emit("\tmovzx\tedx, al");
        r#gen.emit(&format!(
            "\tinc\tQWORD PTR [rsi + rdx * 8 + {}]",
            byte * 2048
        ));
        if byte < 7 {
            r#gen.emit(&format!("\tcmp\tr10, {}", 8 * (byte + 1)));
            r#gen.emit("\tje\t.Lradix_range_counted");
            r#gen.emit("\tshr\trax, 8");
        }
    }
    r#gen.emit(".Lradix_range_counted:");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tcmp\trcx, r12");
    r#gen.emit("\tjb\t.Lradix_range_count");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Lradix_range_pass:");
    r#gen.emit("\tlea\trsi, [rip + stone.sort_counts]");
    r#gen.emit("\tmov\trax, rcx");
    r#gen.emit("\tshl\trax, 8");
    r#gen.emit("\tadd\trsi, rax");
    // a byte every key shares leaves the order as it is
    r#gen.emit("\tmov\trax, QWORD PTR [r13]");
    r#gen.emit("\tsub\trax, rbx");
    r#gen.emit("\tshr\trax, cl");
    r#gen.emit("\tmovzx\teax, al");
    r#gen.emit("\tcmp\tQWORD PTR [rsi + rax * 8], r12");
    r#gen.emit("\tje\t.Lradix_range_next");
    // each count becomes where the first key with that byte goes
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit(".Lradix_range_offset:");
    r#gen.emit("\tmov\tr8, QWORD PTR [rsi + rdx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rsi + rdx * 8], rax");
    r#gen.emit("\tadd\trax, r8");
    r#gen.emit("\tinc\trdx");
    r#gen.emit("\tcmp\trdx, 256");
    r#gen.emit("\tjb\t.Lradix_range_offset");
    // move each key, in order, to the next place for its byte
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit(".Lradix_range_move:");
    r#gen.emit("\tmov\trax, QWORD PTR [r13 + r8 * 8]");
    r#gen.emit("\tmov\trdx, rax");
    r#gen.emit("\tsub\trdx, rbx");
    r#gen.emit("\tshr\trdx, cl");
    r#gen.emit("\tmovzx\tedx, dl");
    r#gen.emit("\tmov\tr9, QWORD PTR [rsi + rdx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [r15 + r9 * 8], rax");
    r#gen.emit("\tinc\tr9");
    r#gen.emit("\tmov\tQWORD PTR [rsi + rdx * 8], r9");
    r#gen.emit("\tinc\tr8");
    r#gen.emit("\tcmp\tr8, r12");
    r#gen.emit("\tjb\t.Lradix_range_move");
    r#gen.emit("\tmov\trax, r13");
    r#gen.emit("\tmov\tr13, r15");
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit(".Lradix_range_next:");
    r#gen.emit("\tadd\trcx, 8");
    r#gen.emit("\tcmp\trcx, r10");
    r#gen.emit("\tjb\t.Lradix_range_pass");
    r#gen.emit("\tmov\trax, r13");
    r#gen.emit("\tpop\tr15");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");
}

use crate::generators::common::AssemblyGenerator;

pub fn print(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("print:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx"); // Save caller-saved registers
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tsub\trsp, 32");

    // Check if rdi points to a string (heuristic: if value > 0x1000, treat as pointer)
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tcmp\trax, 0x1000");
    r#gen.emit("\tjae\t.Lprint_string");

    // Print integer
    r#gen.emit(".Lprint_int:");
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tmov\trcx, 10");
    r#gen.emit("\tlea\trsi, [rbp - 1]");
    r#gen.emit("\tmov\tbyte ptr [rsi], 10"); // newline

    // Handle negative numbers
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjns\t.Lprint_positive");
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr12, 1"); // flag for negative
    r#gen.emit("\tjmp\t.Lprint_convert");

    r#gen.emit(".Lprint_positive:");
    r#gen.emit("\txor\tr12, r12");

    r#gen.emit(".Lprint_convert:");
    r#gen.emit("\ttest\trax, rax"); // Handle zero case
    r#gen.emit("\tjnz\t.Lprint_loop");
    r#gen.emit("\tdec\trsi");
    r#gen.emit("\tmov\tbyte ptr [rsi], 48"); // '0'
    r#gen.emit("\tjmp\t.Lprint_sign");

    r#gen.emit(".Lprint_loop:");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lprint_sign");
    r#gen.emit("\tdec\trsi");
    r#gen.emit("\txor\trdx, rdx");
    r#gen.emit("\tdiv\trcx");
    r#gen.emit("\tadd\tdl, 48"); // '0'
    r#gen.emit("\tmov\t[rsi], dl");
    r#gen.emit("\tjmp\t.Lprint_loop");

    r#gen.emit(".Lprint_sign:");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjz\t.Lprint_write");
    r#gen.emit("\tdec\trsi");
    r#gen.emit("\tmov\tbyte ptr [rsi], 45"); // '-'
    r#gen.emit("\tjmp\t.Lprint_write");

    // Print string
    r#gen.emit(".Lprint_string:");
    r#gen.emit("\tmov\trsi, rdi"); // String pointer

    // Calculate string length (find null terminator)
    r#gen.emit("\txor\trdx, rdx");
    r#gen.emit(".Lstrlen_loop:");
    r#gen.emit("\tcmp\tbyte ptr [rsi + rdx], 0");
    r#gen.emit("\tje\t.Lstrlen_done");
    r#gen.emit("\tinc\trdx");
    r#gen.emit("\tjmp\t.Lstrlen_loop");

    r#gen.emit(".Lstrlen_done:");
    r#gen.emit("\ttest\trdx, rdx");
    r#gen.emit("\tjz\t.Lprint_done"); // Empty string

    // Add newline
    r#gen.emit("\tmov\tbyte ptr [rsi + rdx], 10");
    r#gen.emit("\tinc\trdx");
    r#gen.emit("\tjmp\t.Lprint_write");

    // sys_write
    r#gen.emit(".Lprint_write:");
    r#gen.emit("\tmov\trax, 1"); // sys_write
    r#gen.emit("\tmov\trdi, 1"); // stdout
    r#gen.emit("\tlea\trdx, [rbp - 1]");
    r#gen.emit("\tsub\trdx, rsi");
    r#gen.emit("\tinc\trdx"); // length
    r#gen.emit("\tsyscall");

    r#gen.emit(".Lprint_done:");
    r#gen.emit("\tadd\trsp, 32");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

pub fn len(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("len:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");

    // rdi points to null-terminated string
    // Calculate length by finding null terminator
    r#gen.emit("\txor\trax, rax");
    r#gen.emit(".Llen_loop:");
    r#gen.emit("\tcmp\tbyte ptr [rdi + rax], 0");
    r#gen.emit("\tje\t.Llen_done");
    r#gen.emit("\tinc\trax");
    r#gen.emit("\tjmp\t.Llen_loop");

    r#gen.emit(".Llen_done:");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

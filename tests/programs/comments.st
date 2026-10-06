// comments run from '//' to the end of the line, and '#' is not a comment
x = 1 // after code
// a comment between statements

def double(n); // after a header
    // inside a block, at the block's indentation
    ret n * 2
// back at column one inside nothing

if x;
// at column one inside a block does not end it
    print(double(x))
        // deeper than the block does not start one
    print("// is only a comment outside strings")
    print(6 / 2) // a single slash still divides

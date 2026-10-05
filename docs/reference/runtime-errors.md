# Runtime errors

When a program fails while running, it prints `error:` and a message to stderr and exits with
status 1. Output printed before the error stays printed. The interpreter prefixes the message
with the file name, and a compiled executable prints it on its own:

```text
$ stone run average.st
7
average.st: error: division by zero
$ stone build average.st -o average && ./average
7
error: division by zero
```

Both backends stop at the same point with the same message, since operands are evaluated left
to right in both.

| message | cause |
| --- | --- |
| `division by zero` | `/` with a divisor of `0`, or `0.0` (as in Python, rather than IEEE 754's infinity) |
| `integer overflow in division` | the smallest int divided by `-1`, which has no int result |
| `list index out of range` | an index at or past the length, or a negative index before the start. The interpreter adds the index and length |
| `cannot convert float to int (nan or out of range)` | `int()` of nan, an infinity, or a float outside the int range |
| `recursion is too deep (more than 1000 nested calls)` | more than 1,000 calls active at once |
| `'name' is used before it is assigned` | a function read a global that had not been assigned yet when it ran |

Integer `+`, `-`, and `*` do not fail: they wrap around on overflow. Float arithmetic follows
IEEE 754 apart from division by zero, so it produces `inf`, `-inf`, and `nan` instead of errors.

A `ret` at the top level is not an error: it ends the program with status 0.

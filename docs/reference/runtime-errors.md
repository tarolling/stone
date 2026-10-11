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
| `division by zero` | `/` or `%` with a divisor of `0`, or `0.0` (as in Python, rather than IEEE 754's infinity or nan) |
| `integer overflow in division` | the smallest int divided by `-1`, which has no int result. `%` by `-1` is always `0` |
| `negative exponent` | `**` with an int base and a negative exponent, which has no int result |
| `list index out of range` | an index at or past the length, or a negative index before the start. The interpreter adds the index and length |
| `cannot convert float to int (nan or out of range)` | `int()` or `math.floor()` of nan, an infinity, or a float outside the int range |
| `invalid literal for int() with base 10: '...'` | `int()` of a string that is not a decimal int, quoted as given |
| `int() argument out of range: '...'` | `int()` of a string holding an int too large or small to fit |
| `could not convert string to float: '...'` | `float()` of a string that is not a float, quoted as given |
| `empty separator` | `split("")` |
| `integer overflow in abs` | `math.abs()` of the smallest int, whose absolute value does not fit in an int |
| `math domain error` | `math.sqrt()` of a negative number, as in Python |
| `min of an empty list` | `math.min()` of an empty list |
| `max of an empty list` | `math.max()` of an empty list |
| `empty range for randint` | `random.randint(low, high)` with `low` greater than `high` |
| `cannot choose from an empty list` | `random.choice()` of an empty list |
| `sleep length must be non-negative` | `time.sleep()` of a negative number or nan |
| `sleep length is too large` | `time.sleep()` of 2^63 seconds or more |
| `recursion is too deep (more than 1000 nested calls)` | more than 1,000 calls active at once |
| `'name' is used before it is assigned` | a function read a global that had not been assigned yet when it ran |

Integer `+`, `-`, `*`, and `**` wrap around on overflow instead of failing. Float arithmetic
follows IEEE 754 apart from division by zero and `math.sqrt` of a negative number, so it produces `inf`, `-inf`, and `nan` instead of
errors.
A float `**` multiplies by repeated squaring and takes the reciprocal for a negative exponent, so
`0.0 ** -1` is `inf`.

A `ret` at the top level is not an error: it ends the program with status 0.

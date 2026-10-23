# Runtime errors

When a program fails while running, it prints the error to stderr the way the checker prints
one, with the file, line, and column, the message, and the source line with carets under the
expression that failed, then exits with status 1. Output printed before the error stays
printed. A compiled executable prints exactly what the interpreter does, since `stone build`
writes each place an error can happen into the executable:

```text
$ stone run average.stn
7
average.stn:5:9: error: division by zero
  |
5 |     ret total / values.len()
  |         ^^^^^^^^^^^^^^^^^^^^
$ stone build average.stn -o average && ./average
7
average.stn:5:9: error: division by zero
  |
5 |     ret total / values.len()
  |         ^^^^^^^^^^^^^^^^^^^^
```

Both backends stop at the same point with the same message, since operands are evaluated left
to right in both. The error points at the innermost expression that failed: the division in
`1 + a / b`, the subscript in `xs[i]`, the call in `int(s)`, `xs.pop(i)`, `x.unwrap()`, `print(x)`, or
`math.sqrt(x)`, and
for `recursion is too deep`, the call that went one level too deep. A change such as
`grid[i][j] = v` or `ps[i].x = v` points at the subscript whose index is out of range. An executable names the
file by the path it was built from.

| message | cause |
| --- | --- |
| `division by zero` | `/` or `%` with a divisor of `0`, or `0.0` (as in Python, rather than IEEE 754's infinity or nan) |
| `integer overflow in division` | the smallest int divided by `-1`, which has no int result. `%` by `-1` is always `0` |
| `negative exponent` | `**` with an int base and a negative exponent, which has no int result |
| `list index out of range (index 5, length 2)` | an index at or past the length, or a negative index before the start, including `insert` at an index below `-len` or above `len`. It gives the index as written and the list's length |
| `key not found: 'pear'` | `m[k]` for a key `m` has no entry for. It gives the key, quoted if it is a string |
| `pop from empty list` | `pop(i)` on an empty list (`pop()` gives `none` instead) |
| `pop index out of range` | `pop(i)` with `i` at or past the length, or before the start |
| `string index out of range (index -4, length 3)` | a string index at or past the length, or a negative index before the start. It gives the index as written and the string's length in bytes |
| `cannot cut a string inside a character` | `s[i]` of a byte that is part of a character outside ASCII, or a slice that would hold only part of one (see [strings](types.md#strings)) |
| `slice step cannot be zero` | a slice such as `xs[::0]` or `s[::0]` |
| `lists have different lengths (3 and 2)` | arithmetic on two lists, such as `a + b`, whose lengths differ. It gives the first list's length and then the first length that differs from it |
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
| `cannot unwrap none` | `unwrap()` of an option that is `none` |
| `cannot unwrap err: ...` | `unwrap()` of a result that is an `err`, followed by the error as `print` writes it, as in `cannot unwrap err: end of input` |
| `cannot unwrap_err ok: ...` | `unwrap_err()` of a result that is an `ok`, followed by the value |
| `value is nested too deeply to print (more than 10000 levels)` | `print` of an enum or struct value inside more than 9,999 others, as in a long linked list. What comes before it on the line stays printed |
| `'name' is used before it is assigned` | a function read a global that had not been assigned yet when it ran |

Arithmetic on lists computes the whole expression for the first element, then the second, and
so on, so an error comes from the first element where any of its operations fails. In
`a / b + c / d`, a zero in `d` at index 1 stops the program before a zero in `b` at index 2.
Such an error points at the whole list expression, `a / b + c / d`, while a number computed
once inside it, such as `(n / m)` in `xs * (n / m)`, fails at its own expression.

Integer `+`, `-`, `*`, and `**` wrap around on overflow instead of failing. Float arithmetic
follows IEEE 754 apart from division by zero and `math.sqrt` of a negative number, so it produces `inf`, `-inf`, and `nan` instead of
errors.
A float `**` multiplies by repeated squaring and takes the reciprocal for a negative exponent, so
`0.0 ** -1` is `inf`.

A `ret` at the top level is not an error: it ends the program with status 0.

# Runtime errors

When a program fails while running, it prints the error to stderr the way the checker prints
one, with the file, line, and column, the message, and the source line with carets under the
expression that failed, then the calls that led there, and exits with status 1. Output printed before the error stays
printed. A compiled executable prints exactly what the interpreter does, since `stone build`
writes each place an error can happen into the executable:

```text
$ stone run average.stn
7
average.stn:5:9: error: division by zero
  |
5 |     ret total / values.len()
  |         ^^^^^^^^^^^^^^^^^^^^
average.stn:8:7: called from here
  |
8 | print(average([]))
  |       ^^^^^^^^^^^
$ stone build average.stn -o average && ./average
7
average.stn:5:9: error: division by zero
  |
5 |     ret total / values.len()
  |         ^^^^^^^^^^^^^^^^^^^^
average.stn:8:7: called from here
  |
8 | print(average([]))
  |       ^^^^^^^^^^^
```

Both backends stop at the same point with the same message, since operands are evaluated left
to right in both. The error points at the innermost expression that failed: the division in
`1 + a / b`, the subscript in `xs[i]`, the call in `xs.pop(i)`, `x.unwrap()`, `print(x)`, or
`math.sqrt(x)`, and
for `recursion is too deep`, the call that went one level too deep. A change such as
`grid[i][j] = v` or `ps[i].x = v` points at the subscript whose index is out of range. An executable names the
file by the path it was built from.

## Calls that led to an error

After the error come the calls that led to it, innermost first, each as
`file:line:col: called from here` and the call's line with carets under it. Every call of a
function written in stone counts, in any file of the program: functions, methods, associated
functions, function values, lambdas, and closures, operators and conversions a struct
implements, and the calls stone makes for you, such as `print` or a string's `{value}` calling a
struct's `display`, `==` calling its `eq`, and `<` or `sort` calling its `cmp`. Calls made inside
the prelude and the builtin modules written in stone (`net`, `gui`, `json`) are left out, and so
is the call of a test by `stone test`.

A run of calls from one place, as in recursion, is written once with a count:

```text
countdown.stn:4:13: error: division by zero
  |
4 |         ret 10 / n
  |             ^^^^^^
countdown.stn:5:9: called from here (3 times)
  |
5 |     ret countdown(n - 1)
  |         ^^^^^^^^^^^^^^^^
countdown.stn:8:7: called from here
  |
8 | print(countdown(3))
  |       ^^^^^^^^^^^^
```

At most 20 calls are written, a run counting as one, then `... and 11 more calls` says how many
calls are left out. A compiled executable writes the same calls as `stone run`, even where
`stone build` compiled a small function into its caller, and finds them by walking its own
stack, allocating nothing.

| message | cause |
| --- | --- |
| `division by zero` | `/` or `%` with a divisor of `0`, or `0.0` (as in Python, rather than IEEE 754's infinity or nan) |
| `integer overflow in division` | the smallest int divided by `-1`, which has no int result. `%` by `-1` is always `0` |
| `negative exponent` | `**` with an int base and a negative exponent, which has no int result |
| `list index out of range (index 5, length 2)` | an index at or past the length, or a negative index before the start, including `insert` at an index below `-len` or above `len`. It gives the index as written and the list's length |
| `key not found: 'pear'` | `m[k]` for a key `m` has no entry for. It gives the key, quoted if it is a string |
| `pop from empty list` | `pop(i)` on an empty list (`pop()` gives `none` instead) |
| `pop index out of range` | `pop(i)` with `i` at or past the length, or before the start |
| `bytes index out of range (index 3, length 3)` | a `bytes` index at or past the length, or a negative index before the start. It gives the index as written and the length |
| `string index out of range (index -4, length 3)` | a string index at or past the length, or a negative index before the start. It gives the index as written and the string's length in bytes |
| `cannot cut a string inside a character` | `s[i]` of a byte that is part of a character outside ASCII, or a slice that would hold only part of one (see [strings](types.md#strings)) |
| `slice step cannot be zero` | a slice such as `xs[::0]` or `s[::0]` |
| `lists have different lengths (3 and 2)` | arithmetic on two lists, such as `a + b`, whose lengths differ. It gives the first list's length and then the first length that differs from it |
| `cannot convert float to int (nan or out of range)` | `math.floor()` of nan, an infinity, or a float outside the int range |
| `empty separator` | `split("")` |
| `integer overflow in abs` | `math.abs()` of the smallest int, whose absolute value does not fit in an int |
| `math domain error` | `math.sqrt()` of a negative number, `math.sin()`, `math.cos()`, or `math.tan()` of an infinity, or `math.asin()` or `math.acos()` of a number outside -1 to 1, as in Python |
| `min of an empty list` | `math.min()` of an empty list |
| `max of an empty list` | `math.max()` of an empty list |
| `empty range for randint` | `random.randint(low, high)` with `low` greater than `high` |
| `cannot choose from an empty list` | `random.choice()` of an empty list |
| `clamp's lower bound is greater than its upper bound, or one of them is nan` | `x.clamp(lo, hi)` with `lo` greater than `hi`, as in `5.clamp(10, 0)`, or a bound that is nan |
| `could not read the current directory` | `os.cwd()` when the working directory cannot be read, such as after it was deleted |
| `sleep length must be non-negative` | `time.sleep()` of a negative number or nan |
| `sleep length is too large` | `time.sleep()` of 2^63 seconds or more |
| `recursion is too deep (more than 1000 nested calls)` | more than 1,000 calls active at once |
| `cannot unwrap none` | `unwrap()` of an option that is `none` |
| `cannot unwrap err: ...` | `unwrap()` of a result that is an `err`, followed by the error as `print` writes it, as in `cannot unwrap err: end of input`, or `cannot unwrap err: invalid literal for int() with base 10: 'x'` for `int("x").unwrap()` |
| `cannot unwrap_err ok: ...` | `unwrap_err()` of a result that is an `ok`, followed by the value |
| `value is nested too deeply to print (more than 10000 levels)` | `print` of an enum or struct value inside more than 9,999 others, as in a long linked list. `print` builds its whole line first, so nothing of the line is printed |
| `value is nested too deeply to compare (more than 10000 levels)` | `==` or `!=` of enum values nested more than 10,000 deep |
| `'name' is used before it is assigned` | a function read a global that had not been assigned yet when it ran |

Arithmetic on lists computes the whole expression for the first element, then the second, and
so on, so an error comes from the first element where any of its operations fails. In
`a / b + c / d`, a zero in `d` at index 1 stops the program before a zero in `b` at index 2.
Such an error points at the whole list expression, `a / b + c / d`, while a number computed
once inside it, such as `(n / m)` in `xs * (n / m)`, fails at its own expression.

Integer `+`, `-`, `*`, and `**` wrap around on overflow instead of failing. Float arithmetic
follows IEEE 754 apart from division by zero and the `math` domain errors above, so it produces `inf`, `-inf`, and `nan` instead of
errors.
A float `**` multiplies by repeated squaring and takes the reciprocal for a negative exponent, so
`0.0 ** -1` is `inf`.

A `ret` at the top level is not an error: it ends the program with status 0.

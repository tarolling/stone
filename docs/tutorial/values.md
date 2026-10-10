# Values and types

```{literalinclude} ../examples/values.st
:language: stone
```

prints

```{literalinclude} ../examples/values.out
:language: text
```

## The types

| type | examples | notes |
| --- | --- | --- |
| `int` | `0`, `42`, `-7` | 64-bit signed; overflow wraps, but dividing `MIN` by `-1` is an error |
| `float` | `2.5`, `1e16`, `0.1` | IEEE 754 double |
| `bool` | `true`, `false` | |
| `str` | `"hello"` | double quotes only, no escape sequences, cannot span lines |
| `none` | `none` | what a function without `ret` returns |
| `list[T]` | `[1, 2, 3]` | see {doc}`lists` |

You never write types down. The checker infers each variable's type from what is assigned to
it, and a variable keeps that type: assigning `"a"` to a variable that holds an `int` is an
error.

## Arithmetic

`+`, `-`, `*`, `/`, and `%` work on two ints or two floats, and `+` also joins two strings. stone
never converts between `int` and `float` on its own, so `price * count` with a float price and
an int count is an error. Convert explicitly with `float(count)` or `int(price)`. `int()` drops
the fraction, so `int(-2.5)` is `-2`.

Division of ints truncates toward zero, as in C, so `-7 / 2` is `-3` rather than Python's `-4`.
`%` is the remainder of that division, so it takes the sign of the left side: `-7 % 2` is `-1`
rather than Python's `1`, and `a == a / b * b + a % b` always holds. Dividing by zero with `/`
or `%`, whether by `0` or `0.0`, stops the program with `division by zero`.

`**` raises a number to a power: `2 ** 10` is `1024` and `2.5 ** 2` is `6.25`. The exponent is
always an `int`, even for a float base, so `2.0 ** -1` is `0.5` but `2.0 ** 0.5` is an error.
An int raised to a negative exponent has no int result, so `2 ** -1` stops the program with
`negative exponent`. As in Python, `**` groups right to left and binds tighter than a minus sign
before it, so `2 ** 3 ** 2` is `512` and `-2 ** 2` is `-4`.

## Printing

`print` takes any number of values, prints them separated by spaces, and ends the line. Floats
print the way Python's `repr` shows them: the shortest digits that read back as the same float,
which is why `0.1 + 0.2` prints as `0.30000000000000004`.

Next, {doc}`control-flow`.

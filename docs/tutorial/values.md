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

`+`, `-`, `*`, and `/` work on two ints or two floats, and `+` also joins two strings. stone
never converts between `int` and `float` on its own, so `price * count` with a float price and
an int count is an error. Convert explicitly with `float(count)` or `int(price)`. `int()` drops
the fraction, so `int(-2.5)` is `-2`.

Division of ints truncates toward zero, as in C, so `-7 / 2` is `-3` rather than Python's `-4`.
Dividing by zero, whether by `0` or `0.0`, stops the program with `division by zero`.

There is no `%` operator yet; compute `a % b` as `a - a / b * b`.

## Printing

`print` takes any number of values, prints them separated by spaces, and ends the line. Floats
print the way Python's `repr` shows them: the shortest digits that read back as the same float,
which is why `0.1 + 0.2` prints as `0.30000000000000004`.

Next, {doc}`control-flow`.

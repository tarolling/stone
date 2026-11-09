# Errors

stone finds most mistakes before your program runs, and stops with a clear message on the rest.

## Errors before running

`stone run`, `stone build`, and `stone check` all parse and type-check the whole file first. If
anything is wrong, they print every error with its position and run nothing. For example, this
program multiplies a float by an int:

```stone
price = 2.5
count = 3
print(price * count)
```

`stone check prices.stn` reports

```text
prices.stn:3:15: error[E0005]: expected float, found int
  |
3 | print(price * count)
  |               ^^^^^
For more about this error, try `stone explain E0005`.
```

and exits with status 1. `E0005` is the error's code: `stone explain E0005` describes that kind
of error at length, with an example of it and its fix, and [Error codes](../reference/errors.md)
lists every code. The fix is `price * float(count)`. Syntax errors look the same:
forgetting the `:` after `if x > 1` reports `expected ':', found end of line`.

Among the things the checker reports:

- operands of the wrong type, like `1 + "a"` or `-"a"`
- mixing `int` and `float` without converting one, as `float(count)` does
- assigning a value of a different type to a variable
- calling a function with the wrong number of arguments
- reading a variable that might not be assigned yet, such as one only assigned inside an `if`
- `break` or `cont` outside a loop, and `range` outside a `for` loop

## Errors while running

Some mistakes depend on values only known while running:

```{literalinclude} ../examples/runtime_error.stn
:language: stone
```

prints the first average, then stops:

```text
7
runtime_error.stn:5:9: error: division by zero
  |
5 |     ret total / values.len()
  |         ^^^^^^^^^^^^^^^^^^^^
runtime_error.stn:8:7: called from here
  |
8 | print(average([]))
  |       ^^^^^^^^^^^
```

A runtime error prints like a checker error, pointing at the expression that failed, then shows
the calls that led there, innermost first, and exits with status 1. `stone run` and a compiled executable print the same thing. See {doc}`../reference/runtime-errors` for
every one.

A program can check what it relies on with `assert`, which stops it the same way unless a
condition holds:

```stone
fn average(values: list[int]) -> int:
    assert values.len() > 0, "average of no values"
    total = 0
    for v in values:
        total += v
    ret total / values.len()
```

A failed comparison shows both sides, so `assert xs == [1, 3]` with `xs` holding `[1, 2]` stops
with `assertion failed: [1, 2] == [1, 3]`, and a message after a comma, which must be a `str`,
says what went wrong in its place. The message is only evaluated when the assertion fails.

## Errors you expect

Some failures are part of a program's job, such as a line of a file that is not a number. stone
has no exceptions: a function that may fail returns a `result[T, E]`, which is either `ok(x)`,
holding the `T` it made, or `err(e)`, holding an error `E`, often a `str` saying what went wrong.
Like `option`, it is an enum from the prelude whose variants are written alone.

```{literalinclude} ../examples/results.stn
:language: stone
```

prints

```{literalinclude} ../examples/results.out
:language: text
```

`int(s)` of a string returns a result: `ok` of the int it reads, or `err` of a message saying why
it is none, as do `float(s)` and `int(x)` of a float, which may be nan. `?` after a result passes
an error on: `int(line[i + 1:])?` gives the int an `ok` holds, and if the result is an `err`, the
function returns that `err` right there. A function can only use `?` on a result if it returns a result with the same error
type, so every error is either handled with `match` or passed on, in plain sight. `?` works on
options the same way, in a function that returns an option.

`input()` returns a result too, whose `err` says the input is over, so `line = input()?` in a
function that returns a `result[T, str]` passes the end of the input on like any other error.

A result also has methods: `is_ok()`, `is_err()`, `unwrap()`, `unwrap_or(default)`,
`unwrap_err()`, and `ok()` and `err()`, which turn it into an option. `unwrap()` of an `err`
stops the program with the error in the message, as in
`cannot unwrap err: invalid literal for int() with base 10: '4x'`.

A program that handles an error by reporting it can write the report with `eprint`, which
takes the same values as `print` but writes the line to standard error, so it stays apart from
the program's output:

```stone
match int("4x"):
    case ok(n):
        print(n)
    case err(e):
        eprint("skipping:", e)
```

With results in hand, {doc}`networking` shows a program that talks over the network, where
almost anything can fail.

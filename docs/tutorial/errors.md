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
prices.stn:3:15: error: expected float, found int
  |
3 | print(price * count)
  |               ^^^^^
```

and exits with status 1. The fix is `price * float(count)`. Syntax errors look the same:
forgetting the `:` after `if x > 1` reports `expected ':', found end of line`.

Among the things the checker reports:

- operands of the wrong type, like `1 + "a"` or `-"a"`
- mixing `int` and `float` without `int()` or `float()`
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
```

A runtime error prints like a checker error, pointing at the expression that failed, then exits
with status 1. `stone run` and a compiled executable print the same thing. See {doc}`../reference/runtime-errors` for
every one.

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

`s.parse_int()` reads an int as `int(s)` does, but returns `err` of the message `int(s)` would
stop with instead of stopping. `?` after a result passes an error on: `line[i + 1:].parse_int()?`
gives the int an `ok` holds, and if the result is an `err`, the function returns that `err`
right there. A function can only use `?` on a result if it returns a result with the same error
type, so every error is either handled with `match` or passed on, in plain sight. `?` works on
options the same way, in a function that returns an option.

`input()` returns a result too, whose `err` says the input is over, so `line = input()?` in a
function that returns a `result[T, str]` passes the end of the input on like any other error.

A result also has methods: `is_ok()`, `is_err()`, `unwrap()`, `unwrap_or(default)`,
`unwrap_err()`, and `ok()` and `err()`, which turn it into an option. `unwrap()` of an `err`
stops the program with the error in the message, as in
`cannot unwrap err: invalid literal for int() with base 10: '4x'`.

That is the whole tutorial. The {doc}`../reference/syntax` and {doc}`../reference/builtins`
pages cover the details.

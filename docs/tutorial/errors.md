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
forgetting the `;` after `if x > 1` reports `expected ';', found end of line`.

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

That is the whole tutorial. The {doc}`../reference/syntax` and {doc}`../reference/builtins`
pages cover the details.

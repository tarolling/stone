# Control flow

```{literalinclude} ../examples/control_flow.st
:language: stone
```

prints

```{literalinclude} ../examples/control_flow.out
:language: text
```

## Conditions

`if`, `elif`, and `while` take a condition that is a `bool` or an `int`, where `0` is false and
every other int is true. Strings, floats, and lists are not conditions, so write `len(s) > 0` or
`x != 0.0` instead.

Comparisons are `==`, `!=`, `<`, `<=`, `>`, and `>=`. They chain the way they do in Python:
`1 < x < 10` means `1 < x and x < 10`, and evaluates `x` once. `<` and its relatives compare
numbers; `==` and `!=` also compare strings, bools, and `none`, but not lists.

`and`, `or`, and `not` combine conditions and short-circuit, so `x != 0 and 10 / x > 1` never
divides by zero.

## Loops

`while` checks its condition before each iteration. `for` walks a list or counts with
`range`:

- `range(n)` counts `0, 1, ..., n - 1`
- `range(a, b)` counts `a, a + 1, ..., b - 1`

`range` is only allowed as the thing a `for` loop iterates over; it does not make a list. Its
bounds are evaluated once, before the first iteration, and assigning to the loop variable inside
the body does not change the next iteration.

`break` leaves the innermost loop, and `cont` skips to its next iteration.

Next, {doc}`functions`.

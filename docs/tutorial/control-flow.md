# Control flow

```{literalinclude} ../examples/control_flow.stn
:language: stone
```

prints

```{literalinclude} ../examples/control_flow.out
:language: text
```

## Conditions

`if`, `elif`, and `while` take a condition that is a `bool`: `true`, `false`, a comparison, or
`and`, `or`, and `not` of those. No other value is true or false, so write `n != 0`,
`s.len() > 0`, or `x != 0.0` instead of `n`, `s`, or `x`, and `while true:` for a loop that
ends only with `break` or `ret`.

Comparisons are `==`, `!=`, `<`, `<=`, `>`, and `>=`. They chain the way they do in Python:
`1 < x < 10` means `1 < x and x < 10`, and evaluates `x` once. `<` and its relatives compare
numbers, strings, bytes, and structs that implement `ord` (see {doc}`traits`); `==` and `!=`
also compare bools, and tuples, structs, options, and enum values part by part, but not lists or
maps.

`in` and `not in` ask whether a list holds a value, a string holds some text, or a map has a
key, as in `"b" in ["a", "b"]`, `"ell" in "hello"`, and `key not in counts`. They never chain
with other comparisons.

`and`, `or`, and `not` combine conditions and short-circuit, so `x != 0 and 10 / x > 1` never
divides by zero.

## Loops

`while` checks its condition before each iteration. `for` walks a list, the characters of a
string, the keys of a map, or the ints of `bytes`, or counts with `range`:

- `range(n)` counts `0, 1, ..., n - 1`
- `range(a, b)` counts `a, a + 1, ..., b - 1`

`range` is only allowed as the thing a `for` loop iterates over; it does not make a list. Its
bounds are evaluated once, before the first iteration, and assigning to the loop variable inside
the body does not change the next iteration.

`break` leaves the innermost loop, and `cont` skips to its next iteration.

Next, {doc}`functions`.

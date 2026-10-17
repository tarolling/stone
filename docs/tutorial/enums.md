# Enums and options

```{literalinclude} ../examples/enums.stn
:language: stone
```

prints

```{literalinclude} ../examples/enums.out
:language: text
```

## Enums

An enum is a type whose values are one of a few kinds, called its variants. `enum Shape:` opens
one, and each indented line after it names a variant and, in parentheses, the types of the
values it holds, so a `Shape` is a circle with a radius, a rectangle with a width and a height,
or empty. Enums are like Rust's: a variant can hold any values, including lists and other enum
values, or none at all.

A variant is written after its enum's name: `Shape.Circle(1.0)` makes a circle, and
`Shape.Empty` the empty shape. Like a tuple, an enum value never changes, and it prints the way
it is written.

## Match

`match` looks at a value and runs the first `case` whose pattern fits it. `case Shape.Rect(w, h):`
fits a rectangle and names its two values `w` and `h` for the block under it, and `_` in place
of a name fits any value without naming it, as in `case Shape.Rect(w, _):`. A name on its own,
as in `case other:`, fits every value.

The cases must cover every value. Leave out `case Shape.Empty:` from `area`, and the checker
stops before the program runs:

```text
error: 'match' does not cover Shape.Empty; add 'case Shape.Empty:' or 'case _:'
```

So adding a variant to an enum points at every `match` that needs a case for it.

## Options

Some answers can be missing, such as the position of a name that is not in a list. stone has no
null: a value that may be missing is an option, `option[T]`, which is either `some(x)`, holding a
`T`, or `none`. `find` above returns an `option[int]`, and `match` reads it with the patterns
`some(i)` and `none`, so the program cannot use a position that is not there.

An option also has methods: `unwrap_or(-1)` gives the value or `-1` when there is none,
`is_some()` and `is_none()` say which it is, and `unwrap()` gives the value, stopping the
program with `cannot unwrap none` when there is none. Options compare with `==` and `!=`, so
`x == some(3)` and `x != none` work too, as do values of your own enums. See
[Types](../reference/types.md#enums) for the details.

Next, {doc}`input`.

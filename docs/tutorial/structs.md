# Structs

```{literalinclude} ../examples/structs.stn
:language: stone
```

prints

```{literalinclude} ../examples/structs.out
:language: text
```

## Defining a struct

A struct groups a few values under one name, each in a field of its own. `struct Point;` opens
one, and each indented line after it names a field and its type, so a `Point` has a float `x`
and a float `y`. Structs are like Rust's: plain data with typed fields, and no classes or
inheritance.

A field can hold any type: numbers, strings, lists, tuples, options, enum values, and other
structs, so `Path` above holds a name and a list of points.

## Making and reading values

`Point(x=3.0, y=4.0)` makes a point, naming a value for every field. The names can come in any
order, so `Point(y=0.0, x=0.0)` works too, and leaving one out, naming one twice, or giving
values without names is an error before the program runs. `p.x` reads a field, and a struct
prints with its field names, as in `Point(x=3.0, y=4.0)`.

## Changing fields

A field of a struct stored in a variable can change: `back.name = "back"` replaces a field,
`back.points[1].x += 1.0` changes a field of a list's element, and `back.points.append(p)`
changes a list a field holds.

Like a list, a struct is a value, so `back = walk` makes a copy: changing `back` leaves `walk`
as it was, which is why `walk` still has two points above. A function gets a copy of the
struct it is given too, so it changes the caller's struct only by returning it, as in
`p = moved(p, 1.0)`. The copies are only conceptual: the compiler keeps a struct's fields in
registers as it would separate variables, and copies a list a field holds only when something
else still holds it.

Next, {doc}`enums`.

# Traits

```{literalinclude} ../examples/traits.stn
:language: stone
```

prints

```{literalinclude} ../examples/traits.out
:language: text
```

## Defining a trait

A trait names methods that several types can have. `trait Shape:` lists each method's
signature, `fn area(self) -> float`, without a body, and a type implements the trait with an
`impl Shape for Circle:` block that defines each of them, with the same parameters and result.
A method of the trait can also have a body, which is a default: every type that implements the
trait gets it, unless its `impl` block defines the method itself, as `Rect` could with its own
`describe`. A trait's methods all take `self`, and inside a trait `Self` is the type that
implements it, as in `fn same(self, other: Self) -> bool`.

Once a type implements a trait, its methods are called like any other: `Circle(r=1.0).area()`.
A struct's methods, inherent or from traits, all have different names, so a call never needs to
say which trait it means.

## Bounds

A generic function can say which traits its type parameters must implement, after a colon:
`fn total[T: Shape](xs: list[T])`. Inside it, a value of type `T` has the methods of `Shape`,
and nothing else, so the body is still checked once for every type `T` could be. A call checks
that its types implement the traits: `total([1.5])` is an error unless `float` implements
`Shape`. A type parameter can have several bounds, as in `[T: Shape + Named]`.

The compiler makes a copy of `total` for each type it is called with, so each call of `area`
goes straight to the method of that type, with no lookup while the program runs.

## Builtin types

`int`, `float`, `bool`, and `str` can implement traits too, in the file that defines the trait,
so `4.describe()` works once `impl Shape for int:` does.

## Comparing and ordering

Two traits come with every program. `eq` is what `==` and `!=` use. Every type `==` compares
part by part implements it already, and a struct can say what equal means for it instead, with
`impl eq for Tag:` and `fn eq(self, other: Self) -> bool`; that is also how a struct that holds a
list becomes comparable. `ord` is what `<`, `<=`, `>`, `>=`, and `sort` use: `int`, `float`, and
`str` implement it, and a struct can with `fn cmp(self, other: Self) -> ordering`, which returns
one of `less`, `equal`, or `greater`. Bounds work with them too, so `fn largest[T: ord]` can use
`>` on any ordered type, and `[T: eq]` lets a function use `==`.

Sorting a list of a struct uses its `cmp` and keeps values it finds equal in the order they were
in.

## Writing values as text

A third trait, `display`, is how `print`, `str`, and a value in a string write a value. Every type
implements it, which is why anything can be printed, and a struct can say how it is written
instead, with `impl display for Version:` and `fn display(self) -> str`. Its text then appears
wherever a `Version` does, inside a list or another struct too, as `print(vs)` shows above.

## Converting between types

Two more traits of the prelude, `from` and `try_from`, say how a value becomes another type.
Unlike the others, each has a type parameter, the type converted from, so a type can implement
it once for each source type, as `impl from[Celsius] for Fahrenheit:` does:

```{literalinclude} ../examples/conversions.stn
:language: stone
```

prints

```{literalinclude} ../examples/conversions.out
:language: text
```

A type called with one value converts it: `Fahrenheit(c)` calls the `from` above. A conversion
that can fail is a `try_from`, whose `try_from` returns a `result[Self, str]`, so
`Celsius("warm")` is an `err` that `?` passes on. A struct is still made with named values, as
`Celsius(degrees=1.0)`, so the two never look alike.

The builtin conversions work the same way. `float(2)` and `str(x)` cannot fail, while `int(2.9)`,
`int("12")`, and `float("2.5")` give results, since a float may be nan and text may not be a
number at all. A bound such as `[T: try_from[str]]` lets generic code convert to its type
parameter, as `T(text)?` does in `read_all`, where what each call's result is used as says which
type `T` is.

Next, {doc}`function-values`.

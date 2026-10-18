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

Next, {doc}`input`.

# Generics

```{literalinclude} ../examples/generics.stn
:language: stone
```

prints

```{literalinclude} ../examples/generics.out
:language: text
```

## Type parameters

A function that works the same way on values of any type is written once, with type parameters
in brackets after its name: `fn first[T](xs: list[T]) -> T:` takes a list of any type and
returns a value of that type. Inside the signature, `T` is used like any other type, alone or
inside `list[T]`, `tuple[A, B]`, `option[T]`, or a map's values.

You never write the types at a call. The checker works them out from the arguments, as it
works out every other type, so `first([3, 1, 2])` returns an `int` and `first(["a"])` a `str`.
A type parameter that appears only in the result, as in `fn empty[T]() -> list[T]:`, is
worked out from how the result is used. Arguments must agree, so with
`fn pair[T](a: T, b: T)`, `pair(1, "a")` is an error at `"a"`.

## What a generic function can do

The checker checks a generic function's body once, for every type `T` could be, so a mistake
is always reported inside the function, never at a distant call. A value of type `T` can be
assigned, passed, returned, printed, and stored in lists, tuples, options, and map values. It
cannot be used in arithmetic, compared with `==` or `<`, converted with `str`, used as a
condition, indexed, looped over, or matched, and it has no methods, since `T` might be a type
that allows none of these. A list of `T` is still a list, so `xs.len()`, `xs.append(x)`, and
`xs[0]` all work on a `list[T]`.

## Generic structs and enums

A struct or enum takes type parameters the same way, after its name: `struct Pair[A, B]:` holds
a value of each of two types, and `enum Tree[T]:` is a tree of any type. Their types are written
with the types filled in, as in `Pair[int, str]` or `Tree[str]`, and a value fills them in from
what it holds, so `Pair(first=1, second="one")` is a `Pair[int, str]` and `p.first + 1` works.

The functions of a generic struct's `impl` block use its type parameters without writing them
again: in `impl Pair:`, `fn swap(self) -> Pair[B, A]:` returns a pair the other way around.

The compiler makes a copy of a generic function, or of a generic struct's methods, for each set
of types the program uses it with, the way Rust does, so it runs as fast as one written for a
single type.

Next, {doc}`input`.

# Functions as values

```{literalinclude} ../examples/function_values.stn
:language: stone
```

prints

```{literalinclude} ../examples/function_values.out
:language: text
```

## Passing functions

A function's name without parentheses is a value, which can be passed to another function,
returned, or kept in a variable, a list, a struct, or anything else that holds values. Its type
is written `fn`, the types of its parameters in parentheses, and `->` and the type it returns,
so `bonus` above is a `fn(int) -> int`. A function that returns no value has a type without
`->`, such as `fn(str)`, and calling it is a statement of its own, as for any such function.

Any value of a function type can be called: a parameter, as `f(f(x))` calls `apply_twice`'s, an
element, as in `steps[0](1)`, or a field, as `p.run(x)` calls the function a struct's field
`run` holds. Calls through a value are checked like any other: the checker knows the value's
type, so a wrong argument is an error at the argument.

A generic function can be passed too, and gets its types from where it goes, as a call does:
`first` passed where a `fn(list[str]) -> str` is wanted takes `str` for `T`. Builtins such as
`print` and `str`, methods, and variants are not values yet; define a function that calls one
and pass that instead. Functions cannot be compared with `==`, and printing one writes its type,
as `bonus` prints `fn(int) -> int`.

## Lists' helpers

Lists have methods that take a function and call it on each element, in order:

- `xs.map(f)` is a new list of what `f` gives for each element, and `xs.filter(keep)` a new
  list of the elements `keep` gives true for.
- `xs.any(test)` is whether `test` gives true for some element, and `xs.all(test)` whether it
  does for every one. Each stops at the first element that decides the answer.
- `xs.sort_by_key(key)` sorts the list in place by what `key` gives for each element, which must
  be a type `<` orders. It calls `key` once per element before sorting, and elements with equal
  keys keep their order.
- `xs.min_by_key(key)` and `xs.max_by_key(key)` give the element with the least or greatest key
  as an option, `none` for an empty list. Of several with an equal key, `min_by_key` gives the
  first and `max_by_key` the last, the two ends of what `sort_by_key` would give.

When `stone build` compiles a call that passes a function by name, as `players.map(name)` does,
it makes a copy of `map` that calls `name` directly, so the helpers cost no more than the loop
they stand for.

## Lambdas

A function that is only needed in one place can be written there, as a lambda: its parameters
between bars, then one expression, which is what it returns.

```{literalinclude} ../examples/lambdas.stn
:language: stone
```

prints

```{literalinclude} ../examples/lambdas.out
:language: text
```

A lambda's types come from where it is used, so `|p| p.name` passed to a `list[Player]`'s `map`
takes a `Player` and gives a `str`. A parameter may still be given a type, as in
`|a: int, b: int| a + b`, which a lambda kept in a variable needs when nothing else says what it
takes. `||` starts a lambda that takes nothing, and a lambda's body may hold another lambda, as
`grid.map` does above. Its body is one expression on the line it starts on; anything longer is a
named function.

A lambda is a function value like any other, so it can be passed, kept, called, and printed, and
`stone build` calls it directly, as it does a function passed by name. A lambda may call
functions, make structs and variants, and use modules as any code does.

## Closures

A lambda may also use the variables around it: the parameters and variables of the function it
is written in, the parameters of a lambda it is inside, and globals. Such a lambda is a closure,
which holds a copy of each variable it uses, taken where the lambda is written, as `over` and
`adder` do here:

```{literalinclude} ../examples/closures.stn
:language: stone
```

prints

```{literalinclude} ../examples/closures.out
:language: text
```

Since values are copies, nothing is shared: changing `k` after `f` is made leaves `f`'s copy as
it was, and `adder(3)` and `adder(10)` each hold their own `n`. A closure cannot change its
copies either, so `|x| ys.pop()` is an error; write a function that takes `ys` and returns it
changed. A closure passed straight to a function, as `xs.filter(|x| x > limit)` passes one, costs
no more than a named function, since `stone build` passes its copies along with the other
arguments instead of making a closure.

For now, a closure cannot capture a socket or a listener, nor, inside a generic function, a value
whose type depends on a type parameter; pass those as parameters instead.

Next, {doc}`input`.

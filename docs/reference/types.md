# Types

stone is statically typed. You write the type of each function's parameters and return value,
and the checker infers every other type, for each variable and expression, before the program
runs, reporting every place they conflict.

## The types

| type | values |
| --- | --- |
| `int` | 64-bit signed integers. `+`, `-`, `*`, and `**` wrap around on overflow |
| `float` | IEEE 754 doubles, including `inf`, `-inf`, and `nan` |
| `bool` | `true` and `false` |
| `str` | immutable byte strings, ordered byte by byte |
| `bytes` | immutable sequences of any bytes, such as `b"\x00\xff"`, for data that is not text |
| `list[T]` | growable lists of `T`, which are values: assigning or passing one copies it |
| `map[K, V]` | maps from keys of type `K`, which are `int`, `str`, or `bool`, to values of type `V`, which are values like lists |
| `tuple[T1, T2, ...]` | two or more values of fixed types, such as `(1, "a")`, which never change |
| `option[T]` | `some(x)`, which holds a `T`, or `none`, which holds nothing |
| `result[T, E]` | `ok(x)`, which holds a `T`, or `err(e)`, which holds an error `E` |
| structs | the named, typed fields a `struct` lists, such as `Point(x=1.0, y=2.0)`, which are values |
| enums | the variants an `enum` lists, such as `Shape.Circle(1.0)`, which never change |
| `ordering` | `less`, `equal`, or `greater`, which `cmp` returns |
| `fn(T1, ...) -> R` | functions, such as `fn(int, list[str]) -> bool`, which are values: named, passed, stored, and called |
| `socket`, `listener` | a TCP connection, and a port waiting for connections, from the [`net` module](builtins.md#the-net-module): handles, so a copy refers to the same connection, which closes right after the last statement that uses it |

## Writing types

Types appear only in function signatures, after each parameter's name and after `->`:

```stone
fn scale(xs: list[float], k: float) -> list[float]:
    ret xs * k
```

`int`, `float`, `bool`, and `str` are written as their names, a list as `list[T]`, so a grid
of floats is `list[list[float]]`, a map as `map[K, V]`, so word counts are `map[str, int]`, a
tuple as `tuple[` and the type of each value, so a name and a
score is `tuple[str, int]`, an option as `option[T]`, a function as `fn` and its parameters'
types in parentheses, then `->` and the type it returns if it returns one, so a test of an int is
`fn(int) -> bool`, raw data as `bytes`, a connection as `socket` and a server as `listener`, and a struct or an enum by its name, such
as `Point` or `Shape`, or `shapes.Shape` for one of the module `shapes`. A function without `->` returns no value: a call
to it can only be a statement of its own, so `x = f()` and `print(f())` are errors, and returning
a value from it is an error that suggests the `->` to add. A parameter without a type is an error
too, which suggests the type the checker would infer for it.

Writing signatures keeps each function's contract in one place: a call with a wrong argument is
an error at that argument, and a mistake inside the function is reported inside it, against the
signature, rather than wherever the function happens to be called.

## Inference

Each name gets one type for its whole scope, from the first thing assigned to it and every use
after. Assigning a different type later is an error:

```text
x = 1
x = "one"   # error: cannot assign str to 'x', which is int
```

An empty list `[]` starts with an unknown element type, which is filled in by what is appended
to it, assigned into it, or what it is passed to.

A function has exactly the types its signature writes, so one that takes `xs: list[int]`
cannot also be called with a `list[str]`, unless it is generic (see below).

Arithmetic works on numbers and, element by element, on lists of them, so an operand whose type
is not known yet can be either. For example, after `xs = []`, nothing is known about `xs[0]`,
and `xs[0] * 2` makes it an `int` or a `list[int]`. The operand still has to be a number or a
list of numbers that fits the other operand, so a later `xs.append("a")` is an error at `"a"`.
If nothing ever settles it, it is the same type as the other operand.

## Generics

A generic function names its type parameters in brackets after its name, and uses them in its
signature like any other type:

```stone
fn first[T](xs: list[T]) -> T:
    ret xs[0]
```

Each call fills in the type parameters with types of its own, inferred from its arguments and
from how its result is used, so `first([1])` is an `int` and `first(["a"])` a `str`. Type
arguments are never written at a call. A type parameter cannot reuse the name of a type, such
as `int` or a struct, and cannot take types of its own (`T[int]`).

The body is checked once, with each type parameter as a type of its own that stands for any
type, so every error is reported inside the generic function or at an argument, never because
of a distant call. A value of type `T` can be assigned, passed, returned, printed, and stored
in lists, tuples, options, and map values, but not used in arithmetic, compared, converted with
`str`, used as a condition, indexed, iterated, matched, or used as a map key, and it has only the
methods of the traits that bound it (see [Traits](#traits)). Two different type parameters are
different types.

The compiler makes one copy of a generic function for each set of types the program calls it
with. Two rules keep that finite and safe:

- A generic function cannot reach a call of itself with a larger type, as `f([x])` inside
  `fn f[T](x: T):` would, since that would need a copy for `list[T]`, then `list[list[T]]`,
  and so on.
- A list element or map entry a generic function stores must fit, once its type parameters are
  filled in, in the same 32 values as any other, which the call that fills them in reports.

A global cannot take a type parameter's type, as `g` would after `g = []` if a generic function
returned `g[0]` as its `T`.

### Generic structs and enums

A struct or enum can have type parameters too, written the same way after its name, and used in
its fields or variants:

```stone
struct Pair[A, B]:
    first: A
    second: B

enum Tree[T]:
    Leaf
    Node(Tree[T], T, Tree[T])
```

Its type is written with a type for each parameter, such as `Pair[int, str]` or `Tree[float]`,
and writing `Tree` alone is an error. A value fills them in from its own values, as a call does:
`Pair(first=1, second="a")` is a `Pair[int, str]`, `Tree.Node(Tree.Leaf, 2.5, Tree.Leaf)` a
`Tree[float]`, and `Tree.Leaf` alone a `Tree` of whatever its use needs. Reading a field or
matching a variant gives the type filled in, so `p.first` is an `int`.

An `impl` block of a generic struct takes no brackets: the struct's type parameters are in scope
in all of its functions, and `self` is the struct with them, such as `Pair[A, B]`. Like a generic
function's body, each function is checked once, with the type parameters as opaque types, and
compiled once for each set of types it is used with. Methods cannot have type parameters of their
own yet.

A generic type can hold itself, in a list or an enum as any type can, but only with the same type
parameters: `enum Bad[T]:` with a variant `More(Bad[list[T]])` is an error, since a value could
then hold ever larger types. An enum value holds at most 32 values in each variant once its type
parameters are filled in, which the code making the value reports.

`==` compares two values of a generic type if the types filled in can be compared, so a
`Pair[int, str]` can be compared and a `Pair[list[int], int]` cannot.
## Traits

A trait names methods that types implement, with a signature for each and maybe a default
body:

```stone
trait Shape:
    fn area(self) -> float
    fn half(self) -> float:
        ret self.area() / 2.0
```

An `impl` block for a type implements a trait by defining each of its methods without a
default, with the parameters and result the trait gives them, and maybe its defaults too, which
replace them for that type:

```stone
impl Shape for Circle:
    fn area(self) -> float:
        ret 3.0 * self.r * self.r
```

A block that leaves out a method without a default, defines a function the trait does not have,
or changes a method's signature is an error. Every method of a trait takes `self` first, and
cannot have type parameters of its own for now. Inside a trait, `Self` is the type that
implements it, and inside any `impl` block, the block's type, as in `fn same(self, other: Self)`.
A trait is not a type: a value always has the type of the struct or builtin type it is, and a
function that takes any type implementing a trait has a type parameter with a bound.

A type implements a trait at most once, and its methods, whether its own or from traits, all
have different names, so `c.area()` always means one function. The implementation goes in the
file that defines the trait or the one that defines the struct; `int`, `float`, `bool`, and `str`
can implement a trait in the trait's file. A trait is imported like a struct, with
`use shapes.Shape`, and a library marks it `pub` to share it. Its methods need no `use`: they
are called wherever the type is.

### Bounds

A generic function names the traits each type parameter implements after a colon, joined by
`+`:

```stone
fn total[T: Shape](xs: list[T]) -> float:
    t = 0.0
    for x in xs:
        t += x.area()
    ret t
```

In the body, a value of type `T` has the methods of those traits, and still nothing else, so it
is checked once for every type `T` could be. Two bounds with a method of the same name make a
call of it an error. Each call checks its types against the bounds, so `total([1])` is an error
unless `int` implements `Shape`, and a generic function passing its own `T` on must have the
bound itself. Only a function's type parameters have bounds for now, not a struct's or an
enum's.

The compiler makes a copy of the function for each set of types, and of each default method for
each type that keeps it, so a call through a bound calls the type's method directly.

### Methods that change self

Whether a trait's method changes `self` is part of its signature, since a generic function is
checked before the types that implement the trait are known. A default changes `self` if its
body does, as `fn advance(self): self = self.next()` does, and a method without a body never
does. A type's method changes `self` exactly when the trait's does: a definition that changes
`self` where the trait's does not is an error, and one that does not still follows the rules of a
method that does, being a statement of its own, called on a variable, an element, or a field.

### eq, ord, and display

The prelude defines three traits that the operators and printing use:

```stone
pub trait eq:
    fn eq(self, other: Self) -> bool

pub trait ord:
    fn cmp(self, other: Self) -> ordering

pub trait display:
    fn display(self) -> str
```

`==` and `!=` use `eq`. Every type `==` compares part by part implements it, and a struct can
implement it itself, in which case its `eq` decides wherever its values are compared, inside
tuples, structs, options, and enums too, and a struct holding a list or map can be compared that
way. `<`, `<=`, `>`, `>=`, and `sort` use `ord`, which `int`, `float`, and `str` implement and a
struct can. `a < b` on a struct is `a.cmp(b) == less`, `a <= b` is `a.cmp(b) != greater`, and so
on, and a list of a struct sorts by `cmp`, keeping equal values in their order. `[T: eq]` lets a
generic function use `==` on a `T`, and `[T: ord]` lets it use `<` and `sort`; `ord` does not
imply `eq`. `x.eq(y)` and `x.cmp(y)` work on every type that implements the trait. A struct's
method named `eq` or `cmp` belongs in its `impl eq` or `impl ord` block, and an enum cannot have
an `impl` block yet, so it always compares part by part.

`print`, `str`, and a value in a string (`"{x}"`) use `display`. Every type implements it,
writing a value as `print` always has, and a struct can implement it itself, in which case its
`display` decides how its values are written wherever they appear, inside lists, maps, tuples,
options, enums, and other structs too, with no quotes around what it returns:

```stone
struct Money:
    cents: int

impl display for Money:
    fn display(self) -> str:
        ret "${self.cents / 100}.{self.cents % 100}"

print(Money(cents=250), [Money(cents=5)])   // $2.50 [$0.5]
```

`x.display()` is the same as `str(x)`, and printing or writing a `T` in a string needs no bound,
since every type implements `display`. A struct's method named `display` belongs in its
`impl display` block.

`ordering` is a prelude enum like `option`, whose variants are written and printed alone:

```stone
pub enum ordering:
    less
    equal
    greater
```

## Strings

A string is a sequence of bytes holding UTF-8 text, so a character outside ASCII, such as `é`,
is more than one byte. `s.len()` counts bytes, and indexes count bytes too: `s[i]` is the
one-byte string at index `i`, counting a negative index from the end, and `s[a:b:c]` slices
bytes the way a list slice picks elements. Neither may cut a character in two, which stops the
program with `cannot cut a string inside a character`: `s[i]` must be an ASCII byte, a slice
with a step of 1 must start and stop between characters, and a slice with any other step must
pick only ASCII bytes. Text that is all ASCII therefore indexes and slices exactly as in
Python:

```stone
s = "naïve"
print(s[0], s[-1], s[:2], s[2:4], s.len())   // n e na ï 6
```

A `for` loop over a string takes one character at a time, each a string of the one to four
bytes that make it up, so it never cuts one, and `for c in "naïve"` runs five times:

```stone
for c in "aé€":
    print(c, c.len())   // a 1, then é 2, then € 3
```

A string knows its length, so `s.len()` takes the same time for any string. `t in s` (or
`s.contains(t)`) tests whether the text `t` appears anywhere in `s`, byte for byte, and
`s.find(t)` gives where it first does:

```stone
print("ell" in "hello", "" in "hello", "x" not in "hello")   // true true true
```

`<`, `<=`, `>`, and `>=` order strings byte by byte, so a string orders before any longer string
it starts, and the order of characters is the order of their Unicode code points, as in Python.
Strings cannot be changed in place, so `s[0] = "x"` is an error; build a new string instead,
as in `s = "x" + s[1:]`.

### Values in strings

Any expression in braces inside a string is written into it, as `print` writes it, so a string
on its own appears without quotes and one inside a list with them:

```stone
name = "ann"
xs = [1, 2]
print("{name} has {xs.len()} items: {xs}, first {xs[0] * 10}")   // ann has 2 items: [1, 2], first 10
```

Every string works this way, with no prefix. The values are evaluated left to right before any
is written, like a call's arguments, and a value may hold strings of its own, even with values in
them (`"{"{x}!"}"`), as well as indexes, slices, and maps. `{{` and `}}` write a brace, and a
lone `}`, an empty `{}`, or a value that runs past the end of the line is an error. A `:` after
a value is kept for format specs such as `{x:.2f}`, which are not supported yet.

## Bytes

`bytes` hold any bytes, 0 and those outside ASCII included, for data that is not text, such as
a file's contents or what a network protocol sends. A bytes literal is a string literal after a
`b`, with ASCII characters and escapes, where `\x00` through `\xff` write any byte and braces are
plain bytes. Bytes index to ints and slice to bytes, with no characters to cut:

```stone
data = b"GET\x00\xff"
print(data[0], data[-1], data[:3], data.len())   // 71 255 b'GET' 5
for b in data[3:]:
    print(b)   // 0, then 255
```

`+` joins bytes, `==` and `<` compare them byte by byte, as strings compare, and `part in data`
tests whether `part` appears in them. `print` writes bytes as Python writes them, with every byte
outside printable ASCII escaped, as `b'GET\x00\xff'`. Like strings, bytes never change, and
they cannot be map keys. `"text".bytes()` gives a string's UTF-8 bytes, `data.text()` gives the
string bytes hold, if they are UTF-8 text, as a result, and `bytes([104, 105])` makes bytes from
their values; see [Builtins](builtins.md#bytes). `fs.read_bytes` and a socket's `read_bytes` and
`read_exact` read them, and `fs.write_bytes` and `send_bytes` write them.

## Tuples

A tuple holds two or more values, each of its own type, written in parentheses. It is how a
function returns several values:

```stone
fn divide(a: int, b: int) -> tuple[int, int]:
    ret (a / b, a % b)

q, r = divide(17, 5)
print(q, r, divide(9, 2))   // 3 2 (4, 1)
```

Names separated by commas, on the left of `=` or after `for`, unpack a tuple, one name for each
value and at any depth, as in `a, (b, c) = t`. The whole value is computed before any name is
assigned, so `a, b = (b, a)` swaps. A list of tuples unpacks one element at a time:

```stone
scores = [("ann", 3), ("bo", 5)]
for name, score in scores:
    print(name, score)
```

`t[0]` reads a tuple's first value and `t[-1]` its last. The index must be a number written in
the code, so the checker knows the value's type, and one past the tuple's size is an error
before the program runs. A tuple's values are only read: `t[0] = v` and `t[0].append(v)` are
errors, and a new tuple replaces the old one, as in `t = (v, t[1])`.

Values separated by commas need their parentheses everywhere else, so `x = 1, 2` and
`ret a, b` are errors that suggest `(1, 2)` and `ret (a, b)`. A tuple has at least two values:
`(x)` is `x` in parentheses, and `()` and `(x,)` are errors.

A tuple prints as Python prints one, `(1, 'a')`, with strings inside it quoted. `==` and `!=`
compare two tuples value by value, so `(1, "a") == (1, "a")` is `true`, as long as no value is a
list or a map. A tuple cannot be ordered with `<`, iterated, converted with `str`, or used in
arithmetic or as a condition.

A tuple is not stored anywhere of its own: the compiler keeps each of its values where it would
keep a variable, passes and returns them as separate values, and stores the values of a list's
tuples side by side in the list, so a tuple costs nothing over writing its values out by hand.
An element of a list can hold at most 32 values, counting each value of its tuples.

## Structs

A struct is a type whose values hold a value for each field it lists, each of the type written
after its name, like a struct in Rust:

```{literalinclude} ../examples/structs.stn
:language: stone
:lines: 3-9
```

`Point(x=1.0, y=2.0)` makes a value of it. Every field gets a value, named as in the definition,
once each and in any order, and the values run in the order they are written. Leaving a field
out, naming one twice or one the struct does not have, and giving values without names are all
errors before the program runs. The name alone, without a call, is not a value.

`p.x` reads a field, and a field of a struct in a variable can change: `p.x = 3.0`,
`p.x += 1.0`, `ps[i].y = 0.0`, `path.points.append(q)`, and `path.points[0].x = 2.0` are
changes, made as `xs[i] = v` is. Like lists, structs are values: `q = p` and passing `p` to a
function copy it, so changing `q` leaves `p` as it was, and a function changes its caller's
struct only by returning it, as in `p = moved(p, 1.0)`, unless it is a method that changes
`self` (see [Methods](#methods)). As with lists, the change must be to a
variable: `f().x = 1.0` is an error, since the change would be lost, and a function cannot change
a global's fields.

A field can hold any type, including other structs. A struct can hold itself only through a list,
an option, or an enum, as in `kids: list[Tree]` or `next: option[Node]`, since a value that held
itself directly would never end; `next: Node` is an error that suggests `option[Node]`.

The prelude defines one struct, `output`, which [`os.run`](builtins.md#the-os-module) returns:
its fields are `status: int`, `stdout: str`, and `stderr: str`. As with the prelude's `result`, a
variable may still be named `output`, though a function may not.

A struct prints with its name and fields, as in `Point(x=1.0, y=2.0)`, with the strings it holds
quoted. `==` and `!=` compare two structs field by field, as long as no field holds a list or a
map. A struct cannot be ordered with `<`, converted with `str`, iterated, or used in arithmetic or
as a condition.

Structs are defined at the top level, and can be used before their definition. A library module
makes one usable from other files with `pub struct`, and `use shapes.Point` imports it, or
`shapes.Point(x=1.0, y=2.0)` names it through its module. See [Modules](modules.md).

A struct is not stored anywhere of its own: as with a tuple, the compiler keeps each of its
fields where it would keep a variable, passes and returns them as separate values, and stores the
fields of a list's structs side by side in the list, so changing a field of a struct in a
variable is as cheap as assigning a variable. An element of a list or a variant can hold at most
32 values, counting each field of its structs. A value can print at most 10,000 struct and enum
values deep.

### Methods

An `impl` block names a struct and holds functions for it, each written as any other function:

```{literalinclude} ../examples/methods.stn
:language: stone
:lines: 7-16
```

A function whose first parameter is `self`, written without a type, is a method of the struct,
and `self` is a value of it. `a.dist(b)` calls the method `dist` with `a` as `self`, evaluating
`a` and then the arguments. Any other function of the block is an associated function, called on
the struct by name, as `Point.origin()` is. Calling a method on the struct or an associated
function on a value is an error, as is a parameter named `self` anywhere but first in a method.
A struct may have several `impl` blocks, but each function's name once, and a method cannot share
a field's name. Methods belong to their struct, so a method may have the name of a builtin one,
such as `len`, and `p.len()` calls the struct's.

A method that changes `self`, by assigning `self`, one of its fields, or anything inside them, or
by calling something that changes one of those, such as `self.points.append(p)` or another such
method, changes the value it is called on, as `append` changes a list. stone finds these methods
itself, with no keyword. A call of one is a statement of its own or, if the method returns a
value, may be inside any expression, as in `print(s.pop_top())`, as long as nothing else in that
statement uses what it changes (`s`, or `self.items` for `self.items.pop()`, while `self.offset`
is another field and may be used), and the value it is called on
is a variable, an element, or a field, which a function may change only if it is the function's
own: `p.shift(1.0)`, `ps[i].shift(1.0)`, and `path.start.shift(1.0)` all change what they name,
while `Point.origin().shift(1.0)` is an error, since the change would be lost. The receiver's
indexes are evaluated first, then the arguments, then the method runs, so an index out of range
fails after the arguments, as for `append`.

A method that changes `self` moves the value into the call and back, so it changes the value in
place; a method that only reads `self` borrows it. Neither copies the struct or the lists it
holds unless another variable still shares them.

## Maps

A map holds a value for each of its keys, like a `dict` in Python or a `HashMap` in Rust:

```stone
ages = {"ann": 31, "bob": 27}
ages["cy"] = 45      // adds an entry
ages["ann"] = 32     // changes one, which keeps its place
print(ages["bob"], ages.len(), "dee" in ages)   // 27 3 false
print(ages)          // {'ann': 32, 'bob': 27, 'cy': 45}
```

Keys are `int`, `str`, or `bool`, all of one type, and values can be any type, all of one type
too. `{}` is an empty map, whose types come from how it is used. A map keeps its entries in the
order their keys were first added, which is the order it prints in, and a literal that names a
key twice keeps the later value, as in Python. `m[k]` stops the program with
`key not found: 'dee'` if `m` has no entry for `k`, so where it might not, check with
`k in m` (or `m.contains(k)`) first or read it with `m.get(k)`, which gives `some` of the value
or `none`.

A `for` loop over a map goes through its keys in that order, and `m.keys()`, `m.values()`, and
`m.items()` give them as new lists, the last as `(key, value)` tuples. `m.pop(k)` removes `k`'s
entry and gives `some` of its value, or `none` if there is none:

```stone
counts = {"a": 1, "b": 2}
for k in counts:
    counts[k] += 10              // the loop goes through the keys as they were
for k, n in counts.items():
    print(k, n)                  // a 11, then b 12
gone = counts.pop("a")
print(gone, counts.keys())       // some(11) ['b']
```

Like lists, maps are values: `b = a` and passing `a` to a function copy it, so a function
changes its caller's map only by returning it, as in `counts = add(counts, word)`, which changes
it in place. An entry changes like a list's element, through any place that starts at a
variable: `m[k] += 1`, `m[k].append(x)`, `m[k].x = 1.0`, `grid[r][c] = v` for a map of maps,
`p.counts[k] += 1` for a struct's field, and `p.counts.pop(k)`. A plain assignment `m[k] = v`
adds the entry if it is missing, but a key on the way to what changes, or the target of `+=`,
must have one, or the program stops with `key not found`. A map cannot be compared with `==`,
converted with `str`, or used in arithmetic or as a condition.

An entry is stored as a key and its value's values side by side, as a list stores tuples, in
insertion order, with a hash table to find each key, so reading and setting an entry takes
about the same time however large the map is. Removing an entry moves the ones after it down,
so `pop` takes time in proportion to the map's size. An entry can hold at most 32 values,
counting its key and each field of its value's tuples and structs.

## Enums

An enum is a type whose values are one of the variants it lists, each holding the values its
parentheses give the types of, or none, like an enum in Rust:

```stone
enum Shape:
    Circle(float)
    Rect(float, float)
    Empty
```

A variant is written with its enum's name, so `Shape.Circle(1.0)` and `Shape.Rect(2.0, 3.0)` are
values of `Shape`, and so is `Shape.Empty`, without parentheses. `match` tells the variants apart
and names the values they hold:

```{literalinclude} ../examples/enums.stn
:language: stone
:lines: 1-16
```

`match` runs the first case whose pattern fits, and the cases must cover every value: leaving
out `Shape.Empty` above is an error that names it, `'match' does not cover Shape.Empty`. A case
that can never run, because the ones before it cover every value it would, is a warning. A
pattern is one of:

- a variant with a pattern for each of its values, as in `Shape.Rect(w, h)`, which fits a value
  of that variant whose values fit those patterns, at any depth, as in `some(Shape.Circle(r))`
- a name, which fits any value and assigns it to that variable, like `=`
- `_`, which fits any value and assigns nothing

So `case Empty:`, without `Shape.`, would assign any value to a variable named `Empty`, which is
an error that suggests `Shape.Empty`. The names a case assigns are ordinary variables, so a name
every case assigns can be read after the `match`.

A variant can hold any type, including lists, tuples, and the enum itself, so enums make trees
and linked lists:

```stone
enum Tree:
    Leaf(int)
    Node(Tree, Tree)
```

Enum values never change: a new one replaces the old, as with tuples, and the values a `match`
names are copies, so changing one, as in `xs.append(1)` for a list it named, changes only that
variable. Values print as they are written, as in `Shape.Rect(2.0, 3.0)`, with the strings they
hold quoted. `==` and `!=` compare two values of an enum, or two options, by variant and then by
the values they hold, so `some(1) == some(1)` and `x != none` work, as long as no variant holds a
list or a map. Comparing values nested more than 10,000 deep stops the program, as printing them
does. They cannot be ordered with `<`, converted with `str`, or used in arithmetic or as
conditions; `match` tells them apart and names their values.

Enums are defined at the top level, and can be used before their definition. A library module
makes one usable from other files with `pub enum`, and `use shapes.Shape` imports it, or
`shapes.Shape.Circle(1.0)` names it through its module. See [Modules](modules.md).

A value of an enum is stored once and shared by every variable that holds it, with a count of
the variables that do, like a list, so passing one around never copies it. A variant without
values is stored once for the whole program. Freeing a long chain of values, such as a linked
list of a million nodes, takes no more stack than freeing one. An enum value can print at most
10,000 enum values deep: `print` stops the program at a deeper one, after printing what comes
before it, with `value is nested too deeply to print (more than 10000 levels)`.

## Options

`option[T]` is the type of a value that may be missing: `some(x)` holds a `T`, and `none` holds
nothing. It is an ordinary [generic enum](#generic-structs-and-enums), defined in stone by the
prelude, a file every program is linked with:

```stone
pub enum option[T]:
    none
    some(T)
```

Its variants are written alone, as `some(x)` and `none` rather than `option.some(x)`, in every
file and without a `use`, and no function, variable, or type may take the name `some`, nor
`ordering`'s `less`, `equal`, and `greater`, nor `result`'s `ok` and `err`. No enum or struct may
be named `option`, `ordering`, or `result` either, but since their variants never follow their
names, a variable, parameter, or function may be, as in `result = []`. A function that may have no
answer returns an option:

```{literalinclude} ../examples/enums.stn
:language: stone
:lines: 18-
```

`match` reads an option with the patterns `some(x)` and `none`, and its methods read it too:
`x.is_some()` and `x.is_none()` say which it is, `x.unwrap()` returns the value it holds and stops
the program with `cannot unwrap none` if there is none, and `x.unwrap_or(default)` returns
`default` instead. An option prints as `some(1)` or `none`. Since it is an enum, it follows the
rules of every enum: two options compare with `==` when what they hold can, and `some` of a value
must fit an enum value's 32 values.

`none` on its own has an unknown type until its use settles it, as an empty list does, so after
`best = none`, `best = some("a")` makes `best` an `option[str]`.

## Results

`result[T, E]` is the type of something that may fail: `ok(x)` holds the `T` it made, and `err(e)`
an error `E`, such as a `str` saying what went wrong. Like `option`, it is a prelude enum whose
variants are written alone:

```stone
pub enum result[T, E]:
    ok(T)
    err(E)
```

`match` reads a result with the patterns `ok(x)` and `err(e)`, and its methods read it too:
`r.is_ok()` and `r.is_err()` say which it is, `r.unwrap()` returns the value an `ok` holds and
stops the program with `cannot unwrap err: ` and the error on an `err`, `r.unwrap_or(default)`
returns `default` instead, `r.unwrap_err()` returns the error and stops with
`cannot unwrap_err ok: ` and the value on an `ok`, and
`r.ok()` and `r.err()` give `some` of the value or the error, or `none`. `s.parse_int()` and
`s.parse_float()` return results, whose error is the message `int(s)` or `float(s)` would stop
the program with. A result prints as `ok(1)` or `err('bad')`, and compares with `==` when what it
holds can.

### Passing errors on

`?` after a result gives the value an `ok` holds, and on an `err` returns that `err` from the
function it is in, right there:

```stone
fn sum(texts: list[str]) -> result[int, str]:
    total = 0
    for t in texts:
        total += t.parse_int()?
    ret ok(total)

print(sum(["1", "2"]), sum(["1", "x"]))   // ok(3) err("invalid literal for int() with base 10: 'x'")
```

It follows a value as a call or an index does, so `f()?.name`, `m.get(k)?[0]`, and `-f()?` mean
what they look like, and it may appear anywhere in an expression; whatever the statement made
before it is freed when it returns. The checker requires:

- a function: `?` at the top level, or in a function that returns no value, is an error
- on a result, a function that returns a result with the same error type, as in
  `result[float, str]` for a `?` on a `result[int, str]`; nothing converts one error into another
- on an option, a function that returns an option, where `?` returns `none`

A result becomes an option with `.ok()`, so `t.parse_int().ok()?` works in a function that
returns an option.

## Functions

A function's name without parentheses is a value of its function type, `fn` and its
parameters' types, then `->` and the type it returns, if any:

```stone
fn double(x: int) -> int:
    ret x * 2

fn apply(f: fn(int) -> int, x: int) -> int:
    ret f(x)

print(apply(double, 4), [1, 2].map(double))   // 8 [2, 4]
```

Any expression of a function type can be called, such as a parameter, `fs[0](x)`, or a struct
field that holds one, as `p.run(x)`. A generic function used as a value takes its types from
where it is used. Builtins, methods, and variants are not values yet. A function value is never
compared or used as a map key, and `print` writes it as its type, as `fn(int) -> int`. For
now, a function named as a value inside a generic function cannot have a type that depends on
the type parameters, such as `id` at `T`; take it as a parameter instead. Closures, functions
written inside an expression that capture values around them, are not supported yet.

A function never changes, so passing one copies nothing. The methods `map`, `filter`, `any`,
`all`, `sort_by_key`, `min_by_key`, and `max_by_key` of lists take a function; see
[Builtins](builtins.md).

## Rules the checker enforces

These keep the two backends identical and catch common mistakes:

- **No implicit conversion.** Arithmetic and comparisons never mix `int` and `float`; use
  `int()` or `float()`. The one exception is `**`, whose exponent is always an `int`. The same
  holds element by element, so `list[int] + list[float]` and `[1, 2] * 0.5` are errors. Strings
  never mix with numbers either: `int("3")` and `float("2.5")` read numbers from strings, and
  `str(3)` writes them.
- **Conditions are `bool`.** `if`, `elif`, `while`, `and`, `or`, and `not` reject ints,
  floats, strings, lists, and `none`, so `if 1 or 0:` is an error; write `n != 0` instead of `n`.
- **Lists and maps are not comparable.** `==` and `!=` compare numbers, bools, strings, and
  tuples, structs, options, and enum values of them, part by part, but not lists or maps, even
  inside another value, unless a struct holding one implements `eq`. Functions, sockets, and
  listeners are never compared. `<`, `<=`, `>`, and `>=`
  compare only numbers, strings, and structs that implement `ord`.
- **A call without a value is a statement.** A function without `->`, and builtins such as
  `print`, return no value, so their calls can only be statements of their own.
- **Matches cover every value.** The cases of a `match` must fit every value of its subject,
  which must be an enum or an option.
- **Tuples are indexed by constants and never change.** `t[i]` needs `i` written as a number,
  and nothing changes a tuple's values or what they hold, so `t[0] = v` is an error.
- **Arithmetic on lists needs numbers.** `+ - * / % **` apply to a list only if it holds ints
  or floats, so `["a"] + ["b"]` and `[[1]] * [[2]]` are errors.
- **Changes are statements, made to variables.** Since lists and structs are values,
  `xs.append(v)`, `xs[i] = v`, and `p.x = v` change only the variable they start from. Each is a
  statement of its own, so `ys = xs.append(1)` is an error, and the list or struct must be in a
  variable or an element or field of one, so `f().append(1)` and `xs[1:].append(1)` are errors
  too, since the change would be lost. `pop`,
  which also returns what it removes, may instead be inside any expression, as in
  `print(xs.pop().unwrap())`, as long as nothing else in that statement uses the list it
  changes, so `print(xs, xs.pop())` is an error. A method that changes `self` follows the same
  rules, as in `s.push(1)` and `print(s.pop_top())`.
- **Functions change only their own variables.** A function can read a global but cannot
  change it, with `append`, an index, or a field, just as it cannot assign it. It takes the list as an
  argument and returns the changed one instead. A function that changes a parameter and then
  never uses it gets a warning, since its caller cannot see the change.
- **`range` is only a `for` iterable.** It cannot be stored or passed around.
- **Functions are top level, and only called.** A function cannot be assigned, stored, or
  passed as a value, and a builtin's name cannot be assigned to.
- **Signatures are written.** Every parameter has a type, and a function that returns a value
  writes its type after `->`.
- **Calls match.** Each call passes as many arguments as the function has parameters, each of
  the parameter's type. Only a struct takes named values, as in `Point(x=1.0, y=2.0)`, and a
  struct takes nothing else.
- **Returns match the signature.** Every `ret` in a function returns the type after its `->`,
  and a function with a return type returns a value on every path.
- **Variables are assigned before they are read.** At the top level and inside each function,
  every path to a read must assign the name first. A function reading a global is checked when
  it runs instead, since it may be called before or after the assignment.

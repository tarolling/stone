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
| `list[T]` | growable lists of `T`, which are values: assigning or passing one copies it |
| `map[K, V]` | maps from keys of type `K`, which are `int`, `str`, or `bool`, to values of type `V`, which are values like lists |
| `tuple[T1, T2, ...]` | two or more values of fixed types, such as `(1, "a")`, which never change |
| `option[T]` | `some(x)`, which holds a `T`, or `none`, which holds nothing |
| structs | the named, typed fields a `struct` lists, such as `Point(x=1.0, y=2.0)`, which are values |
| enums | the variants an `enum` lists, such as `Shape.Circle(1.0)`, which never change |
| functions | each function has one signature, such as `fn(int, list[str]) -> bool` |

## Writing types

Types appear only in function signatures, after each parameter's name and after `->`:

```stone
fn scale(xs: list[float], k: float) -> list[float];
    ret xs * k
```

`int`, `float`, `bool`, and `str` are written as their names, a list as `list[T]`, so a grid
of floats is `list[list[float]]`, a map as `map[K, V]`, so word counts are `map[str, int]`, a
tuple as `tuple[` and the type of each value, so a name and a
score is `tuple[str, int]`, an option as `option[T]`, and a struct or an enum by its name, such
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

Functions are monomorphic: a function has exactly the types its signature writes, so one that
takes `xs: list[int]` cannot also be called with a `list[str]`.

Arithmetic works on numbers and, element by element, on lists of them, so an operand whose type
is not known yet can be either. For example, after `xs = []`, nothing is known about `xs[0]`,
and `xs[0] * 2` makes it an `int` or a `list[int]`. The operand still has to be a number or a
list of numbers that fits the other operand, so a later `xs.append("a")` is an error at `"a"`.
If nothing ever settles it, it is the same type as the other operand.

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
for c in "aé€";
    print(c, c.len())   // a 1, then é 2, then € 3
```

A string knows its length, so `s.len()` takes the same time for any string.

`<`, `<=`, `>`, and `>=` order strings byte by byte, so a string orders before any longer string
it starts, and the order of characters is the order of their Unicode code points, as in Python.
Strings cannot be changed in place, so `s[0] = "x"` is an error; build a new string instead,
as in `s = "x" + s[1:]`.

## Tuples

A tuple holds two or more values, each of its own type, written in parentheses. It is how a
function returns several values:

```stone
fn divide(a: int, b: int) -> tuple[int, int];
    ret (a / b, a % b)

q, r = divide(17, 5)
print(q, r, divide(9, 2))   // 3 2 (4, 1)
```

Names separated by commas, on the left of `=` or after `for`, unpack a tuple, one name for each
value and at any depth, as in `a, (b, c) = t`. The whole value is computed before any name is
assigned, so `a, b = (b, a)` swaps. A list of tuples unpacks one element at a time:

```stone
scores = [("ann", 3), ("bo", 5)]
for name, score in scores;
    print(name, score)
```

`t[0]` reads a tuple's first value and `t[-1]` its last. The index must be a number written in
the code, so the checker knows the value's type, and one past the tuple's size is an error
before the program runs. A tuple's values are only read: `t[0] = v` and `t[0].append(v)` are
errors, and a new tuple replaces the old one, as in `t = (v, t[1])`.

Values separated by commas need their parentheses everywhere else, so `x = 1, 2` and
`ret a, b` are errors that suggest `(1, 2)` and `ret (a, b)`. A tuple has at least two values:
`(x)` is `x` in parentheses, and `()` and `(x,)` are errors.

A tuple prints as Python prints one, `(1, 'a')`, with strings inside it quoted. It cannot be
compared, iterated, converted with `str`, or used in arithmetic or as a condition.

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

A struct prints with its name and fields, as in `Point(x=1.0, y=2.0)`, with the strings it holds
quoted. It cannot be compared with `==`, converted with `str`, iterated, or used in arithmetic or
as a condition; compare its fields instead.

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
value, the whole value of an assignment, as in `x = s.pop_top()`, and the value it is called on
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
print(ages["bob"], ages.len(), ages.contains("dee"))   // 27 3 false
print(ages)          // {'ann': 32, 'bob': 27, 'cy': 45}
```

Keys are `int`, `str`, or `bool`, all of one type, and values can be any type, all of one type
too. `{}` is an empty map, whose types come from how it is used. A map keeps its entries in the
order their keys were first added, which is the order it prints in, and a literal that names a
key twice keeps the later value, as in Python. `m[k]` stops the program with
`key not found: 'dee'` if `m` has no entry for `k`, so check with `m.contains(k)` first where it
might not.

Like lists, maps are values: `b = a` and passing `a` to a function copy it, so a function
changes its caller's map only by returning it, as in `counts = add(counts, word)`, which changes
it in place. A change must be to a map in a variable, and for now it can only be an assignment,
`m[k] = v`: `m[k] += 1` and `m[k].append(x)` are errors, so write `m[k] = m[k] + 1`. A map
cannot be compared with `==`, converted with `str`, iterated, or used in arithmetic or as a
condition.

An entry is stored as a key and its value's values side by side, as a list stores tuples, in
insertion order, with a hash table to find each key, so reading and setting an entry takes
about the same time however large the map is. An entry can hold at most 32 values, counting its
key and each field of its value's tuples and structs.

## Enums

An enum is a type whose values are one of the variants it lists, each holding the values its
parentheses give the types of, or none, like an enum in Rust:

```stone
enum Shape;
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

So `case Empty;`, without `Shape.`, would assign any value to a variable named `Empty`, which is
an error that suggests `Shape.Empty`. The names a case assigns are ordinary variables, so a name
every case assigns can be read after the `match`.

A variant can hold any type, including lists, tuples, and the enum itself, so enums make trees
and linked lists:

```stone
enum Tree;
    Leaf(int)
    Node(Tree, Tree)
```

Enum values never change: a new one replaces the old, as with tuples, and the values a `match`
names are copies, so changing one, as in `xs.append(1)` for a list it named, changes only that
variable. Values print as they are written, as in `Shape.Rect(2.0, 3.0)`, with the strings they
hold quoted. They cannot be compared with `==`, converted with `str`, or used in arithmetic or as
conditions; `match` tells them apart.

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

`option[T]` is the enum built into the language for a value that may be missing: `some(x)` holds
a `T`, and `none` holds nothing. A function that may have no answer returns one:

```{literalinclude} ../examples/enums.stn
:language: stone
:lines: 18-
```

`match` reads an option with the patterns `some(x)` and `none`, and its methods read it too:
`x.is_some()` and `x.is_none()` say which it is, `x.unwrap()` returns the value it holds and stops
the program with `cannot unwrap none` if there is none, and `x.unwrap_or(default)` returns
`default` instead. An option prints as `some(1)` or `none`.

`none` on its own has an unknown type until its use settles it, as an empty list does, so after
`best = none`, `best = some("a")` makes `best` an `option[str]`.

## Rules the checker enforces

These keep the two backends identical and catch common mistakes:

- **No implicit conversion.** Arithmetic and comparisons never mix `int` and `float`; use
  `int()` or `float()`. The one exception is `**`, whose exponent is always an `int`. The same
  holds element by element, so `list[int] + list[float]` and `[1, 2] * 0.5` are errors. Strings
  never mix with numbers either: `int("3")` and `float("2.5")` read numbers from strings, and
  `str(3)` writes them.
- **Conditions are `bool`.** `if`, `elif`, `while`, `and`, `or`, and `not` reject ints,
  floats, strings, lists, and `none`, so `if 1 or 0;` is an error; write `n != 0` instead of `n`.
- **Lists, tuples, structs, and enums are not comparable.** `==` and `!=` cannot compare lists,
  tuples, structs, options, or enum values, and `<`, `<=`, `>`, and `>=` compare only numbers
  and strings. Tell enum values apart with `match`, and options with `is_some()` and
  `is_none()`, and compare structs by their fields.
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
  which also returns what it removes, may instead be the whole value of an assignment, as in
  `x = xs.pop()`. A method that changes `self` follows the same rules, as in `s.push(1)` and
  `x = s.pop_top()`.
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

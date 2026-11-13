# Error codes

Every error the checker, the linker, or the parser reports has a code, which an error shows after its severity, as in `main.stn:2:5: error[E0005]: cannot assign str to 'x', which is int`. `stone explain E0005` prints what a code means, with an example of the error and its fix, and editors link each error to its code below. Warnings and [runtime errors](runtime-errors.md) have no code.

(e0001)=
## E0001: the program does not parse

The program is not written in stone's grammar, such as a block that does not open with `:`,
a bracket that is never closed, or values separated by commas without parentheses. The error
points at the first token the parser could not use and says what it expected there.

This program fails with E0001:

```stone
x = 2
if x > 1
    print(x)
```

This one is fixed:

```stone
x = 2
if x > 1:
    print(x)
```

(e0002)=
## E0002: a character or literal that is not valid

The source holds text that is no token of stone: an int literal too large for an int, a
string that never ends, an escape such as `\q` that stone does not have, a character such as
`;` that stone never uses, or a bytes literal with a character outside ASCII.

This program fails with E0002:

```stone
x = 99999999999999999999
print(x)
```

This one is fixed:

```stone
x = 999999999999999999
print(x)
```

(e0003)=
## E0003: a name that is not defined, or not that kind of name

A name is used that nothing defines: no variable, function, type, or trait of that name is
visible there. A misspelled name gets help naming the defined one it is closest to. The same
code covers a name of the wrong kind, such as a struct written where a trait is expected.

This program fails with E0003:

```stone
total = 3
print(totl)
```

This one is fixed:

```stone
total = 3
print(total)
```

(e0004)=
## E0004: no such method, field, variant, or function

A value, type, or module has no member of that name: a struct with no such field or method,
an enum with no such variant, or a module with no such function. Like an undefined name, a
misspelled member gets help naming the closest one.

This program fails with E0004:

```stone
struct Point:
    x: float
    y: float

p = Point(x=1.0, y=2.0)
print(p.z)
```

This one is fixed:

```stone
struct Point:
    x: float
    y: float

p = Point(x=1.0, y=2.0)
print(p.x + p.y)
```

(e0005)=
## E0005: mismatched types

A value has a type other than the one its place needs: an argument that does not fit its
parameter, a variable assigned a value of another type than before, or list elements of
different types. The error labels the place that says which type was expected, such as the
parameter's written type. An int and a float never mix: convert one with `float(n)` or
`.floor()`.

This program fails with E0005:

```stone
fn double(n: int) -> int:
    ret n * 2

print(double("4"))
```

This one is fixed:

```stone
fn double(n: int) -> int:
    ret n * 2

print(double(4))
```

(e0006)=
## E0006: the wrong number of arguments or types

A call gives a function more or fewer arguments than it has parameters, or a generic type is
written with more or fewer types than it takes, as in `list[int, str]`. The error labels the
function's definition.

This program fails with E0006:

```stone
fn double(n: int) -> int:
    ret n * 2

print(double(4, 2))
```

This one is fixed:

```stone
fn double(n: int) -> int:
    ret n * 2

print(double(4))
```

(e0007)=
## E0007: a parameter without a type

Every parameter of a function has a written type, so that a function's signature is a
contract that its calls are checked against. The help suggests the type the function's calls
give the parameter.

This program fails with E0007:

```stone
fn double(n) -> int:
    ret n * 2

print(double(4))
```

This one is fixed:

```stone
fn double(n: int) -> int:
    ret n * 2

print(double(4))
```

(e0008)=
## E0008: a value that does not fit the return type

What a function returns does not fit its signature: a function without `-> T` returns a
value, a `ret` value is not of the written return type, a `ret` without a value is in a
function that returns one, or some path through the function ends without returning. A
function that returns a value writes `-> T` after its parameters.

This program fails with E0008:

```stone
fn half(n: int):
    ret n / 2

print(half(4))
```

This one is fixed:

```stone
fn half(n: int) -> int:
    ret n / 2

print(half(4))
```

(e0009)=
## E0009: a call that returns no value used as a value

A function without `-> T` returns no value, so its call can only be a statement of its own.
The same holds for a method that changes its receiver and returns nothing, such as `append`.

This program fails with E0009:

```stone
fn greet(name: str):
    print("hello, {name}")

x = greet("ada")
print(x)
```

This one is fixed:

```stone
fn greet(name: str):
    print("hello, {name}")

greet("ada")
```

(e0010)=
## E0010: a variable that might be used before it is assigned

A variable is read where some path to the read skips every assignment to it, such as an
assignment inside an `if` without an `else`, or inside a loop that might not run. Assign it
on every path, for example before the `if`.

This program fails with E0010:

```stone
n = int(input().unwrap_or("0")).unwrap_or(0)
if n > 0:
    sign = "positive"
print(sign)
```

This one is fixed:

```stone
n = int(input().unwrap_or("0")).unwrap_or(0)
sign = "not positive"
if n > 0:
    sign = "positive"
print(sign)
```

(e0011)=
## E0011: a value of the wrong kind for an operation

An operator, builtin, or method is given a kind of value it does not work on, such as `-` on
strings, `len` on an int, or `math.sqrt` of a string. The message says which kinds it takes.

This program fails with E0011:

```stone
print("stone" - "s")
```

This one is fixed:

```stone
print("stone".replace("s", ""))
```

(e0012)=
## E0012: a condition that is not a bool

The condition of an `if` or `while`, and the operands of `and`, `or`, and `not`, must be
bools. stone has no truthiness: compare instead, as in `if count > 0:` or `if xs.len() > 0:`.

This program fails with E0012:

```stone
count = 3
if count:
    print("some")
```

This one is fixed:

```stone
count = 3
if count > 0:
    print("some")
```

(e0013)=
## E0013: values that cannot be compared with `==`

`==` and `!=` compare values part by part, but functions and handles such as sockets have no
parts to compare, so a value that is or holds one cannot be compared. A type parameter is
compared only when it is bounded by `eq`, as in `[T: eq]`.

This program fails with E0013:

```stone
fn double(n: int) -> int:
    ret n * 2

fn triple(n: int) -> int:
    ret n * 3

print(double == triple)
```

This one is fixed:

```stone
fn double(n: int) -> int:
    ret n * 2

fn triple(n: int) -> int:
    ret n * 3

print(double(1) == triple(1))
```

(e0014)=
## E0014: values that cannot be ordered

`<`, `>`, `sort`, `min`, and `max` need values that have an order: ints, floats, strings,
bytes, or a type that implements `ord` with a `cmp` method. A struct has no order until it
implements one.

This program fails with E0014:

```stone
struct Point:
    x: int
    y: int

a = Point(x=1, y=2)
b = Point(x=2, y=1)
print(a < b)
```

This one is fixed:

```stone
struct Point:
    x: int
    y: int

impl ord for Point:
    fn cmp(self, other: Self) -> ordering:
        ret self.x.cmp(other.x)

a = Point(x=1, y=2)
b = Point(x=2, y=1)
print(a < b)
```

(e0015)=
## E0015: a map key or set element of a type that cannot be one

A map's keys and a set's elements are ints, strings, or bools, which hash and compare the same
in both of stone's backends. Make a key of another value with a string, as in `"{x},{y}"`.

This program fails with E0015:

```stone
seen = {[1, 2]: true}
print(seen.len())
```

This one is fixed:

```stone
seen = {"1,2": true}
print(seen.len())
```

(e0016)=
## E0016: a change to a value that cannot change

Strings, bytes, and tuples never change, and neither does a slice or a value that is not
stored anywhere, so an assignment to one of their elements is an error. Make a new value
instead, as in `name = "S" + name[1:]`.

This program fails with E0016:

```stone
name = "stone"
name[0] = "S"
print(name)
```

This one is fixed:

```stone
name = "stone"
name = "S" + name[1:]
print(name)
```

(e0017)=
## E0017: a change to a variable a function does not own

stone's values are copies, so a function sees its own copy of each argument and cannot change
a global, and a lambda cannot change the variables it captures, since it holds copies of them.
To change a value in a function, pass it in and return it, as in `xs = f(xs)`, which stone
turns into a change in place.

This program fails with E0017:

```stone
scores = [1, 2]

fn add_score(n: int):
    scores.append(n)

add_score(3)
print(scores)
```

This one is fixed:

```stone
fn add_score(scores: list[int], n: int) -> list[int]:
    scores.append(n)
    ret scores

scores = [1, 2]
scores = add_score(scores, 3)
print(scores)
```

(e0018)=
## E0018: a value changed and used in the same statement

A call that changes a value, such as `xs.pop()`, may be part of a larger expression, but
nothing else in that statement may use the value it changes, since which one comes first would
decide the result. Make the change in a statement of its own.

This program fails with E0018:

```stone
xs = [1, 2, 3]
print(xs.pop().unwrap() + xs.len())
```

This one is fixed:

```stone
xs = [1, 2, 3]
last = xs.pop().unwrap()
print(last + xs.len())
```

(e0019)=
## E0019: an assignment to something that is not a variable or a place

Only a variable, an element such as `xs[i]`, or a field such as `p.x` can be assigned, and a
tuple is unpacked only into names. A function, builtin, enum, or struct name cannot be
assigned.

This program fails with E0019:

```stone
fn answer() -> int:
    ret 42

answer = 41
print(answer())
```

This one is fixed:

```stone
fn answer() -> int:
    ret 42

guess = 41
print(guess, answer())
```

(e0020)=
## E0020: a tuple used as something it is not

A tuple holds a fixed number of values of fixed types, so it cannot be iterated, sliced, or
indexed by a variable, and it unpacks only into as many names as it has values. Unpack it to
name its values, as in `n, name = pair`, or use a list.

This program fails with E0020:

```stone
pair = (1, "one")
for x in pair:
    print(x)
```

This one is fixed:

```stone
pair = (1, "one")
n, name = pair
print(n, name)
```

(e0021)=
## E0021: a name defined twice

A function, type, trait, parameter, field, variant, or method is defined twice in the same
place, so a use of the name could mean either. stone has no overloading: give each its own
name.

This program fails with E0021:

```stone
fn area(r: float) -> float:
    ret 3.14 * r * r

fn area(w: float, h: float) -> float:
    ret w * h

print(area(2.0))
```

This one is fixed:

```stone
fn circle_area(r: float) -> float:
    ret 3.14 * r * r

fn rectangle_area(w: float, h: float) -> float:
    ret w * h

print(circle_area(2.0), rectangle_area(2.0, 3.0))
```

(e0022)=
## E0022: a name that belongs to stone

A program defines something with the name of a builtin, such as `print` or `input`, a builtin
method, a builtin module, or the prelude. Those names always mean stone's own, so pick
another.

This program fails with E0022:

```stone
fn input() -> str:
    ret "ada"

print(input())
```

This one is fixed:

```stone
fn name() -> str:
    ret "ada"

print(name())
```

(e0023)=
## E0023: a statement or expression where it is not allowed

Some code can only appear in certain places: functions, types, traits, and `impl` blocks at
the top level of a file, `use` before other statements, `break` and `cont` inside a loop,
`range` as the iterable of a `for` loop, and a slice inside brackets.

This program fails with E0023:

```stone
for i in range(3):
    print(i)
break
```

This one is fixed:

```stone
for i in range(3):
    if i == 2:
        break
    print(i)
```

(e0024)=
## E0024: a name used as a value that is not one

An enum, a struct, a module, or a builtin is not itself a value. A value of an enum is one of
its variants, a struct's value is made with a call, and to pass a builtin around, define a
function that calls it.

This program fails with E0024:

```stone
enum Color:
    Red
    Green

c = Color
print(c)
```

This one is fixed:

```stone
enum Color:
    Red
    Green

c = Color.Red
print(c, Color.Green)
```

(e0025)=
## E0025: something called the wrong way

A method takes `self` and is called on a value, as in `c.count()`, while an associated
function has no `self` and is called on its struct, as in `Counter.new()`. A method must be
called, a constant is not a function, and only a function value can be called.

This program fails with E0025:

```stone
struct Counter:
    n: int

impl Counter:
    fn new() -> Counter:
        ret Counter(n=0)

c = Counter(n=1)
print(c.new())
```

This one is fixed:

```stone
struct Counter:
    n: int

impl Counter:
    fn new() -> Counter:
        ret Counter(n=0)

c = Counter(n=1)
print(Counter.new(), c)
```

(e0026)=
## E0026: a struct or variant made with the wrong values

A struct is made with a named value for every field, in any order, as in `Point(x=1, y=2)`, and
a variant with its values in order, or by name for a variant with named values. A missing,
repeated, or unknown field is an error, and so is a variant given the wrong number of values.

This program fails with E0026:

```stone
struct Point:
    x: int
    y: int

p = Point(1, 2)
print(p)
```

This one is fixed:

```stone
struct Point:
    x: int
    y: int

p = Point(x=1, y=2)
print(p)
```

(e0027)=
## E0027: a `match` that does not cover every value

A `match` must have a case for every value its subject can have, so that no value falls
through. The error names a value no case covers; add a case for it, or `case _:` for every
value the other cases leave out.

This program fails with E0027:

```stone
enum Shape:
    Circle(float)
    Square(float)

fn describe(s: Shape):
    match s:
        case Shape.Circle(r):
            print("a circle of radius {r}")

describe(Shape.Square(2.0))
```

This one is fixed:

```stone
enum Shape:
    Circle(float)
    Square(float)

fn describe(s: Shape):
    match s:
        case Shape.Circle(r):
            print("a circle of radius {r}")
        case Shape.Square(side):
            print("a square of side {side}")

describe(Shape.Square(2.0))
```

(e0028)=
## E0028: a pattern that does not fit

A `case` pattern does not fit the value it matches: a variant with the wrong number of values,
a variant of another enum, a name bound twice, or alternatives joined by `|` that bind
different names. A bare name in a pattern binds a new variable; write a variant with its
enum, as in `Shape.Circle(r)`.

This program fails with E0028:

```stone
enum Shape:
    Circle(float)
    Square(float)

s = Shape.Circle(1.0)
match s:
    case Shape.Circle(r, extra):
        print(r)
    case _:
        print("not a circle")
```

This one is fixed:

```stone
enum Shape:
    Circle(float)
    Square(float)

s = Shape.Circle(1.0)
match s:
    case Shape.Circle(r):
        print(r)
    case Shape.Square(side):
        print(side)
```

(e0029)=
## E0029: a type that would never end

A struct that holds itself directly would never end, since each value would hold another. Hold
it through a list or an option, as in `next: option[Node]`. The same code covers a generic type
or function whose types would grow without end.

This program fails with E0029:

```stone
struct Node:
    value: int
    next: Node

print("never")
```

This one is fixed:

```stone
struct Node:
    value: int
    next: option[Node]

n = Node(value=1, next=none)
print(n.value)
```

(e0030)=
## E0030: a type parameter used where it cannot be

A type parameter stands for any type, so it is only meaningful inside its own definition: it
cannot be the type of a global, and only a function's type parameters can have bounds for now.
A struct's or enum's type parameter takes none.

This program fails with E0030:

```stone
struct Wrapper[T: display]:
    value: T

w = Wrapper(value=1)
print(w.value)
```

This one is fixed:

```stone
struct Wrapper[T]:
    value: T

w = Wrapper(value=1)
print(w.value)
```

(e0031)=
## E0031: a type that does not implement a trait it needs

A generic function's bounds say which traits its type parameters must implement, and a call
gives it a type that does not implement one of them. Implement the trait for the type, or pass
a value of a type that implements it.

This program fails with E0031:

```stone
fn smallest[T: ord](a: T, b: T) -> T:
    if a < b:
        ret a
    ret b

struct Point:
    x: int

print(smallest(Point(x=1), Point(x=2)))
```

This one is fixed:

```stone
fn smallest[T: ord](a: T, b: T) -> T:
    if a < b:
        ret a
    ret b

print(smallest(3, 2))
```

(e0032)=
## E0032: an impl block that leaves out a trait's methods

An `impl Trait for Type` block must implement every method of the trait that has no default
body. The error names the methods it leaves out.

This program fails with E0032:

```stone
trait Shape:
    fn area(self) -> float
    fn name(self) -> str

struct Square:
    side: float

impl Shape for Square:
    fn area(self) -> float:
        ret self.side * self.side

print(Square(side=2.0).area())
```

This one is fixed:

```stone
trait Shape:
    fn area(self) -> float
    fn name(self) -> str

struct Square:
    side: float

impl Shape for Square:
    fn area(self) -> float:
        ret self.side * self.side

    fn name(self) -> str:
        ret "square"

print(Square(side=2.0).area())
```

(e0033)=
## E0033: a method that does not match its trait

A method that implements a trait takes and returns exactly what the trait's method says, takes
`self` first, and changes `self` only if the trait's method does. The error shows the
signature to write.

This program fails with E0033:

```stone
trait Shape:
    fn area(self) -> float

struct Square:
    side: float

impl Shape for Square:
    fn area(self) -> int:
        ret 4

print(Square(side=2.0).area())
```

This one is fixed:

```stone
trait Shape:
    fn area(self) -> float

struct Square:
    side: float

impl Shape for Square:
    fn area(self) -> float:
        ret self.side * self.side

print(Square(side=2.0).area())
```

(e0034)=
## E0034: an impl block that is not allowed

An `impl` block is not allowed where it would change what stone already defines or what
another file decides: a builtin type already implements `display`, `eq`, `ord`, and its
operators, two impls of one trait cannot overlap, and an impl goes in the file of its type or
of its trait.

This program fails with E0034:

```stone
impl display for int:
    fn display(self) -> str:
        ret "a number"

print(1)
```

This one is fixed:

```stone
struct Money:
    cents: int

impl display for Money:
    fn display(self) -> str:
        ret "{self.cents / 100}.{self.cents % 100:02} dollars"

print(Money(cents=1234))
```

(e0035)=
## E0035: a call that could mean more than one function

A call or operator could reach more than one function and nothing decides which: a method that
two bounds of a type parameter both have, or a method or operator whose arguments' types are
not known yet. Rename one of the methods, or give the arguments' types.

This program fails with E0035:

```stone
trait Named:
    fn label(self) -> str

trait Titled:
    fn label(self) -> str

fn show[T: Named + Titled](x: T):
    print(x.label())
```

This one is fixed:

```stone
trait Named:
    fn label(self) -> str

trait Titled:
    fn title(self) -> str

struct Book:
    name: str

impl Named for Book:
    fn label(self) -> str:
        ret self.name

impl Titled for Book:
    fn title(self) -> str:
        ret "The {self.name}"

fn show[T: Named + Titled](x: T):
    print(x.label(), x.title())

show(Book(name="Stone"))
```

(e0036)=
## E0036: a value whose type must be known first

Reading a field, calling a method, or converting a value needs its type, and nothing has said
what it is by then, such as a lambda's parameter whose type no use gives. Write the type, as
in `|p: Point| p.x`.

This program fails with E0036:

```stone
struct Point:
    x: int

get_x = |p| p.x
print(get_x(Point(x=1)))
```

This one is fixed:

```stone
struct Point:
    x: int

get_x = |p: Point| p.x
print(get_x(Point(x=1)))
```

(e0037)=
## E0037: a conversion that does not exist

`T(x)` converts `x` to the type `T` only if `T` implements `from` or `try_from` for `x`'s type,
or stone has the conversion builtin, such as `float(n)` or `int(s)`. There is no conversion
from a list to an int; say what the int should be, as in `xs.len()`.

This program fails with E0037:

```stone
xs = [1, 2]
n = int(xs)
print(n)
```

This one is fixed:

```stone
xs = [1, 2]
n = xs.len()
print(n)
```

(e0038)=
## E0038: `?` where it cannot return

`x?` gives the value of an `ok` or a `some`, or returns the `err` or `none` from the
function it is in, so that function must return a result with the same error type, or an
option. `?` cannot appear at the top level.

This program fails with E0038:

```stone
fn parse(s: str) -> int:
    ret int(s)?

print(parse("4"))
```

This one is fixed:

```stone
fn parse(s: str) -> result[int, str]:
    ret ok(int(s)? * 2)

print(parse("4").unwrap())
```

(e0039)=
## E0039: a value that holds too many values

A list element, map entry, variant, or closure holds at most 32 values, counting each value of
a tuple or struct inside it. Hold the values in a list instead.

This program fails with E0039:

```stone
rows = [(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)]
print(rows.len())
```

This one is fixed:

```stone
row = []
for i in range(33):
    row.append(i)
rows = [row]
print(rows.len())
```

(e0040)=
## E0040: `self` or `Self` where it does not belong

`self` is the first parameter of a method and takes no type, since it is the value the method
is called on. `Self` names the type of a trait or an `impl` block, so it means nothing outside
one.

This program fails with E0040:

```stone
struct Counter:
    n: int

impl Counter:
    fn get(self: Counter) -> int:
        ret self.n

print(Counter(n=1).get())
```

This one is fixed:

```stone
struct Counter:
    n: int

impl Counter:
    fn get(self) -> int:
        ret self.n

print(Counter(n=1).get())
```

(e0041)=
## E0041: a test that reads a global

`stone test` runs each test without the file's top-level code, so a global a test reads,
directly or through the functions it calls, would have no value. Pass the value as an
argument instead.

This program fails with E0041:

```stone
LIMIT = 10

fn clamp(n: int) -> int:
    ret n.min(LIMIT)

test "clamp keeps small numbers":
    assert clamp(3) == 3

print(clamp(30))
```

This one is fixed:

```stone
fn clamp(n: int, limit: int) -> int:
    ret n.min(limit)

test "clamp keeps small numbers":
    assert clamp(3, 10) == 3

print(clamp(30, 10))
```

(e0042)=
## E0042: `in` chained with another comparison

`in` and `not in` never chain with other comparisons, since `a in b == c` reads alike as two
different tests. Write each test on its own, joined by `and` if needed.

This program fails with E0042:

```stone
xs = [1, 2, 3]
print(1 in xs == true)
```

This one is fixed:

```stone
xs = [1, 2, 3]
print(1 in xs)
```

(e0043)=
## E0043: a format spec that does not fit its value

A format spec after `:` in a string's value, as in `{x:.2f}`, follows Python's mini-language,
and its type must fit the value: `f` and `e` take numbers, `d` and `x` take ints, and `s`
takes a string. Values inside a spec are not supported yet.

This program fails with E0043:

```stone
name = "ada"
print("{name:.2f}")
```

This one is fixed:

```stone
name = "ada"
print("{name:>8}")
```

(e0044)=
## E0044: a module that does not exist

`use a.b` loads the module in `a/b.stn` under the program's root, the directory of its entry
file, or a builtin module such as `math`, or a dependency named in `stone.toml`. No file or
builtin module has that name.

This program fails with E0044:

```stone
use geometry

print(geometry.area(1.0))
```

This one is fixed:

`main.stn`:

```stone
use geometry

print(geometry.area(1.0))
```

`geometry.stn`:

```stone
pub fn area(r: float) -> float:
    ret 3.14 * r * r
```

(e0045)=
## E0045: a name that is private to its module

A module's functions, types, and traits are private to it unless defined with `pub`, so
another file cannot use them. Write `pub` before the definition to share it.

This program fails with E0045:

`main.stn`:

```stone
use geometry

print(geometry.area(1.0))
```

`geometry.stn`:

```stone
fn area(r: float) -> float:
    ret 3.14 * r * r
```

This one is fixed:

`main.stn`:

```stone
use geometry

print(geometry.area(1.0))
```

`geometry.stn`:

```stone
pub fn area(r: float) -> float:
    ret 3.14 * r * r
```

(e0046)=
## E0046: a feature the target does not support yet

Some functions are not supported on every target yet: Windows has no Unix sockets, so
`net.connect_unix` is an error in a build for Windows, and when running or checking on
Windows. Use TCP through `net.connect` instead.

This program fails with E0046:

```stone
use net

result = net.connect_unix("/tmp/demo.sock")
print(result.is_ok())
```

This one is fixed:

```stone
use net

result = net.connect("localhost", 8080)
print(result.is_ok())
```

Only a build for `x86_64-windows` reports this error.

(e0047)=
## E0047: a mistake in `stone.toml` or `stone.lock`

A program's `stone.toml` gives its package's name and version under `[package]` and its
dependencies under `[dependencies]`, and `stone.lock`, which stone writes, records what each
dependency held. The error points at the key or value that is not valid.

This program fails with E0047:

`stone.toml`:

```toml
[package]
name = "demo"
verison = "0.1.0"
```

`main.stn`:

```stone
print("hello")
```

This one is fixed:

`stone.toml`:

```toml
[package]
name = "demo"
version = "0.1.0"
```

`main.stn`:

```stone
print("hello")
```

(e0048)=
## E0048: a dependency that cannot be used

A dependency named in `stone.toml` cannot be used: its directory or download is missing, its
own `stone.toml` has errors, its release says it is another version, or its files no longer
match `stone.lock`. Run `stone fetch` to download what is missing.

This program fails with E0048:

`stone.toml`:

```toml
[package]
name = "demo"
version = "0.1.0"

[dependencies]
geo = { path = "vendor/geo" }
```

`main.stn`:

```stone
use geo

print(geo.area(1.0))
```

This one is fixed:

`stone.toml`:

```toml
[package]
name = "demo"
version = "0.1.0"

[dependencies]
geo = { path = "vendor/geo" }
```

`main.stn`:

```stone
use geo

print(geo.area(1.0))
```

`vendor/geo/stone.toml`:

```toml
[package]
name = "geo"
version = "0.1.0"
```

`vendor/geo/lib.stn`:

```stone
pub fn area(r: float) -> float:
    ret 3.14 * r * r
```

(e0049)=
## E0049: a value that cannot be a trait value

A trait's name is a type of values of any type that implements the trait, as `list[Shape]`
holds circles and squares alike, each called through a table of its type's methods. That table
can only call a method whose types are the same for every type: one that takes `self` first,
never takes or returns another `Self`, and has no type parameters of its own, of a trait that has
none either. A trait such as `eq`, whose `eq` takes another `Self`, can be a bound, as in
`fn f[T: eq](a: T, b: T)`, but not a type of values.

A trait value also holds at most 32 values, counting each field of a struct, and cannot hold a
socket, a listener, or a function yet.

This program fails with E0049:

```stone
trait Grows:
    fn bigger(self, other: Self) -> bool

struct Plant:
    height: float

impl Grows for Plant:
    fn bigger(self, other: Self) -> bool:
        ret self.height > other.height

fn tallest(xs: list[Grows]) -> int:
    ret xs.len()

print(tallest([Plant(height=1.0)]))
```

This one is fixed:

```stone
trait Grows:
    fn bigger(self, other: Self) -> bool

struct Plant:
    height: float

impl Grows for Plant:
    fn bigger(self, other: Self) -> bool:
        ret self.height > other.height

fn tallest[T: Grows](xs: list[T]) -> int:
    ret xs.len()

print(tallest([Plant(height=1.0)]))
```

(e0050)=
## E0050: a constant that is not one

A constant's value is worked out when the program is built, so it is made only of literals
(numbers, `true` and `false`, strings without values in braces, and bytes), other constants,
`math.pi`, `math.tau`, and `math.e`, the operators, and tuples of them. A call, a variable, a
list, or a map is not known until the program runs; keep such a value in a function, or in a
variable at the top level of the entry file. An operation that would fail when the program runs,
such as a division by zero, fails the build instead.

A constant is never assigned again, and no variable or parameter takes its name, so its name
always means its value.

This program fails with E0050:

```stone
fn kilobytes(n: int) -> int:
    ret n * 1024

const buffer = kilobytes(64)

print(buffer)
```

This one is fixed:

```stone
const buffer = 64 * 1024

print(buffer)
```

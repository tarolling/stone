# Writing stone: a guide for coding agents

You are going to write programs in stone, a small language that looks like Python but behaves
like Rust: static types, values that are copied instead of shared, and errors reported before the
program runs. Your Python habits will get the syntax mostly right and the semantics sometimes
wrong. This guide covers what you need to write correct stone the first time, and lists what
Python has that stone does not, so you do not reach for it.

Full documentation: <https://tarolling.github.io/stone/>. The pages to keep open are
[syntax](https://tarolling.github.io/stone/reference/syntax.html),
[types](https://tarolling.github.io/stone/reference/types.html), and
[builtins](https://tarolling.github.io/stone/reference/builtins.html).

## Your loop

Install with `curl -fsSL https://raw.githubusercontent.com/tarolling/stone/main/install.sh | sh`
(it puts `stone` in `~/.local/bin`). Source files end in `.stn`.

| command | does |
| --- | --- |
| `stone check app.stn` | type checks without running; run it after every edit |
| `stone run app.stn [args...]` | interprets the program; heavy loops can run slower than in Python |
| `stone build app.stn` | compiles a standalone executable to `build/app` next to the file; use it for large inputs and timing |
| `stone` | starts a REPL |

Errors print as `app.stn:3:7: error: message`, with the source line and a caret under the
problem, and exit with status 1. Warnings use the same format with `warning:`. The messages are
written to be acted on: they usually say what to write instead, so read them before guessing.
Runtime errors (an index out of range, `unwrap` of `none`, division by zero) print the same way
under both `run` and `build`. Fix warnings too: each one points at a likely bug, such as a change
that is lost or `[0] * n` meaning something other than in Python.

## The syntax, in one program

```stone
// comments start with //, and strings use double quotes only
use math

// every parameter has a type; '-> T' if it returns a value
fn classify(n: int) -> str:
    if n < 0:
        ret "negative"
    elif n == 0:
        ret "zero"
    else:
        ret "positive"

// no '->' means no value: calls to it are statements
fn report(label: str, values: list[int]):
    total = 0
    for v in values:
        if v == 0:
            cont
        total += v
    print(label, total, classify(total))

nums = [3, -1, 0, 4]
report("sum", nums)
for i in range(nums.len()):
    print(i, nums[i], math.abs(nums[i]))
done = true
while done:
    done = false
print("last:", nums[-1], "reversed:", nums[::-1])
```

The differences from Python here: `fn` instead of `def`, a type on every parameter, `-> T` for a
return value, `ret` instead of `return`, `cont` instead of `continue`, `true`/`false`, `//`
comments, double-quoted strings only, `xs.len()` instead of `len(xs)`, and `math.abs` instead of
`abs`. Variables are never annotated; their types are inferred. `for _ in range(n):` works for
a loop variable you do not need, and `main` is an ordinary function name.

## Types

| type | example | notes |
| --- | --- | --- |
| `int` | `42` | 64-bit; overflow wraps around |
| `float` | `1.5`, `2e-3` | never mixed with `int`: `float(n)`, `int(x)` |
| `bool` | `true` | the only type a condition accepts |
| `str` | `"hi\n"` | immutable UTF-8 bytes; `s[i]`, `s[a:b]`, `+` to join |
| `list[T]` | `[1, 2]`, `[]` | all elements one type |
| `map[K, V]` | `{"a": 1}`, `{}` | keys `int`, `str`, or `bool`; insertion order |
| `tuple[A, B]` | `(1, "a")` | two or more values, always in parentheses |
| `option[T]` | `some(1)`, `none` | stone's replacement for `None` |
| `result[T, E]` | `ok(1)`, `err("bad")` | stone's replacement for exceptions |
| struct | `Point(x=1.0, y=2.0)` | named, typed fields |
| enum | `Shape.Circle(1.0)` | variants that hold values, as in Rust |

### Lists, maps, and tuples

```stone
fn divide(a: int, b: int) -> tuple[int, int]:
    ret (a / b, a % b)

xs = []
for i in range(5):
    xs.append(i * i)
xs.insert(0, 100)
xs.extend([7, 8])
xs.sort()
print(xs, xs[1:3], xs.len())

ages = {"ann": 31, "bob": 27}
ages["cy"] = 45
ages["ann"] += 1
print(ages, ages.contains("dee"), ages.get("bob").unwrap_or(0))
for name in ages:
    print(name, ages[name])
for name, age in ages.items():
    print(name, age)
gone = ages.pop("bob")
print(gone, ages.keys(), ages.values())

q, r = divide(17, 5)
pairs = [("a", 1), ("b", 2)]
for name, n in pairs:
    print(name, n, q, r)
```

`for k in m` goes through a map's keys in insertion order; `m.keys()`, `m.values()`, and
`m.items()` return new lists. `m[k]` stops the program if `k` is missing; use `m.contains(k)` or
`m.get(k)` when it might be. `m.pop(k)` returns an option and, like a list's `pop`, may be used
inside any expression, as in `m.pop(k).unwrap_or(0)`. Entries change in place like list elements, through any
place: `m[k] += 1`, `m[k].append(x)`, `p.counts[k] = 0`. Only `m[k] = v` adds a missing key; `+=`
and changes inside an entry need the key to exist. A tuple is indexed only by a literal (`t[0]`,
`t[-1]`) and never changes.

### Options

```stone
fn find_index(xs: list[str], target: str) -> option[int]:
    for i in range(xs.len()):
        if xs[i] == target:
            ret some(i)
    ret none

names = ["ann", "bob"]
match find_index(names, "bob"):
    case some(i):
        print("found at", i)
    case none:
        print("missing")
print(find_index(names, "zed").is_none(), find_index(names, "ann").unwrap())
```

`find`, `pop()`, `get`, and `os.env` return options too: `"abc".find("c")` is `some(2)`, not an
index or -1, so write `s.find(t).unwrap_or(-1)` where Python code compares with -1. `pop()` gives
an option as well, so `xs.pop().unwrap()` takes the last element and `while xs.pop().is_some():`
empties a list; since it changes the list, nothing else in the same statement may use it (see
below).

### Results and ?

```stone
fn parse_pair(line: str) -> result[tuple[int, int], str]:
    parts = line.split(",")
    if parts.len() != 2:
        ret err("expected two numbers in '" + line + "'")
    ret ok((parts[0].parse_int()?, parts[1].parse_int()?))

for line in ["3,4", "3", "3,x"]:
    match parse_pair(line):
        case ok(pair):
            print("pair", pair)
        case err(message):
            print("error:", message)
print(parse_pair("1,2").unwrap(), parse_pair("1").is_err())
```

A function that can fail returns `result[T, E]`: `ok(x)` or `err(e)`. `?` after a result gives
the `ok` value, or returns the `err` from the enclosing function at once; it is only allowed in a
function that returns a `result` with the same error type (`?` on an option needs a function
returning an option, and `r.ok()?` turns a result into one). `s.parse_int()` and
`s.parse_float()` return `result[int, str]` and `result[float, str]` instead of stopping the
program as `int(s)` and `float(s)` do. Results have `is_ok`, `is_err`, `unwrap`, `unwrap_or`,
`unwrap_err`, `ok()`, and `err()`.

### Structs and methods

```stone
struct Account:
    owner: str
    balance: int
    history: list[int]

impl Account:
    // an associated function: called as Account.open(...)
    fn open(owner: str) -> Account:
        ret Account(owner=owner, balance=0, history=[])

    // a method that changes self: stone sees that and changes the receiver in place
    fn deposit(self, amount: int):
        self.balance += amount
        self.history.append(amount)

    fn summary(self) -> str:
        ret self.owner + ": " + str(self.balance)

a = Account.open("ann")
a.deposit(50)
a.deposit(25)
print(a.summary(), a.history)
print(a)
```

A struct is made by naming every field once, in any order. There is no inheritance and no
constructor: write an associated function. Methods go in an `impl` block in the struct's file.
`self` is the only untyped parameter.

### Enums and match

```stone
enum Shape:
    Circle(float)
    Rect(float, float)
    Empty

fn area(s: Shape) -> float:
    match s:
        case Shape.Circle(r):
            ret 3.14159 * r * r
        case Shape.Rect(w, h):
            ret w * h
        case Shape.Empty:
            ret 0.0

shapes = [Shape.Circle(1.0), Shape.Rect(2.0, 3.0), Shape.Empty]
for s in shapes:
    print(s, area(s))
```

A `match` must cover every variant (or end with `case _:`); a missing one is an error naming it.
Variants are always written with their enum, `Shape.Empty`, except `some`/`none`, `ok`/`err`,
and the `ordering` variants `less`/`equal`/`greater`, which are reserved names. An enum can hold itself, which is how you build
trees and linked lists.

### Generics and traits

```stone
trait Named:
    fn name(self) -> str
    fn greet(self) -> str:
        ret "hello, " + self.name()

struct Dog:
    nick: str

impl Named for Dog:
    fn name(self) -> str:
        ret self.nick

struct Version:
    major: int
    minor: int

impl ord for Version:
    fn cmp(self, other: Self) -> ordering:
        if self.major != other.major:
            ret self.major.cmp(other.major)
        ret self.minor.cmp(other.minor)

fn first[T](xs: list[T]) -> T:
    ret xs[0]

fn greet_all[T: Named](xs: list[T]):
    for x in xs:
        print(x.greet())

greet_all([Dog(nick="rex"), Dog(nick="fido")])
vs = [Version(major=2, minor=0), Version(major=1, minor=5)]
vs.sort()
print(first(vs), first(["a", "b"]), vs[0] < vs[1])
```

Inside a generic function a `T` can only be moved, stored, returned, and printed, plus the methods
of its bounds. `[T: eq]` allows `==`, and `[T: ord]` allows `<` and `sort`. Implementing `ord`
(with `cmp`) is how you sort a list of structs; there is no `key=`. Functions cannot be passed as
values, so traits are also how you parameterize behavior.

## Values are copied: the one idea to learn

Lists, maps, and structs are values. Assigning one or passing it to a function behaves as a copy
(it is copy-on-write, so it is cheap). Consequences:

- **A function cannot change its caller's list.** It returns the changed value, and the caller
  assigns it: `xs = add_item(xs, 3)`. stone turns this into an in-place update, so it costs
  nothing.
- **A function cannot change a global.** Pass it in and return it.
- **A change is a statement of its own**, made to a variable or an element or field of one:
  `xs.append(1)`, `xs[i] = v`, `p.x = 1.0`, `grid[r][c] += 1`, `ps[i].deposit(5)`. So
  `ys = xs.append(1)` and `f().append(1)` are errors. The exception is `pop` and other methods
  that change their receiver and return a value: those may be inside any expression,
  `print(xs.pop().unwrap())`, as long as nothing else in that statement uses what they change,
  so `print(xs, xs.pop())` is an error; write `x = xs.pop()` first.
- **Methods are the exception that changes in place**: a method that changes `self` changes the
  variable it was called on, as `a.deposit(50)` did above.

```stone
fn with_bonus(scores: list[int], bonus: int) -> list[int]:
    for i in range(scores.len()):
        scores[i] += bonus
    ret scores

scores = [10, 20]
backup = scores
scores = with_bonus(scores, 5)
print(scores, backup)
```

This prints `[15, 25] [10, 20]`: `backup` kept its own copy. The Python habit of a function that
mutates its argument and returns nothing gets this warning, and the change is lost:

```text
warning: changes to 'xs' are lost when 'add' returns, since parameters are copies; return it and
assign the result where 'add' is called
```

## Rules the checker enforces

- No implicit conversion: `1 + 2.0` is an error. Convert with `float(n)` or `int(x)`, and build
  strings with `str(n)`: `"n = " + str(n)`.
- Conditions must be `bool`: write `if xs.len() > 0:`, `if s != "":`, `if n != 0:`.
- `==` does not compare lists or maps (it does compare numbers, strings, bools, tuples, structs,
  options, and enums). Compare lists with a loop.
- `/` on ints truncates toward zero, and `%` takes the dividend's sign: `-7 / 2 == -3`,
  `-7 % 2 == -1`. There is no `//`; that starts a comment.
- `**` takes an `int` exponent, which cannot be negative on an int: use `2.0 ** -1`, and
  `math.sqrt` for square roots.
- Arithmetic on lists is element by element, as in numpy: `[1, 2] * 2` is `[2, 4]`,
  `[1.0, 2.0] + [3.0, 4.0]` is `[4.0, 6.0]`. So `[0] * n` is not `n` zeros (the checker warns),
  and `+` does not join lists; use `extend`.
- `int(x)` on a float truncates toward zero, as in Python (`int(-2.7) == -2`), and stops the
  program on nan or a float too large for an int.
- Every variable is assigned on every path before it is read, and keeps one type: after
  `total = 0`, `total = 0.0` is an error (`cannot assign float to 'total', which is int`), so use
  a second name.
- A function reads globals but cannot assign or change them, and its parameters and locals may
  share a global's name.
- An empty `[]` or `{}` gets its type from later code, read top to bottom, so reading a field
  or calling a method on one of its elements before anything has settled its type is an error
  (`the type of this value must be known before its field 'n' is read`). Put the `append` first
  in the code, or start from a non-empty value.
- Functions, structs, enums, traits, and `impl` blocks are top level only.
- Recursion is limited to 1000 nested calls.

## Not in stone, and what to write instead

| Python | stone |
| --- | --- |
| `[f(x) for x in xs if p(x)]` | a `for` loop with `if` and `append` |
| `f"{a} and {b}"`, `"%d" % n`, `.format` | `str(a) + " and " + str(b)` |
| `a if cond else b` | an `if`/`else` assigning a variable |
| `x in xs` | a loop, or a helper `fn contains(xs: list[int], x: int) -> bool` |
| `k in d` | `d.contains(k)` |
| `for k, v in d.items():` | the same; `d.items()` is a list of tuples |
| `d.pop(k)`, `d.pop(k, default)` | `d.pop(k)` returns an option: `.unwrap_or(default)` |
| `del d[k]` | `d.pop(k)` as a statement |
| `set()` | `map[K, bool]` |
| `def f(x, n=1)`, `f(n=2)` | every argument, positionally (only structs take names) |
| `lambda`, passing a function | a trait and a generic function with a bound |
| `try`/`except`, `raise` | return a `result` and pass errors on with `?`; `s.parse_int()` instead of `int(s)` |
| `class` with inheritance | a `struct` plus `impl`, an `enum` for alternatives, a `trait` for shared behavior |
| `None`, `Optional[T]` | `none`, `option[T]` |
| `pass` | restructure so every block has a real statement |
| `del`, `global`, `nonlocal`, `is`, `with`, `yield` | not available |
| `len(x)`, `abs`, `min`, `max` | `x.len()`, `math.abs`, `math.min`, `math.max` |
| `sorted(xs)`, `xs.sort(key=f, reverse=True)` | `ys = xs` then `ys.sort()`; `impl ord` for custom orders, reverse with `ys[::-1]` |
| `"ab" * 3`, `s.upper()`, `s.replace(...)` | loops over characters; only the string methods below exist |
| `print(x, end="")` | build the line as a string, then `print` it once |
| `open(path).read()`, `.readlines()`, `f.write(s)` | `fs.read(path)`, `fs.read_lines(path)`, `fs.write(path, s)`, each a `result` |
| `os.listdir`, `os.path.exists`, `os.remove`, `os.mkdir` | `fs.list_dir` (sorted), `fs.exists`, `fs.remove`, `fs.make_dir` |
| `sys.argv[1:]`, `sys.exit(n)` | `args()`, `os.exit(n)` |

## Builtins

Functions: `print(values...)` (space-separated, newline), `range(end)` and `range(start, end)`
(only as a `for` iterable), `int(x)`, `float(x)`, `str(x)`, `input()` and `input(prompt)`,
`eof()`, `args()`.

Methods: `len` (str, list, map); lists: `append`, `pop()` (option) and `pop(i)`, `insert`,
`extend`, `sort`; strings: `strip`, `split()` and `split(sep)`, `sep.join(xs)`, `find`
(option), `startswith`, `endswith`; options: `is_some`, `is_none`, `unwrap`, `unwrap_or`; maps:
`contains`, `get` (option), `pop(key)` (option), `keys`, `values`, `items`; any comparable
value: `eq`, `cmp`.

Builtin modules, imported with `use math` (or `use math.sqrt`):

- `math`: `abs`, `min`, `max` (two or more numbers, or a list), `sqrt`, `floor`
- `random`: `seed`, `random`, `randint(low, high)`, `choice(xs)`
- `time`: `now`, `clock`, `sleep(seconds)`
- `os`: `env(name)` (option), `has_env`, `platform`, `arch`, `hostname`, `cpu_count`, `pid`,
  `cwd`, `exit(code)`
- `fs`: `read(path)`, `read_lines(path)`, `write(path, text)`, `append(path, text)`,
  `list_dir(path)` (sorted names), `exists(path)` (bool), `remove(path)`, `make_dir(path)`;
  all but `exists` return a `result` whose error is a message such as
  `no such file or directory: 'data.txt'`

Reading all of standard input:

```stone
total = 0
while not eof():
    line = input().strip()
    if line == "":
        cont
    total += int(line)
print(total)
```

Files, with errors passed on by `?`:

```stone
use fs

fn save(path: str, values: list[int]) -> result[int, str]:
    fs.write(path, "")?
    for v in values:
        fs.append(path, str(v) + "\n")?
    ret ok(values.len())

fn total(path: str) -> result[int, str]:
    sum = 0
    for line in fs.read_lines(path)?:
        sum += line.parse_int()?
    ret ok(sum)

print(save("numbers.txt", [1, 2, 3]))   // ok(3)
print(total("numbers.txt"))             // ok(6)
print(total("missing.txt"))             // err("no such file or directory: 'missing.txt'")
fs.remove("numbers.txt").unwrap()
```

## Modules

A program is an entry file plus the `.stn` files it uses, named by path from the entry file's
directory: `geometry/shapes.stn` is `use geometry.shapes`, called as `shapes.area(r)`, or
`use geometry.shapes.area` to call `area(r)`. Only `pub fn`, `pub struct`, `pub enum`, and
`pub trait` are visible to other files. A module other than the entry file holds only `use`,
`fn`, `struct`, `enum`, `trait`, and `impl`, never top-level statements. A program of several
files conventionally starts at `main.stn`.

## Style

Indent with four spaces. Write comment lines directly above a function to document it (editors
show them on hover). Name things in `snake_case` and types in `PascalCase`. Keep `use` lines at
the top of the file.

## Where to look next

- Tutorial: <https://tarolling.github.io/stone/tutorial/index.html>
- Every runtime error message: <https://tarolling.github.io/stone/reference/runtime-errors.html>
- Example programs with expected output:
  <https://github.com/tarolling/stone/tree/main/docs/examples>
- Python programs next to their stone versions:
  <https://github.com/tarolling/stone/tree/main/bench/programs>

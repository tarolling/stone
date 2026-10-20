# Converting Python to stone: a guide for coding agents

You are going to convert Python code to stone. This guide builds on the stone guide for agents,
which teaches the language: read it first, from
<https://raw.githubusercontent.com/tarolling/stone/main/docs/agents/stone.md>, or from
`docs/agents/stone.md` in the stone repository. Most of the work is not syntax. It is replacing
dynamic features with static ones, and rewriting code that relies on two names sharing one list,
since stone copies values.

## Process

1. **Inventory before writing.** Read the whole Python program and list what it uses that stone
   lacks (see "Not in stone" in the stone guide): classes with inheritance, exceptions, lambdas
   and functions passed as values, comprehensions, f-strings, default and keyword arguments,
   sets, file I/O, and values whose type varies (a variable that is
   sometimes `None`, a name reused for an int and then a float, or a list that mixes types). Find
   arithmetic that could pass 64 bits, such as products in hashes and random number generators,
   and check that every intermediate value stays below 2^63, or the result will wrap. Decide how each will be translated
   before converting anything. If something cannot be translated (file I/O, networking,
   threads, third-party libraries), say so up front and ask whether to continue.
2. **Choose the types.** Write down a stone signature for every function: each parameter's type
   and the return type. A value that may be missing becomes `option[T]`; a value that is one of
   several shapes becomes an `enum`; a class becomes a `struct`; a function that mutates an
   argument returns the changed value instead.
3. **Convert one function at a time**, running `stone check` after each. Read every error and
   warning; they usually say exactly what to write.
4. **Compare outputs.** Run `python3 prog.py` and `stone run prog.stn` on the same input and
   arguments, and diff the output. Then run `stone build prog.stn` and diff the built program's
   output too. The interpreter can be much slower than Python on heavy loops, so for large
   inputs compare the built program instead of shrinking the input. Test edge cases that hit the traps below: negative numbers with `//` and `%`,
   empty input, and non-ASCII text.
5. **Report** what you changed in behavior, what you left out, and how you checked it (see the
   template at the end).

## Translating Python idioms

### Containers

| Python | stone |
| --- | --- |
| `len(xs)` | `xs.len()` |
| `xs + ys` | `zs = xs` then `zs.extend(ys)` (`+` adds element by element) |
| `[0] * n` | a loop of `append`s |
| `[[0] * w for _ in range(h)]` | a nested loop: build a row, then `grid.append(row)` |
| `for i, x in enumerate(xs):` | `for i in range(xs.len()):` then `x = xs[i]` |
| `for a, b in zip(xs, ys):` | `for i in range(xs.len()):` with `xs[i]` and `ys[i]` |
| `sum(xs)`, `max(xs)` | a loop; `math.max(xs)` and `math.min(xs)` for a list of numbers |
| `sorted(xs)` | `ys = xs` then `ys.sort()` (a copy, since lists are values) |
| `xs.sort(key=..., reverse=True)` | a struct with `impl ord`, whose `cmp` defines the order |
| `xs.remove(x)`, `xs.index(x)` | a loop that finds the index, then `pop(i)` |
| `d.get(k, default)` | `d.get(k).unwrap_or(default)` |
| `k in d` | `d.contains(k)` |
| `for k in d:`, `for k, v in d.items():` | the same, in insertion order |
| `d.keys()`, `d.values()`, `list(d.items())` | `d.keys()`, `d.values()`, `d.items()`: new lists |
| `d.pop(k)`, `del d[k]` | `d.pop(k)`, which returns an option, `none` if `k` is missing |
| `d.pop(k, default)` | `d.pop(k).unwrap_or(default)` |
| `d[k] += 1`, `d[k].append(x)` | the same, but the key must exist |
| `defaultdict(int)` | check `d.contains(k)` before reading or changing |
| `set()`, `s.add(x)`, `x in s` | `map[T, bool]`: `s[x] = true`, `s.contains(x)` |
| `collections.Counter` | a map of counts, or a list of structs as in the worked example |

An entry changes in place as a list's element does, through any place, but stone has no
default for a missing key: `d[k] += 1` and `d[k].append(x)` stop the program unless `d` has `k`,
so add it first, as `defaultdict` would:

```stone
// groups words by their first letter, keeping letters in the order they first appear
words = ["apple", "bean", "avocado", "beet", "cherry"]
groups = {}
for w in words:
    if not groups.contains(w[0]):
        groups[w[0]] = []
    groups[w[0]].append(w)
for letter, group in groups.items():
    print(letter, group)

// a set of ints
seen = {}
for n in [3, 1, 3, 2, 1]:
    seen[n] = true
print(seen.len(), seen.contains(2), seen.contains(7))
```

### Classes

| Python | stone |
| --- | --- |
| `@dataclass class P:` with typed fields | `struct P:` with the same fields |
| `P(1, 2)` | `P(x=1, y=2)`: every field, named |
| `__init__` with logic or defaults | an associated function in `impl P:`, such as `fn new(...) -> P:` |
| a method that mutates `self` | the same method; stone changes the receiver in place |
| `__eq__` | `impl eq for P:` with `fn eq(self, other: Self) -> bool` |
| `__lt__`, `functools.total_ordering` | `impl ord for P:` with `fn cmp(self, other: Self) -> ordering` |
| `__repr__` for printing | usually unnecessary: a struct prints as `P(x=1, y=2)`, like a dataclass |
| subclasses that are alternatives (`Circle(Shape)`, `Square(Shape)`) | an `enum` with a variant per subclass and a `match` |
| an abstract base class or a protocol | a `trait`, implemented by each struct, used through `[T: Trait]` |
| class attributes, `@staticmethod` | a top-level value or function, or an associated function |
| `isinstance(x, Circle)` | `match` on an enum |

Choose an enum when the code switches on the kind of value, and a trait when it calls the same
method on any of them. A trait cannot hold a mix of types in one list; if the Python list holds
circles and squares together, use an enum:

```stone
enum Shape:
    Circle(float)
    Square(float)

fn area(s: Shape) -> float:
    match s:
        case Shape.Circle(r):
            ret 3.14159 * r * r
        case Shape.Square(side):
            ret side * side

total = 0.0
for s in [Shape.Circle(1.0), Shape.Square(2.0)]:
    total += area(s)
print(total)
```

### Functions and errors

| Python | stone |
| --- | --- |
| `def f(x, n=10):` | `fn f(x: T, n: int)`, and every call passes `n` |
| `f(*args)`, `**kwargs` | a `list` parameter, or a struct of options |
| `lambda`, a function argument | a trait with one method, and a generic function bounded by it |
| `return a, b` | `ret (a, b)`, with `-> tuple[A, B]` |
| returning `None` on failure | `-> option[T]`, returning `none` or `some(x)` |
| `raise` | return a `result[T, str]` with `err("message")` and let the caller decide; `os.exit(1)` after printing a message for a fatal error |
| `try: int(s) except ValueError:` | `match s.parse_int():` with `case ok(n):` and `case err(message):`; `int(s)` on bad text stops the program, and nothing catches it |
| an exception passing through a function | `?`: `n = s.parse_int()?` returns the `err` from a function that returns a `result` with the same error type |
| a function that mutates a list argument | return the list: `xs = f(xs)`, which is still in place |
| a function that mutates a global | take it as a parameter and return it |
| recursion deeper than 1000 | a loop with an explicit stack (a `list`) |
| `total = 0` and later `total = 0.0` | a second name: each variable keeps one type |
| `x * (i + 1)` with a float `x` | `x * float(i + 1)`: convert every int that meets a float |

```stone
// int(s) stops the program on bad text, but s.parse_int() returns a result, which ? passes on
fn total(texts: list[str]) -> result[int, str]:
    sum = 0
    for s in texts:
        sum += s.parse_int()?
    ret ok(sum)

for texts in [["42", " -7 "], ["1", "x1"], ["99999999999999999999"]]:
    match total(texts):
        case ok(n):
            print("total", n)
        case err(message):
            print("error:", message)
```

The error is the message `int(s)` would stop with, such as
`invalid literal for int() with base 10: 'x1'`.

### Strings and I/O

| Python | stone |
| --- | --- |
| `f"{name}: {score:.2f}"` | `name + ": " + str(score)` (no format specifiers; round by hand if needed) |
| `print(a, b, sep=", ")`, `end=""` | build one string, then `print` it |
| `s.lower()`, `s.replace()`, `s.isdigit()` | loops over characters, comparing with `<` and `>` |
| `s * 3` | a loop of `+=` |
| `ord(c)`, `chr(n)` | not available; compare characters as strings (`c >= "a" and c <= "z"`) |
| `open(path).read()`, `sys.stdin` | `while not eof(): line = input()` and redirect the file to standard input |
| `sys.argv[1:]` | `args()` (without the program's name) |
| `sys.exit(n)` | `os.exit(n)` |
| `os.environ.get("X", "d")` | `os.env("X").unwrap_or("d")`, after `use os` |
| `random.randint`, `time.time()` | `random.randint`, `time.now()`, after `use random` and `use time` |

## Traps that change output without an error

These compile and run but print something different from Python. Check each one the program
could hit.

- **Integer division and remainder.** Python's `//` rounds down and its `%` takes the
  divisor's sign; stone's `/` truncates toward zero and `%` takes the dividend's sign. They agree
  when both operands are non-negative. When either can be negative, use helpers:

  ```stone
  // Python's a // b and a % b
  fn floor_div(a: int, b: int) -> int:
      q = a / b
      if a % b != 0 and (a < 0) != (b < 0):
          q -= 1
      ret q

  fn py_mod(a: int, b: int) -> int:
      r = a % b
      if r != 0 and (r < 0) != (b < 0):
          r += b
      ret r

  print(floor_div(-7, 2), py_mod(-7, 2), -7 / 2, -7 % 2)
  ```

  This prints `-4 1 -3 -1`.
- **`/` on ints.** Python's `7 / 2` is `3.5`; stone's is `3`. Write `float(a) / float(b)`.
- **Overflow.** Python ints never overflow; stone's wrap at 64 bits. Code that computes large
  factorials, products, or hashes needs a modulus at each step, as the Python likely intended.
- **Printed values.** Floats, ints, strings, lists, tuples, dicts, and dataclasses print as
  Python prints them, but `True`/`False` print as `true`/`false`, `None` as `none`, and an
  optional value as `some(3)` rather than `3`. Unwrap options before printing, and turn a bool
  into `"True"` yourself if the output must match exactly.
- **Shared mutation.** In Python, `b = a` then `b.append(1)` changes `a`, and a function that
  appends to its argument changes the caller's list. In stone both change only the copy. Find
  every place the Python relies on two names seeing one change, and pass the value along and
  return it instead. The checker warns about a function whose change to a parameter is lost; it
  cannot see aliasing between two variables in the same function.
- **Non-ASCII text.** `s.len()` counts bytes, not characters, so `"café".len()` is 5 where
  Python's `len` gives 4, and `s[i]` on a byte inside a character stops the program.
  `for c in s` walks characters, as in Python.
- **List operators.** `xs + ys` adds element by element (and stops if the lengths differ), and
  `xs * 2` doubles each element. The checker catches some of these; read each one carefully.
- **`find`.** It returns `some(i)` or `none`, never -1: `s.find(t).unwrap_or(-1)`.
- **Sorting.** `xs.sort()` sorts ints, floats, strings, and structs that implement `ord`, and is
  stable for structs, like Python's sort. Floats sort `nan` last.

## Worked example

This Python program uses a dataclass, a function that mutates its argument, a generator
expression, a comprehension, f-strings, `min` with a key, a dict, and `sorted` with a key:

```python
from dataclasses import dataclass


@dataclass
class Item:
    name: str
    qty: int
    price: float


def restock(items, name, amount):
    for item in items:
        if item.name == name:
            item.qty += amount
            return True
    return False


def total_value(items):
    return sum(i.qty * i.price for i in items)


def word_counts(text):
    counts = {}
    for word in text.split():
        counts[word] = counts.get(word, 0) + 1
    return counts


items = [Item("apple", 3, 0.5), Item("pear", 0, 0.75), Item("plum", 12, 0.25)]
restock(items, "pear", 4)
if not restock(items, "kiwi", 1):
    print("no kiwi")
in_stock = [i.name for i in items if i.qty > 0]
print(f"in stock: {', '.join(in_stock)}")
print(f"total value: {total_value(items)}")
cheapest = min(items, key=lambda i: i.price)
print("cheapest:", cheapest)
counts = word_counts("the cat and the hat and the bat")
for word, n in sorted(counts.items(), key=lambda kv: kv[1], reverse=True):
    print(word, n)
```

In stone, printing exactly what the Python prints:

```stone
struct Item:
    name: str
    qty: int
    price: float

// a word and how often it appears, ordered by count, highest first
struct WordCount:
    word: str
    n: int

impl ord for WordCount:
    fn cmp(self, other: Self) -> ordering:
        ret other.n.cmp(self.n)

// returns the changed list, and whether the item was found
fn restock(items: list[Item], name: str, amount: int) -> tuple[list[Item], bool]:
    for i in range(items.len()):
        if items[i].name == name:
            items[i].qty += amount
            ret (items, true)
    ret (items, false)

fn total_value(items: list[Item]) -> float:
    total = 0.0
    for item in items:
        total += float(item.qty) * item.price
    ret total

// counts each word, keeping the words in the order they first appear
fn word_counts(text: str) -> map[str, int]:
    counts = {}
    for word in text.split():
        if counts.contains(word):
            counts[word] += 1
        else:
            counts[word] = 1
    ret counts

items = [Item(name="apple", qty=3, price=0.5), Item(name="pear", qty=0, price=0.75), Item(name="plum", qty=12, price=0.25)]
items, found = restock(items, "pear", 4)
items, found = restock(items, "kiwi", 1)
if not found:
    print("no kiwi")
in_stock = []
for item in items:
    if item.qty > 0:
        in_stock.append(item.name)
print("in stock: " + ", ".join(in_stock))
print("total value: " + str(total_value(items)))
cheapest = items[0]
for item in items:
    if item.price < cheapest.price:
        cheapest = item
print("cheapest:", cheapest)
ranked = []
for word, n in word_counts("the cat and the hat and the bat").items():
    ranked.append(WordCount(word=word, n=n))
ranked.sort()
for c in ranked:
    print(c.word, c.n)
```

The decisions:

- `restock` mutated an item through the list it was given and returned whether it found one. In
  stone it changes `items[i].qty` and returns both the list and the flag as a tuple, and the
  caller assigns both. Changing `item` from `for item in items` would change only a copy, so it
  loops over indexes.
- `qty * price` mixed an int and a float; stone needs `float(item.qty)`.
- The generator expression, the comprehension, and `min(key=...)` became loops, and the f-strings
  became `+` with `str`.
- `counts.get(word, 0) + 1` became a `contains` check, since `counts[word] += 1` needs the key.
- `sorted(..., key=...)` became a list of structs built from `items()`. `WordCount`'s `cmp`
  orders by count, highest first, and stone's sort of structs is stable like Python's, so ties
  keep their first-seen order.
- `print("cheapest:", cheapest)` needed no change: a struct prints as a dataclass does.

## Report template

When you finish, report:

```text
Converted: <files>, <functions converted> of <functions total>
Checked:   stone check clean; outputs diffed against python3 on <inputs>, under stone run and stone build
Behavior changes:
  - <e.g. bools print as true/false; negative // replaced by floor_div>
Not converted:
  - <feature>: <why, and what the user could do>
```

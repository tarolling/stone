# Input

So far every program has worked on values written into its source. To work on data from
outside, a program reads standard input with `input` and `eof`, and its command-line arguments
with `args`.

## Reading lines

`input()` returns the next line of standard input without its newline. `input("prompt ")`
writes the prompt first, without a newline, for programs someone runs by hand:

```stone
name = input("what is your name? ")
print("hello, " + name)
```

Once the input runs out, `input()` returns `""`. An empty line also gives `""`, so to read
every line, loop until `eof()` says nothing is left:

```{literalinclude} ../examples/input.st
:language: stone
```

Given this input,

```{literalinclude} ../examples/input.in
:language: text
```

it prints

```{literalinclude} ../examples/input.out
:language: text
```

Run it with the input redirected from a file, or piped from another program:

```sh
stone run scores.st < scores.txt
cat scores.txt | stone run scores.st
```

## Turning text into values

Everything read is a `str`, so a few builtins turn text into other values and back:

- `line.strip()` removes spaces, tabs, and line breaks from both ends.
- `line.split()` breaks a line into words at runs of whitespace, and `line.split(",")` breaks it
  at each comma, keeping empty fields.
- `int("42")` and `float("2.5")` read numbers, allowing whitespace around them. Text that is not
  a number, such as `int("4x")`, stops the program with an error naming the text, like
  `invalid literal for int() with base 10: '4x'`.
- `str(value)` goes the other way, giving the text `print` would write, so `"n = " + str(3)` is
  `"n = 3"`.

## Command-line arguments

`args()` returns the arguments given after the program as a `list[str]`, without the program's
own name:

```stone
total = 0
for a in args();
    total = total + int(a)
print(total)
```

```sh
stone run sum.st 1 2 3                       # prints 6
stone build sum.st -o sum && ./sum 1 2 3     # prints 6 too
```

Everything after the file belongs to the program, even arguments starting with `-`, so
`stone run sum.st -5 --x` gives it `["-5", "--x"]`.

Next, {doc}`modules`.

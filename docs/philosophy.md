# Philosophy

stone aims to be as simple to write as Python, as fast and memory-safe as Rust, and to report
errors as clearly as Rust does, while compiling to programs that need nothing from the system
they run on, not even a C library. These goals can conflict, and stone resolves the conflict
the same way every time: **the compiler takes on the complexity so the programmer does not
have to.**

This page describes those goals, how they guide decisions, and how far stone has come toward
each one. Parts of it describe where stone is going rather than where it is today, and each
section says which is which.

## The compiler does the work

If the compiler can figure something out, stone does not make the programmer write it.
Annotations, keywords, and options that exist only to help the compiler, such as types,
lifetimes, `mut`, `&`, `<T>`, `lazy`, or `inline`, are left out of the language. The language
is defined so that the compiler can always infer them instead.

Python 3.15's `lazy import numpy` is a good example. Python needs the keyword because
importing a module runs its top-level code, which can be slow and can have side effects, so
the interpreter cannot delay it without being told to. stone avoids the problem by design: a
library module holds only `use` and `def`, so importing it runs nothing, and the compiler
links in only what the program uses. Every import is effectively lazy, and there is no keyword
for it.

The pattern applies to the whole language. When a feature would need a keyword to be fast or
safe, the better fix is to change the semantics so that the compiler can always choose the
fast, safe option on its own and the programmer never sees the difference.

What stone already infers:

- **Types.** The checker infers the type of every variable, parameter, and expression, so
  there are no annotations.
- **Memory management.** Compiled programs count references, with no garbage collector. The
  compiler places every retain and release, decides which parameters are only borrowed, and
  skips counting where it can prove a count would not matter.
- **What to include.** Imports run nothing, and runtime routines are compiled into a program
  only if it calls them.

What it should infer next:

- **Generic functions.** A function is currently monomorphic: `def first(xs); ret xs[0]`
  cannot be called on both a `list[int]` and a `list[str]`. The compiler should infer generic
  functions and specialize them for each use, the way Rust compiles generics, without the
  programmer writing any type parameters.
- **Where values live and when they are copied.** Values that never escape a function should
  live on its stack, and an update to a value nothing else refers to should happen in place.
  See [value semantics](#value-semantics).
- **Dead code.** Functions that are never called should be left out, not only unused runtime
  routines.

## Simple like Python

The code a programmer reads and writes should look like Python's and need as few concepts.
stone keeps Python's structure (indentation, `def`, `for x in xs`, `if`, `while`) and its
habit of having one obvious way to do something. It does not take on Python's dynamic
features, such as `eval`, monkeypatching, or values whose type changes at run time, because a
compiler cannot make those fast or safe. stone gets static types through inference instead of
asking for annotations.

A feature belongs in stone only if a newcomer can understand it without knowing what the
compiler does with it. If a feature makes sense only to someone who knows how it is compiled,
the compiler should handle it instead.

stone's syntax differs from Python's in a few small ways (blocks open with `;`, functions
return with `ret`, and loops skip ahead with `cont`), listed in [syntax](reference/syntax.md).

## Memory safety and value semantics

A program the checker accepts can never read freed memory, read outside a list, read a
variable before it is assigned, or follow a null reference. Anything that cannot be ruled out
before the program runs, such as an index out of range or division by zero, is checked while
it runs and stops the program with a clear error, never undefined behavior.

stone already guarantees this. Every list index is checked, there is no null (`none` is a
value of its own type), reading an unassigned variable is a checker error or a runtime error,
and reference counting frees every string and list exactly once. The checker rules out
cyclic values, so reference counting never leaks.

(value-semantics)=

### Value semantics

stone has value semantics: assigning a value, passing it to a function, or putting it in a list
behaves as if it were copied, so changing one variable never changes another.

```stone
a = [1, 2]
b = a
b.append(3)
print(a)  // [1, 2], where Python would print [1, 2, 3]
```

This is the one place stone deliberately differs from Python. Sharing mutable values is the
main source of aliasing bugs, and it is what makes memory safety hard: it allows cycles once
there are user-defined types, and it forces a language either to use a garbage collector or
to make the programmer prove ownership, as Rust's borrow checker does. Value semantics avoids
both. Programmers never have to think about who else holds a value, and the compiler is free
to optimize.

The copies are only conceptual. The compiler shares the underlying memory and copies it only
when a value that something else also refers to is about to change (copy on write), and it
uses the reference count it already keeps to update a value in place when nothing else refers
to it. In the common case, a value with one owner, nothing is ever copied.

A few rules follow from this, and the checker enforces each with an error that says what to
do instead:

- A function changes a value that belongs to its caller by returning it: `xs = add(xs, 1)`.
  A function can read globals but never change them, just as it cannot assign them. The checker
  warns when a function changes a parameter and then never uses it, since the caller would not
  see the change.
- A change is a statement of its own (`xs.append(1)`, `grid[i][j] = 0`) made to a variable or
  an element of one. Changing a temporary, as in `f().append(1)`, would have no effect.
- A `for` loop walks the list as it was when the loop started, so appending to the list inside
  the loop does not make the loop run longer.

Status: lists have value semantics in both backends. Compiled code checks a list's reference
count before changing it and copies the list only when something else refers to it
(`list_unique` in the IR), so changing a list that only one variable holds is still done in
place. A function owns each parameter it assigns or changes, and the caller hands it a reference
for it. When nothing reads the argument after the call, as in `xs = f(xs)` or `ret f(xs)`, the
caller moves its own reference in instead of keeping one, so the function changes the list in
place. More generally, both backends find each variable's last use, and a value moves there
instead of being copied: into a call (`ys = f(xs)`), another variable (`ys = xs`), or a list
(`rows.append(row)`) when nothing reads it afterward. A global moves only into a function that
never reads it, directly or through the functions it calls, and never in an interactive session,
where a later entry may read it.

## Fast like Rust

A compiled stone program should run about as fast as the equivalent C or Rust. The
[benchmarks](development/benchmarks.md) compare `stone build` with `gcc -O2`, `gcc -O0`, and
Python.

Speed comes from what the language guarantees, not from hints the programmer adds. Static
types mean values are unboxed and calls are direct. Value semantics means no aliasing, so the
compiler can keep values in registers and reorder freely. Runtime checks such as list bounds
stay in the language, and the compiler's job is to prove them unnecessary and remove them, not
to let the programmer turn them off.

Status: the compiler lowers to an IR, allocates registers with linear scan, and specializes a
few cases such as division by a constant. Optimization passes (inlining, constant
propagation, removing checks and reference counts it can prove redundant) are future work.

## Errors like Rust

An error should say what went wrong, where, why, and how to fix it. The goal is diagnostics as
good as rustc's:

- the main location with its source line, plus labels on every other location that explains
  the error ("first assigned here", "expected `int` because of this"),
- `note:` lines that explain the rule involved and `help:` lines that suggest a fix, as code
  where possible,
- an error code for each kind of error, with a longer explanation available from the CLI,
- as many errors as possible from one run, without a cascade of follow-on errors,
- runtime errors that give the location and the chain of calls that led there, from compiled
  programs as well as from the interpreter.

Status: every error from the lexer, parser, linker, and checker prints as
`file:line:col: error: message` with the source line and a caret, and the language server
shows the same errors in the editor. Runtime errors print the message only. Labels, notes,
suggestions, error codes, and runtime locations are future work.

## Run anywhere: no libc

A program built with `stone build` should depend on nothing but the operating system kernel:
no C library, no dynamic linker, no runtime to install. One file should run on any machine
with the right processor and kernel, whatever distribution or libc version it has.

On Linux this is fully achievable because the kernel's system call interface is stable. The
runtime makes system calls directly, allocates memory with its own allocator on top of `mmap`,
and formats and parses numbers itself, which also lets it guarantee that compiled programs
print floats exactly like the interpreter does.

macOS and Windows do not have a stable system call interface. Apple supports only programs
that call the kernel through `libSystem`, and Windows only through `kernel32.dll`. On those
systems the goal is the thinnest possible layer: call only the system's own interface, never a
C library's higher-level functions.

The same idea applies to the toolchain. Eventually `stone build` should write executables
itself, with no assembler, linker, or C compiler installed, so that installing stone is all it
takes to build stone programs.

Status: compiled programs currently link against glibc and call 26 of its functions, plus its
`stdin`:

- memory: `malloc`, `realloc`, `free`
- output, input, and exiting: `dprintf`, `getline`, `getc`, `ungetc`, `exit`
- numbers: `snprintf`, `strtod`, `strtoll`, `atoi`, `fmod`, `pow`, `__errno_location`
- strings: `strcpy`, `strcat`, `strchr`, `strstr`, `strncasecmp`
- the `os` module: `getenv`, `gethostname`, `getcwd`, `getpid`, `sysconf`, `clock_gettime`

`stone build` also runs gcc to assemble and link. Both are planned to go away.

## Two backends, one behavior

`stone run` interprets a program and `stone build` compiles it, and both must behave exactly
the same on everything the checker accepts: the same output, down to float formatting, and the
same runtime errors with the same messages. The checker rejects anything the two cannot run
identically. Every program in the test suite runs through both, so the interpreter serves as
the specification the compiler is tested against.

## Making decisions

When choosing between designs, stone puts these first, in this order:

1. **Safety.** No accepted program has undefined behavior.
2. **Simplicity for the programmer.** Fewer concepts and less to write, even if the compiler
   has to do much more work.
3. **Clear errors.** A design whose failures are hard to explain is worse than one whose
   failures are easy to explain.
4. **Speed.** As fast as possible within the first three.
5. **Compiler simplicity.** This comes last. A complicated compiler is an acceptable price
   for a simpler language.

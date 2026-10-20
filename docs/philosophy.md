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
Annotations, keywords, and options that exist only to help the compiler, such as lifetimes,
`mut`, `&`, `lazy`, or `inline`, are left out of the language. The language is defined so that
the compiler can always infer them instead.

Python 3.15's `lazy import numpy` is a good example. Python needs the keyword because
importing a module runs its top-level code, which can be slow and can have side effects, so
the interpreter cannot delay it without being told to. stone avoids the problem by design: a
library module holds only `use` and `fn`, so importing it runs nothing, and the compiler
links in only what the program uses. Every import is effectively lazy, and there is no keyword
for it.

The pattern applies to the whole language. When a feature would need a keyword to be fast or
safe, the better fix is to change the semantics so that the compiler can always choose the
fast, safe option on its own and the programmer never sees the difference.

The one exception to this rule is a function's signature: the type of each parameter and of the
value it returns, as in `fn mean(xs: list[float]) -> float:`. Signatures are written for the
reader and for error messages, not for the compiler. A signature is the function's contract,
readable without its body, and it lets the checker report a mistake where it is made: a wrong
argument at the call, and a wrong value inside the function, rather than wherever two
inferences happen to disagree. Everything else is still inferred, as in Rust, so no variable is
ever annotated.

What stone already infers:

- **Types.** The checker infers the type of every variable and expression from the values
  assigned to it and the signatures of the functions it calls, so the only types written are
  the parameter and return types of functions.
- **Specialized generic functions.** A generic function writes its type parameters in brackets,
  as in `fn first[T](xs: list[T]) -> T:`, and every call infers its own types for them, so no
  type is ever written at a call. The compiler makes a copy of the function for each set of
  types it is called with, the way Rust compiles generics, so a generic function is as fast as
  one written for a single type. Structs and enums can be generic the same way, as in
  `struct Pair[A, B]:`. Even `option[T]` and `result[T, E]` are, written in stone in the prelude
  that every program is linked with, so they follow the same rules as the programmer's own enums.
- **Which method a bound calls.** A trait lists methods that types implement, and a bound such
  as `fn total[T: Shape](xs: list[T])` lets a generic function call them. Each copy of the
  function calls the method of its own type directly, so there is no `dyn` to write and no
  lookup while the program runs. Whether a trait's method changes `self` is decided by the
  trait, from its default body, so no `mut` marks it either. Even `==` and `<` are the prelude's
  traits `eq` and `ord`, which a struct can implement to compare its own way.
- **Memory management.** Compiled programs count references, with no garbage collector. The
  compiler places every retain and release, decides which parameters are only borrowed, and
  skips counting where it can prove a count would not matter.
- **What to include.** Imports run nothing, and runtime routines are compiled into a program
  only if it calls them.

What it should infer next:

- **Where values live and when they are copied.** Values that never escape a function should
  live on its stack, and an update to a value nothing else refers to should happen in place.
  See [value semantics](#value-semantics).
- **Dead code.** Functions that are never called should be left out, not only unused runtime
  routines.

## Simple like Python

The code a programmer reads and writes should look like Python's and need as few concepts.
stone keeps Python's structure (indentation, `for x in xs`, `if`, `while`) and its
habit of having one obvious way to do something. It does not take on Python's dynamic
features, such as `eval`, monkeypatching, or values whose type changes at run time, because a
compiler cannot make those fast or safe. stone gets static types by requiring the annotations
Python makes optional on a function's parameters and return value, and inferring the rest.

A feature belongs in stone only if a newcomer can understand it without knowing what the
compiler does with it. If a feature makes sense only to someone who knows how it is compiled,
the compiler should handle it instead.

stone's syntax differs from Python's in a few small ways (functions return with `ret`, and loops
skip ahead with `cont`), listed in [syntax](reference/syntax.md).

## Memory safety and value semantics

A program the checker accepts can never read freed memory, read outside a list, read a
variable before it is assigned, or follow a null reference. Anything that cannot be ruled out
before the program runs, such as an index out of range or division by zero, is checked while
it runs and stops the program with a clear error, never undefined behavior.

stone already guarantees this. Every list index is checked, there is no null (a value that may
be missing is an `option[T]`, which `match` or `unwrap` must open, and `none` is one of its
values), an error a program expects is a `result[T, E]` that the caller must open or pass on
with `?`, never an exception that unwinds through code that did not ask for it, reading an
unassigned variable is a checker error or a runtime error, and reference
counting frees every string, list, and enum value exactly once. The checker rules out cyclic
values, and enum values never change, so reference counting never leaks.

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
  see the change. The one exception is a method that changes `self`, which changes the value it
  is called on, as `append` changes a list: `p.shift(1.0)` means `p = Point.shift(p, 1.0)`, so
  it is still a change made by returning, written the way Python programmers expect. The
  compiler sees which methods change `self`, so no `mut` keyword marks them.
- A change is a statement of its own (`xs.append(1)`, `grid[i][j] = 0`) made to a variable or
  an element of one. Changing a temporary, as in `f().append(1)`, would have no effect.
- A `for` loop walks the list as it was when the loop started, so appending to the list inside
  the loop does not make the loop run longer.

Status: lists, and maps, which compiled code stores as lists of entries with a hash table
beside them, have value semantics in both backends, and a change reaches into either through
any place, such as `p.counts[k] += 1`. Compiled code checks a list's reference
count before changing it and copies the list only when something else refers to it
(`list_unique` in the IR), so changing a list that only one variable holds is still done in
place. A function owns each parameter it assigns or changes, and the caller hands it a reference
for it. When nothing reads the argument after the call, as in `xs = f(xs)` or `ret f(xs)`, the
caller moves its own reference in instead of keeping one, so the function changes the list in
place. More generally, both backends find each variable's last use, and a value moves there
instead of being copied: into a call (`ys = f(xs)`), another variable (`ys = xs`), or a list
(`rows.append(row)`) when nothing reads it afterward. A global moves only into a function that
never reads it, directly or through the functions it calls, and never in an interactive session,
where a later entry may read it. Arithmetic on lists, such as `xs = xs * 2.0 + ys` or
`xs += ys`, writes its result over an operand's list when that operand is at its last use or is
the variable being assigned and nothing else refers to its list, so it allocates nothing.
Tuples never change, so they need nothing of this: the compiler keeps each of a tuple's values as
a variable of its own, counting only the strings and lists among them. Structs are kept the same
way, so changing a field of a struct in a variable (`p.x = 1.0`) is an assignment to that
field's variable, and a field holding a list follows the rules for lists, including moving into
a call that replaces it (`p.items = f(p.items)`). A method that changes `self` takes the
receiver's values moved in, as from a variable at its last use, and returns them after its result
for the caller to store back where they came from, a variable, an element, or a field, so
`stack.push(x)` changes the stack in place; a method that only reads `self` borrows it. Enum
values never change
either, so they are shared freely: each is stored once, with a count of the variables that hold
it, and a `match` hands out its values with a reference each.

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
few cases such as division and remainder by a constant. Arithmetic on whole lists, such as `a * 2.0 + b`,
compiles to one loop with no intermediate lists and no index checks, which handles two elements
at a time with SSE2 or NEON where the operations allow, so on the `vector` benchmark it is
about 2.7 times as fast as numpy, which builds a temporary array for each operator. A tuple is
never allocated: its values live in registers like any other variables, a function takes and
returns them as separate values, and a list of tuples stores each element's values side by side,
like a Rust `Vec<(i64, String)>`, which is a first step toward choosing where every value lives.
Structs are compiled the same way, so a `list[Point]` is a flat array of coordinates, though each
change to a field of one still checks the list's bounds and that it is not shared, which keeps
the `nbody` benchmark about 5 times slower than C.
A generic function, or a method of a generic struct, is compiled once for each set of types it is
used with, so its copies are as fast as functions written for each type, and a generic struct is
flattened like any other. A call through a trait's bound is a direct call in each copy, and a
trait's default method is compiled once for each type that keeps it. A trait is not yet a type of
values, as `dyn Shape` is in Rust, so a list holds values of one type; a list of shapes of
several types would need its values boxed with a table of their methods.
Enum values, including options, are stored on the heap for now, which lets an enum hold itself,
as a tree or linked list does, without the `Box` Rust asks for; keeping the values of an enum
that does not hold itself in registers, as tuples are, is future work.
Wider vectors
such as AVX2 on targets that have them, and optimization passes (inlining, constant
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
shows the same errors in the editor. Runtime errors print the same way, pointing at the
expression that failed, from compiled programs as well as from the interpreter: the compiler
renders each place an error can happen into the executable, so it costs nothing until the error
happens. Labels, notes, suggestions, error codes, and the chain of calls behind a runtime error
are future work.

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

The same idea applies to the toolchain. `stone build` should write executables itself, with no
assembler, linker, or C compiler installed, so that installing stone is all it takes to build
stone programs.

Status: on Linux, compiled programs need no C library. They start at their own `_start`, make
system calls directly, allocate memory with their own allocator on top of `mmap`, read input
through their own buffer, and format and parse floats themselves, digit for digit like the
interpreter, so each one is a static executable. On arm64 macOS, the same runtime calls only
`libSystem`'s system call wrappers (`read`, `write`, `mmap`, `sysctl`, and a few more), each
through a shim that keeps the contract of a Linux system call, so the rest of the runtime is
shared. `stone build` writes either executable itself, with its own assembler and linkers,
including the Mach-O file's imports and its code signature, so it needs no assembler, linker,
or C compiler installed, and builds for every target from any machine. Windows is not supported
yet.

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

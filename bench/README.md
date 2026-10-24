# stone benchmarks

This crate times stone's compiled output (`stone build`) against the same programs written in C and Python. The interpreter is not benchmarked.

```sh
cargo run --release --manifest-path bench/Cargo.toml -- [--runs 5] [--filter NAME] [--stone PATH] [--numpy PYTHON] [--min]
```

For each benchmark, the runner builds stone in release mode, compiles the `.stn` with `stone build` and the `.c` with `gcc -O2 -fwrapv -ffp-contract=off` and `gcc -O0 -fwrapv -ffp-contract=off` (linking `-lm`), and runs the `.py` with `python3`. Every version runs with the arguments in the benchmark's `.args` file, one per line, as the golden tests' `.args` files are. The runner runs each implementation once untimed and requires its stdout to match the `.out` file exactly. It then times `--runs` more runs, checking the output of each, and prints a Markdown table of the median wall times, or the fastest with `--min`. Progress and warnings go to stderr, so `> results.md` captures only the table. It needs `gcc` and `python3` on `PATH`.

A benchmark of arithmetic on lists can also have a numpy version, `name.np.py`, which runs under the Python that `--numpy` names (`python3` by default), such as a virtualenv's `bin/python`, and adds `numpy (ms)` and `numpy / stone` columns. If that Python cannot import numpy, the runner warns and leaves numpy out.

`cargo test --manifest-path bench/Cargo.toml` checks the statistics, table, and `.args` code, and that every benchmark still passes stone's checker, without running any workloads.

## Reading the table

- `stone / C -O2` is how many times slower compiled stone is than optimized C. Lower is better for stone.
- `Python / stone` is how many times faster compiled stone is than CPython. Higher is better for stone.
- `C -O0` is a useful anchor. Like stone's backend, gcc at `-O0` does no optimization across statements, though it keeps every variable in memory where stone allocates registers.
- The geometric mean row summarizes the ratios, treating 2x faster and 2x slower symmetrically.
- Times include process startup, which is about 1 ms for the native programs and tens of milliseconds for Python.

## Benchmarks

| name | what it stresses | workload |
| --- | --- | --- |
| `fib` | calls and returns | recursive `fib(35)` |
| `lcg` | integer arithmetic, division, and `%` | 20 million steps of a linear congruential generator |
| `globals` | reading globals from a function | `lcg`, but with its constants in globals |
| `collatz` | branches and division | the longest Collatz chain for starts below 300,000 |
| `sieve` | list append, indexed load and store | counting primes up to 5,000,000 |
| `matmul` | nested list indexing | multiplying two 300x300 `list[list[int]]` |
| `matmul_float` | float arithmetic and nested list indexing | multiplying two 300x300 `list[list[float]]` |
| `vector` | arithmetic on lists, element by element | `y = 0.5 * x + y` over a million floats, 50 times |
| `strings` | `str`, `join`, `split`, `startswith`, `endswith`, `find`, and string indexing | joining 200,000 numbers into a sentence and splitting it back into words, 10 times |
| `wordcount` | a `map[str, int]`: `get`, `contains`, reads, and sets | counting 2 million words drawn from 50,000 |
| `nbody` | a `list` of structs, changes to their fields, and `impl` methods | a million steps of the Sun and the four gas giants orbiting |
| `binary_trees` | an enum that holds itself, `match`, and freeing | building and walking binary trees of depth 4 to 15, as in the Benchmarks Game |
| `sort` | `sort` on lists of ints and floats, against C's `qsort` and Python's `list.sort` | sorting 2 million 62-bit ints, then 2 million floats |

Each workload size is in the benchmark's `.args` file, so every version reads it at runtime and gcc cannot specialize on a constant size. Sizes are chosen so `gcc -O2` takes at least about 40 ms and the whole suite finishes in a few minutes. The runner warns when any time is under 10 ms, which usually means gcc optimized the work away.

The versions are meant to be written naturally in each language, not to mimic each other instruction by instruction, so the C versions format numbers into a buffer instead of allocating a string for each, use a hand-written hash table in `wordcount`, and free trees with an explicit walk in `binary_trees`. Every program keeps integers nonnegative and within 64 bits, so stone's `/` and `%`, which truncate like C's, give the same output as Python's `//` and `%`, which round down. Programs with floats print them scaled to an int, so C need not print floats the way stone does, and add them in the same order in all three versions, so they round identically. `-ffp-contract=off` keeps gcc from fusing a multiply and an add, which stone never does, on targets with FMA.

To add a benchmark, write `name.stn`, `name.c`, and `name.py`, put the hot loop inside a function, read the workload size from the arguments, and save the arguments as `name.args` and the shared output as `name.out`.

## Baseline

Median of 9 runs on 2026-10-23, on an Intel Core i7-10750H under WSL2, pinned to one core with `taskset -c 2`, with gcc 13.3 and Python 3.12 from the distribution, and numpy 2.5.4 in a virtualenv for `vector`. Absolute times on a machine like this can differ by 2x between sessions, and by a fifth between runs of the suite, as CPU boost and background load change, so compare the ratio columns, which come from the same session, rather than times from different sessions. The `sieve` row is from a second run of it alone, since the first was disturbed (C took 192 ms). For a single change, time the old and new stone binaries against each other with `--stone`, as the History notes do.

| program | C -O2 (ms) | C -O0 (ms) | stone (ms) | Python (ms) | numpy (ms) | stone / C -O2 | Python / stone | numpy / stone |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| binary_trees | 107.0 | 166.3 | 135.5 | 1011.0 | | 1.3x | 7.5x | |
| collatz | 62.8 | 115.4 | 100.7 | 2474.6 | | 1.6x | 24.6x | |
| fib | 19.5 | 65.2 | 78.8 | 1298.2 | | 4.0x | 16.5x | |
| globals | 235.0 | 260.3 | 55.6 | 4869.8 | | 0.2x | 87.5x | |
| lcg | 50.1 | 70.3 | 58.9 | 4899.2 | | 1.2x | 83.2x | |
| matmul | 18.1 | 70.5 | 84.7 | 2200.8 | | 4.7x | 26.0x | |
| matmul_float | 28.3 | 76.3 | 96.6 | 1437.9 | | 3.4x | 14.9x | |
| nbody | 67.2 | 296.7 | 296.5 | 4871.5 | | 4.4x | 16.4x | |
| sieve | 111.6 | 121.4 | 139.4 | 1015.4 | | 1.2x | 7.3x | |
| sort | 688.0 | 876.3 | 202.0 | 4580.4 | | 0.3x | 22.7x | |
| strings | 99.5 | 129.1 | 274.2 | 1342.6 | | 2.8x | 4.9x | |
| vector | 61.1 | 130.8 | 75.9 | 3139.4 | 312.5 | 1.2x | 41.4x | 4.1x |
| wordcount | 257.9 | 212.9 | 281.9 | 1106.9 | | 1.1x | 3.9x | |
| geometric mean | | | | | | 1.5x | 17.8x | 4.1x |

Compiled stone is about 1.5x slower than `gcc -O2` and about 18x faster than CPython, as a geometric mean. It is faster than `gcc -O2` on `globals` and `sort`, faster than `gcc -O0` on `binary_trees`, `collatz`, `globals`, `lcg`, `sort`, and `vector`, and level with it on `nbody`. On `vector` it was about 4x faster than numpy in this run, against 2.7x on 2026-10-16; nothing changed in its loop since, so that difference is between sessions. The Python ratio is low on `strings` and `wordcount` because they spend their time in work CPython does in C: splitting and joining strings, and hashing in `dict`.

- `sort` beats C because C's version calls a comparison function through `qsort` for every comparison, while stone sorts ints and floats by radix, a pass per byte in which they differ.
- `globals` beats C because its constants are globals that the program assigns one literal each, once, so stone reads them as literals and `% modulus` shifts like `% 2147483648`. C's globals could change, so gcc loads them and uses `idiv`. Assigning such a global anywhere else makes stone load it too.
- `sieve` is close to C because allocating and touching 40 MB of memory dominates all three native versions.
- `binary_trees` is close to C because both spend their time allocating and freeing nodes, and stone's allocator, which keeps a free list per block size, keeps up with glibc's `malloc` there. Every `Tree.Leaf` is one shared static value, so stone allocates exactly the nodes C does.

### History

| change | collatz | fib | globals | lcg | matmul | sieve | geometric mean |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| first baseline | 12.2x | 4.9x | 2.3x | 9.8x | 15.8x | 1.4x | 5.6x |
| constant divisors skip checks, powers of two shift | 4.4x | 5.7x | 1.5x | 3.2x | 15.1x | 1.7x | 3.8x |
| list index checks inline instead of calling `stone.list_slot` | 4.1x | 5.8x | 1.6x | 3.2x | 13.3x | 2.0x | 3.8x |
| IR, linear-scan register allocation, and compare-and-branch conditions | 1.8x | 3.7x | 1.1x | 1.4x | 4.4x | 1.0x | 1.9x |
| programs use `%`, which shifts for a power of two, and read their sizes from `.args` | 1.7x | 4.0x | 1.0x | 1.6x | 4.8x | 1.0x | 1.9x |
| constant globals, multiply-high division, check hoisting and reuse, inlining | 1.6x | 4.0x | 0.2x | 1.2x | 4.7x | 1.2x | 1.5x |

Each cell is stone's time over `gcc -O2`'s from the same session, and the geometric mean covers the six columns shown. Benchmarks a change does not touch still move by a few tenths between rows, which shows the noise. The last row changed the C versions too, since gcc can no longer specialize on a constant size. For a single change, timing the old and new stone binaries against each other in one session is more reliable: inlining index checks made `matmul` 1.25x faster that way, and left `sieve` unchanged, since memory traffic dominates it. Register allocation, timed the same way with `--stone`, made `collatz` 2.6x faster, `matmul` and `matmul_float` 2.5x, `lcg` 2.3x, `fib` 1.6x, `globals` 1.35x, and `sieve` 1.17x.

Rewriting `x - (x / m) * m` as `x % m` first made `lcg` and `collatz` about 3.5x slower, since `%` by a constant power of two still used `idiv` where `/` already shifted. Computing the remainder from the shifted quotient made `lcg` 3.7x faster than that and `collatz` 3.2x, timed the same way. Checking an index into a list of structs or tuples without dividing the list's slot count by the element's width made `nbody` 3.9x faster.

On 2026-10-23, timed against the previous binary in the same session: reading globals that hold one literal as literals made `globals` 4.7x faster (0.24 s to 0.05 s). Inlining `chain` into `longest` made `collatz` 1.3x faster. Checking each element of a loop once, moving the check of `bodies[i]` and the uniqueness check of `bodies` out of the inner loop, inlining `drift`, and reading fields at fixed offsets together made `nbody` 1.25x faster (360 ms to 296 ms, and 0.36 s to 0.28 s timed by hand). `fib`, `matmul`, `sieve`, `binary_trees`, `lcg`, `strings`, and `wordcount` did not change. The same change made popping a map's entries constant time on average: popping half of 400,000 keys took 0.06 s, where the previous binary needed 10 s for half of 40,000.

## Where the time goes

The generated assembly for each benchmark is left at `bench/target/programs/<name>-stone.s`, and `STONE_DEBUG=1` with a debug build prints each function's IR and register assignment. Values live in registers, and loop tests compare and jump directly, so the remaining gap to C comes mostly from:

- **Only five registers survive calls.** A value that lives across a call needs one of the callee-saved registers `rbx` and `r12` to `r15`, and the rest spill to the stack, the least-used first. `matmul`'s `multiply` keeps the matrices and inner counters in registers, but its outer loop counters live in memory, since `append` is called inside that loop. Each vreg also gets one location for its whole life, with no splitting around calls.
- **Floats live in general registers.** A float is kept as its bits in the same registers and stack slots as ints, and moves into an `xmm` register only for each operation, so `a - b` is `movq`, `movq`, `subsd`, and `movq` back, where gcc keeps floats in `xmm` registers and does it in one instruction. This is most of what is left between `nbody` and C, and much of `matmul_float`.
- **Calls do extra bookkeeping.** Every call increments `stone.call_depth`, compares it with 1,000, and decrements it on return, so recursion stays within the language's depth limit. This is most of what separates `fib` from C. Small functions that call no others are inlined, which keeps only the comparison: `collatz`'s `chain` and `nbody`'s `drift` are no longer calls.
- **Division by a variable uses `idiv`.** A literal divisor that is a power of two compiles to shifts, and any other literal to a multiplication by its magic number, for `/` and `%` alike, with no zero or overflow check. So does a global that holds one literal for the whole program. A divisor held in any other variable still gets both checks and an `idiv`, while C has no checks at all, since dividing by zero is undefined behavior there.
- **List access chases two pointers and checks bounds.** An index loads the list's header, adjusts a negative index, compares it with the length, then loads the data pointer and the element. For `list[list[int]]` that happens twice per `a[i][k]`, where C's flat array needs one load. Checking the same element again is skipped when nothing in between could change the list's length, and a check every turn of a loop repeats moves before the loop, but each access still reloads the data pointer.
- **Globals are checked on every read in a function.** A function checks a global's `g.<name>.set` flag before loading it, since globals always live in memory, unless the global holds one literal for the whole program, which reads as that literal.
- **Loops test at the top.** Each iteration jumps back to the test, then branches into the body, where gcc puts the test at the bottom so an iteration takes one branch.

### Structs, strings, maps, and enums

`nbody` is the largest gap. A `list[Body]` stores each body's seven floats side by side, as C's array of structs does. Within an iteration of the pair loop, `bodies[j]` is now checked once and `bodies[i]` not at all, since its check and the check that `bodies` is not shared both move before the loop, and `bodies[i].drift(dt)` is inlined, so its fields change in place. Each field is then two instructions to read or write, a load of the data pointer and the access at the field's offset, and each float operation four, as above. gcc keeps `bodies[i]`'s fields in `xmm` registers for the whole inner loop.

`strings` allocates a counted string for every `str()`, a list of them, the joined sentence, and then a string for every word `split` finds, where C formats each number straight into one buffer and walks it in place. Each of those strings is reference counted and freed when the round's list goes away.

`wordcount` allocates a string for each `str()` key and then hashes it twice, once for `get` and once to set the new count, as Python's `counts.get(word, 0) + 1` does. C formats each key into a stack buffer and finds its slot once.

### Arithmetic on lists

`vector` measures arithmetic on whole lists, which numpy is built for. An expression such as `0.5 * x + y` compiles to one loop with no list for any intermediate result, where numpy makes a temporary array for each operator. The loop runs over pairs of floats with SSE2 (NEON on arm64), then finishes any odd element one at a time, and writes over `y` in place, since `y` is assigned and nothing else holds its list. The baseline above shows the result.

The times include building the lists and summing them with ordinary loops, plus Python's startup and importing numpy, which is about 100 ms of numpy's time. Timing only the passes over the lists, stone took about 1.25x as long as `gcc -O2`, which also uses SSE2 here, and numpy about 2.5x as long as stone, or 1.9x with `out=` arguments that avoid its temporaries.

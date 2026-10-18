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

Median of 9 runs on 2026-10-16, on an Intel Core i7-10750H under WSL2, pinned to one core with `taskset -c 2`, with gcc 13.3 and Python 3.12 from the distribution, and numpy 2.5.4 in a virtualenv for `vector`. Absolute times on a machine like this can differ by 2x between sessions, and by a fifth between runs of the suite, as CPU boost and background load change, so compare the ratio columns, which come from the same session, rather than times from different sessions. For a single change, time the old and new stone binaries against each other with `--stone`, as the History notes do.

| program | C -O2 (ms) | C -O0 (ms) | stone (ms) | Python (ms) | numpy (ms) | stone / C -O2 | Python / stone | numpy / stone |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| binary_trees | 102.2 | 144.7 | 116.0 | 902.2 | | 1.1x | 7.8x | |
| collatz | 59.2 | 108.9 | 99.6 | 2217.7 | | 1.7x | 22.3x | |
| fib | 18.3 | 57.5 | 73.1 | 1136.7 | | 4.0x | 15.5x | |
| globals | 215.8 | 231.9 | 214.6 | 4411.4 | | 1.0x | 20.6x | |
| lcg | 45.3 | 60.3 | 71.5 | 4477.6 | | 1.6x | 62.6x | |
| matmul | 17.1 | 66.9 | 81.8 | 2036.1 | | 4.8x | 24.9x | |
| matmul_float | 25.0 | 69.2 | 91.7 | 1390.3 | | 3.7x | 15.2x | |
| nbody | 59.0 | 271.3 | 323.4 | 4447.9 | | 5.5x | 13.8x | |
| sieve | 114.7 | 128.5 | 112.1 | 952.6 | | 1.0x | 8.5x | |
| strings | 86.5 | 123.8 | 246.8 | 1204.1 | | 2.9x | 4.9x | |
| vector | 60.3 | 141.4 | 88.4 | 2769.5 | 240.5 | 1.5x | 31.3x | 2.7x |
| wordcount | 217.7 | 220.1 | 288.2 | 938.2 | | 1.3x | 3.3x | |
| geometric mean | | | | | | 2.1x | 14.3x | 2.7x |

Compiled stone is about 2.1x slower than `gcc -O2` and about 14x faster than CPython, as a geometric mean. It is faster than `gcc -O0` on `binary_trees`, `collatz`, `globals`, `sieve`, and `vector`, and on `vector` it is about 2.7x faster than numpy. The Python ratio is lower than in earlier baselines mostly because `strings` and `wordcount` spend their time in work CPython does in C: splitting and joining strings, and hashing in `dict`.

- `sort` was added on 2026-10-17, after this baseline. That day, pinned the same way, it took 227 ms in stone, 772 ms with `gcc -O2`, and 5288 ms in Python. Stone wins because C's version calls a comparison function through `qsort` for every comparison, while stone sorts ints and floats by radix, a pass per byte in which they differ.
- `sieve` is close to C because allocating and touching 40 MB of memory dominates all three native versions.
- `globals` looks close to C only because C's version is also slow: once the modulus is a variable instead of a constant, gcc has to use `idiv` too. Compare stone's `globals` with its own `lcg` to see what reading globals costs.
- `binary_trees` is close to C because both spend their time allocating and freeing nodes, and stone's allocator, which keeps a free list per block size, keeps up with glibc's `malloc` there. Every `Tree.Leaf` is one shared static value, so stone allocates exactly the nodes C does.

### History

| change | collatz | fib | globals | lcg | matmul | sieve | geometric mean |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| first baseline | 12.2x | 4.9x | 2.3x | 9.8x | 15.8x | 1.4x | 5.6x |
| constant divisors skip checks, powers of two shift | 4.4x | 5.7x | 1.5x | 3.2x | 15.1x | 1.7x | 3.8x |
| list index checks inline instead of calling `stone.list_slot` | 4.1x | 5.8x | 1.6x | 3.2x | 13.3x | 2.0x | 3.8x |
| IR, linear-scan register allocation, and compare-and-branch conditions | 1.8x | 3.7x | 1.1x | 1.4x | 4.4x | 1.0x | 1.9x |
| programs use `%`, which shifts for a power of two, and read their sizes from `.args` | 1.7x | 4.0x | 1.0x | 1.6x | 4.8x | 1.0x | 1.9x |

Each cell is stone's time over `gcc -O2`'s from the same session, and the geometric mean covers the six columns shown. Benchmarks a change does not touch still move by a few tenths between rows, which shows the noise. The last row changed the C versions too, since gcc can no longer specialize on a constant size. For a single change, timing the old and new stone binaries against each other in one session is more reliable: inlining index checks made `matmul` 1.25x faster that way, and left `sieve` unchanged, since memory traffic dominates it. Register allocation, timed the same way with `--stone`, made `collatz` 2.6x faster, `matmul` and `matmul_float` 2.5x, `lcg` 2.3x, `fib` 1.6x, `globals` 1.35x, and `sieve` 1.17x.

Rewriting `x - (x / m) * m` as `x % m` first made `lcg` and `collatz` about 3.5x slower, since `%` by a constant power of two still used `idiv` where `/` already shifted. Computing the remainder from the shifted quotient made `lcg` 3.7x faster than that and `collatz` 3.2x, timed the same way. Checking an index into a list of structs or tuples without dividing the list's slot count by the element's width made `nbody` 3.9x faster.

## Where the time goes

The generated assembly for each benchmark is left at `bench/target/programs/<name>-stone.s`, and `STONE_DEBUG=1` with a debug build prints each function's IR and register assignment. Values live in registers, and loop tests compare and jump directly, so the remaining gap to C comes mostly from:

- **Only five registers survive calls.** A value that lives across a call needs one of the callee-saved registers `rbx` and `r12` to `r15`, and the rest spill to the stack, the least-used first. `matmul`'s `multiply` keeps the matrices and inner counters in registers, but its outer loop counters live in memory, since `append` is called inside that loop. Each vreg also gets one location for its whole life, with no splitting around calls.
- **Division by a variable uses `idiv`.** A literal divisor that is a power of two compiles to shifts, for `/` and `%` alike, and any other literal except 0 and -1 skips the zero and overflow checks. A divisor held in a variable still gets both checks and an `idiv`, while C has no checks at all, since dividing by zero is undefined behavior there.
- **Calls do extra bookkeeping.** Every call increments `stone.call_depth`, compares it with 1,000, and decrements it on return, so recursion stays within the language's depth limit. This is most of what separates `fib` from C.
- **List access chases two pointers and checks bounds.** An index loads the list's header, adjusts a negative index, compares it with the length, then loads the data pointer and the element. For `list[list[int]]` that happens twice per `a[i][k]`, where C's flat array needs one load.
- **Globals are checked on every read in a function.** `globals` compared with `lcg` shows the cost of the `g.<name>.set` check, plus the load itself, since globals always live in memory.
- **Loops test at the top.** Each iteration jumps back to the test, then branches into the body, where gcc puts the test at the bottom so an iteration takes one branch.
- **Nothing is inlined.** A call passes every value of a struct separately, so `nbody`'s `bodies[i].drift(dt)` passes a body's seven floats and `dt`, two of them on the stack, and gets the seven floats back, four of them through memory, where gcc inlines `drift` and changes the fields in place.

### Structs, strings, maps, and enums

`nbody` is the largest gap. A `list[Body]` stores each body's seven floats side by side, as C's array of structs does, but every change such as `bodies[i].vx -= dx * mj` is its own statement: it checks that the list is not shared (`ListUnique`), checks `i` against the length, and reloads the list's data pointer, and the next line does it all again. gcc loads `bodies[i]` and `bodies[j]` once per pair and keeps their fields in registers. Removing checks it can prove redundant, such as a second uniqueness or bounds check of the same list and index, is future work.

`strings` allocates a counted string for every `str()`, a list of them, the joined sentence, and then a string for every word `split` finds, where C formats each number straight into one buffer and walks it in place. Each of those strings is reference counted and freed when the round's list goes away.

`wordcount` allocates a string for each `str()` key and then hashes it twice, once for `get` and once to set the new count, as Python's `counts.get(word, 0) + 1` does. C formats each key into a stack buffer and finds its slot once.

### Arithmetic on lists

`vector` measures arithmetic on whole lists, which numpy is built for. An expression such as `0.5 * x + y` compiles to one loop with no list for any intermediate result, where numpy makes a temporary array for each operator. The loop runs over pairs of floats with SSE2 (NEON on arm64), then finishes any odd element one at a time, and writes over `y` in place, since `y` is assigned and nothing else holds its list. The baseline above shows the result.

The times include building the lists and summing them with ordinary loops, plus Python's startup and importing numpy, which is about 100 ms of numpy's time. Timing only the passes over the lists, stone took about 1.25x as long as `gcc -O2`, which also uses SSE2 here, and numpy about 2.5x as long as stone, or 1.9x with `out=` arguments that avoid its temporaries.

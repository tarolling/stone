import sys


def step(x):
    if x % 7 == 0:
        return None
    return (x * 31 + 7) % 1000003


def run(n):
    x = 1
    hits = 0
    misses = 0
    kept = []
    for i in range(n):
        y = step(x + i)
        if y is not None:
            x = y
            hits += 1
        else:
            x += 1
            misses += 1
        if i % 16 == 0:
            kept.append(step(i))
    total = 0
    for k in kept:
        total += k if k is not None else 0
    print(x, hits, misses, total)


run(int(sys.argv[1]))

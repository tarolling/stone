def chain(n):
    steps = 0
    while n != 1:
        if n % 2 == 0:
            n = n // 2
        else:
            n = 3 * n + 1
        steps = steps + 1
    return steps


def longest(limit):
    best = 0
    best_start = 0
    for start in range(1, limit):
        steps = chain(start)
        if steps > best:
            best = steps
            best_start = start
    return best_start


print(longest(300000))

# branches and division: the start below a limit with the longest Collatz chain
def chain(n);
    steps = 0
    while n != 1;
        if n - (n / 2) * 2 == 0;
            n = n / 2
        else;
            n = 3 * n + 1
        steps = steps + 1
    ret steps

def longest(limit);
    best = 0
    best_start = 0
    for start in range(1, limit);
        steps = chain(start)
        if steps > best;
            best = steps
            best_start = start
    ret best_start

print(longest(300000))

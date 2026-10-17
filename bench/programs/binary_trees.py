import sys


# a leaf is None, and a node is a pair of trees
def make(depth):
    if depth == 0:
        return (None, None)
    return (make(depth - 1), make(depth - 1))


def check(t):
    if t is None:
        return 0
    left, right = t
    return 1 + check(left) + check(right)


def run(max_depth):
    print(max_depth + 1, check(make(max_depth + 1)))
    long_lived = make(max_depth)
    depth = 4
    while depth <= max_depth:
        iterations = 2 ** (max_depth - depth + 4)
        total = 0
        for _ in range(iterations):
            total += check(make(depth))
        print(depth, iterations, total)
        depth += 2
    print(max_depth, check(long_lived))


run(int(sys.argv[1]))

import sys


def lcg(rounds):
    x = 1
    total = 0
    for _ in range(rounds):
        x = (x * 1103515245 + 12345) % 2147483648
        total += x // 65536
    return total


print(lcg(int(sys.argv[1])))

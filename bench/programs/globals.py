import sys

multiplier = 1103515245
increment = 12345
modulus = 2147483648


def lcg(rounds):
    x = 1
    total = 0
    for _ in range(rounds):
        x = (x * multiplier + increment) % modulus
        total += x // 65536
    return total


print(lcg(int(sys.argv[1])))

import sys


def draw(x):
    return (x * 1103515245 + 12345) % 2147483648


def main(n):
    x = 1
    ints = []
    for _ in range(n):
        x = draw(x)
        high = x
        x = draw(x)
        ints.append(high * 2147483648 + x)
    ints.sort()
    total = 0
    for v in ints:
        total = (total * 31 + v % 1000000007) % 1000000007
    print(total, ints[0], ints[n - 1])

    floats = []
    for _ in range(n):
        x = draw(x)
        floats.append(float(x) / 2147483648.0 - 0.5)
    floats.sort()
    total = 0.0
    for f in floats:
        total = total * 0.5 + f
    print(int(total * 1000000000.0), int(floats[0] * 1000000000.0), int(floats[n - 1] * 1000000000.0))


main(int(sys.argv[1]))

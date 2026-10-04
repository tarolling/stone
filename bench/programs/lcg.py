def lcg(rounds):
    x = 1
    total = 0
    i = 0
    while i < rounds:
        x = (x * 1103515245 + 12345) % 2147483648
        total = total + x // 65536
        i = i + 1
    return total


print(lcg(20000000))

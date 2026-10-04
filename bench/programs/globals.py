multiplier = 1103515245
increment = 12345
modulus = 2147483648


def lcg(rounds):
    x = 1
    total = 0
    i = 0
    while i < rounds:
        x = (x * multiplier + increment) % modulus
        total = total + x // 65536
        i = i + 1
    return total


print(lcg(20000000))

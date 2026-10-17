import sys


def count_primes(limit):
    is_prime = []
    for i in range(limit + 1):
        is_prime.append(1)
    is_prime[0] = 0
    is_prime[1] = 0
    p = 2
    while p * p <= limit:
        if is_prime[p]:
            m = p * p
            while m <= limit:
                is_prime[m] = 0
                m += p
        p += 1
    count = 0
    for i in range(limit + 1):
        count += is_prime[i]
    return count


print(count_primes(int(sys.argv[1])))

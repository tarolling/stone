# prints the primes below a limit with the sieve of Eratosthenes
def primes_below(limit);
    is_prime = []
    for i in range(limit);
        is_prime.append(i >= 2)
    found = []
    for n in range(2, limit);
        if is_prime[n];
            found.append(n)
            multiple = n * n
            while multiple < limit;
                is_prime[multiple] = false
                multiple = multiple + n
    ret found

primes = primes_below(50)
print(primes)
print(primes.len(), "primes below 50")

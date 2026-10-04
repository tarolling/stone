#include <stdio.h>
#include <stdlib.h>

long count_primes(long limit) {
    long *is_prime = malloc((limit + 1) * sizeof(long));
    for (long i = 0; i <= limit; i++)
        is_prime[i] = 1;
    is_prime[0] = 0;
    is_prime[1] = 0;
    for (long p = 2; p * p <= limit; p++) {
        if (is_prime[p]) {
            for (long m = p * p; m <= limit; m += p)
                is_prime[m] = 0;
        }
    }
    long count = 0;
    for (long i = 0; i <= limit; i++)
        count += is_prime[i];
    free(is_prime);
    return count;
}

int main(void) {
    printf("%ld\n", count_primes(5000000));
    return 0;
}

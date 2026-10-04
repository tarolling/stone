#include <stdio.h>
#include <stdlib.h>

long *matrix(long n, long seed) {
    long *m = malloc(n * n * sizeof(long));
    long x = seed;
    for (long i = 0; i < n * n; i++) {
        x = (x * 1103515245 + 12345) % 2147483648;
        m[i] = x / 2097152;
    }
    return m;
}

long *multiply(const long *a, const long *b, long n) {
    long *c = malloc(n * n * sizeof(long));
    for (long i = 0; i < n; i++) {
        for (long j = 0; j < n; j++) {
            long total = 0;
            for (long k = 0; k < n; k++)
                total += a[i * n + k] * b[k * n + j];
            c[i * n + j] = total;
        }
    }
    return c;
}

long checksum(const long *c, long n) {
    long total = 0;
    for (long i = 0; i < n; i++)
        for (long j = 0; j < n; j++)
            total += c[i * n + j] * (i + j + 1);
    return total;
}

int main(void) {
    long n = 300;
    long *a = matrix(n, 1);
    long *b = matrix(n, 2);
    long *c = multiply(a, b, n);
    printf("%ld\n", checksum(c, n));
    free(a);
    free(b);
    free(c);
    return 0;
}

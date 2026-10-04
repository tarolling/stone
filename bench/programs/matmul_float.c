#include <stdio.h>
#include <stdlib.h>

double *matrix(long n, long seed) {
    double *m = malloc(n * n * sizeof(double));
    long x = seed;
    for (long i = 0; i < n * n; i++) {
        x = (x * 1103515245 + 12345) % 2147483648;
        m[i] = (double)x / 2147483648.0;
    }
    return m;
}

double *multiply(const double *a, const double *b, long n) {
    double *c = malloc(n * n * sizeof(double));
    for (long i = 0; i < n; i++) {
        for (long j = 0; j < n; j++) {
            double total = 0.0;
            for (long k = 0; k < n; k++)
                total += a[i * n + k] * b[k * n + j];
            c[i * n + j] = total;
        }
    }
    return c;
}

double checksum(const double *c, long n) {
    double total = 0.0;
    for (long i = 0; i < n; i++)
        for (long j = 0; j < n; j++)
            total += c[i * n + j] * (double)(i + j + 1);
    return total;
}

int main(void) {
    long n = 300;
    double *a = matrix(n, 1);
    double *b = matrix(n, 2);
    double *c = multiply(a, b, n);
    printf("%ld\n", (long)(checksum(c, n) * 1000000.0));
    free(a);
    free(b);
    free(c);
    return 0;
}

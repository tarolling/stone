#include <stdio.h>
#include <stdlib.h>

static long draw(long x) { return (x * 1103515245 + 12345) % 2147483648; }

static int compare_longs(const void *a, const void *b) {
    long x = *(const long *)a, y = *(const long *)b;
    return (x > y) - (x < y);
}

static int compare_doubles(const void *a, const void *b) {
    double x = *(const double *)a, y = *(const double *)b;
    return (x > y) - (x < y);
}

int main(int argc, char **argv) {
    long n = atol(argv[1]);
    long x = 1;
    long *ints = malloc(n * sizeof *ints);
    for (long i = 0; i < n; i++) {
        x = draw(x);
        long high = x;
        x = draw(x);
        ints[i] = high * 2147483648 + x;
    }
    qsort(ints, n, sizeof *ints, compare_longs);
    long total = 0;
    for (long i = 0; i < n; i++) {
        total = (total * 31 + ints[i] % 1000000007) % 1000000007;
    }
    printf("%ld %ld %ld\n", total, ints[0], ints[n - 1]);

    double *floats = malloc(n * sizeof *floats);
    for (long i = 0; i < n; i++) {
        x = draw(x);
        floats[i] = (double)x / 2147483648.0 - 0.5;
    }
    qsort(floats, n, sizeof *floats, compare_doubles);
    double sum = 0.0;
    for (long i = 0; i < n; i++) {
        sum = sum * 0.5 + floats[i];
    }
    printf("%ld %ld %ld\n", (long)(sum * 1000000000.0), (long)(floats[0] * 1000000000.0),
           (long)(floats[n - 1] * 1000000000.0));
    free(ints);
    free(floats);
    return 0;
}

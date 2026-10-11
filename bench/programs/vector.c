#include <stdio.h>
#include <stdlib.h>

static void saxpy(double a, const double *x, double *y, long n, int reps) {
    for (int r = 0; r < reps; r++)
        for (long i = 0; i < n; i++)
            y[i] = a * x[i] + y[i];
}

int main(void) {
    long n = 1000000;
    double *x = malloc(n * sizeof *x);
    double *y = malloc(n * sizeof *y);
    for (long i = 0; i < n; i++) {
        x[i] = (double)(i % 7);
        y[i] = (double)(i % 5);
    }
    saxpy(0.5, x, y, n, 50);
    double total = 0.0;
    for (long i = 0; i < n; i++)
        total += y[i];
    printf("%ld\n", (long)total);
    free(x);
    free(y);
    return 0;
}

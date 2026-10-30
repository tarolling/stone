#include <math.h>
#include <stdio.h>
#include <stdlib.h>

double walk(long steps) {
    double x = 1.0, y = 0.0, total = 0.0;
    for (long i = 0; i < steps; i++) {
        double t = (double)i * 0.001;
        double r = 1.0 + 0.5 * sin(t * 3.0);
        x = r * cos(t);
        y = r * sin(t);
        total += atan2(y, x) + asin(sin(t) * 0.5);
    }
    return total;
}

int main(int argc, char **argv) {
    printf("%ld\n", (long)(walk(atol(argv[1])) * 1000.0));
    return 0;
}

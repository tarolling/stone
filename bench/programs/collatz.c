#include <stdio.h>
#include <stdlib.h>

long chain(long n) {
    long steps = 0;
    while (n != 1) {
        if (n % 2 == 0)
            n /= 2;
        else
            n = 3 * n + 1;
        steps++;
    }
    return steps;
}

long longest(long limit) {
    long best = 0;
    long best_start = 0;
    for (long start = 1; start < limit; start++) {
        long steps = chain(start);
        if (steps > best) {
            best = steps;
            best_start = start;
        }
    }
    return best_start;
}

int main(int argc, char **argv) {
    printf("%ld\n", longest(atol(argv[1])));
    return 0;
}

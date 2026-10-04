#include <stdio.h>

long chain(long n) {
    long steps = 0;
    while (n != 1) {
        if (n % 2 == 0)
            n = n / 2;
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

int main(void) {
    printf("%ld\n", longest(300000));
    return 0;
}

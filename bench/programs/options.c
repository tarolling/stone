#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int some;
    long value;
} option;

static option step(long x) {
    if (x % 7 == 0) {
        return (option){0, 0};
    }
    return (option){1, (x * 31 + 7) % 1000003};
}

int main(int argc, char **argv) {
    long n = atol(argv[1]);
    long x = 1, hits = 0, misses = 0;
    long cap = n / 16 + 1, len = 0;
    option *kept = malloc(cap * sizeof(option));
    for (long i = 0; i < n; i++) {
        option o = step(x + i);
        if (o.some) {
            x = o.value;
            hits++;
        } else {
            x++;
            misses++;
        }
        if (i % 16 == 0) {
            kept[len++] = step(i);
        }
    }
    long total = 0;
    for (long k = 0; k < len; k++) {
        total += kept[k].some ? kept[k].value : 0;
    }
    printf("%ld %ld %ld %ld\n", x, hits, misses, total);
    free(kept);
    return 0;
}

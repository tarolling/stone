#include <stdio.h>
#include <stdlib.h>

long lcg(long rounds) {
    long x = 1;
    long total = 0;
    for (long i = 0; i < rounds; i++) {
        x = (x * 1103515245 + 12345) % 2147483648;
        total += x / 65536;
    }
    return total;
}

int main(int argc, char **argv) {
    printf("%ld\n", lcg(atol(argv[1])));
    return 0;
}

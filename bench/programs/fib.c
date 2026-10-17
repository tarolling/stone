#include <stdio.h>
#include <stdlib.h>

long fib(long n) {
    if (n < 2)
        return n;
    return fib(n - 1) + fib(n - 2);
}

int main(int argc, char **argv) {
    printf("%ld\n", fib(atol(argv[1])));
    return 0;
}

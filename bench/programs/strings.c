#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// the words joined by spaces, in a buffer the caller frees
char *sentence(long n, long seed) {
    char *text = malloc(n * 6 + 1);
    long len = 0;
    long x = seed;
    for (long i = 0; i < n; i++) {
        x = (x * 1103515245 + 12345) % 2147483648;
        if (i > 0)
            text[len++] = ' ';
        len += sprintf(text + len, "%ld", x / 65536);
    }
    text[len] = '\0';
    return text;
}

long score(const char *text) {
    long total = 0;
    const char *p = text;
    while (*p) {
        while (*p == ' ')
            p++;
        const char *start = p;
        while (*p && *p != ' ')
            p++;
        long len = p - start;
        if (len == 0)
            break;
        total += len;
        if (start[0] == '1')
            total += 1;
        if (start[len - 1] == '7')
            total += 2;
        if (start[0] == start[len - 1])
            total += 3;
    }
    return total;
}

long run(long rounds, long n) {
    long total = 0;
    for (long r = 0; r < rounds; r++) {
        char *text = sentence(n, r + 1);
        const char *found = strstr(text, "4242");
        total += score(text) + (found ? found - text : -1);
        free(text);
    }
    return total;
}

int main(int argc, char **argv) {
    printf("%ld\n", run(atol(argv[1]), atol(argv[2])));
    return 0;
}

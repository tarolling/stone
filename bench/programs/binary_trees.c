#include <stdio.h>
#include <stdlib.h>

// a leaf is a null pointer
typedef struct Node {
    struct Node *left;
    struct Node *right;
} Node;

Node *make(long depth) {
    Node *t = malloc(sizeof(Node));
    if (depth == 0) {
        t->left = NULL;
        t->right = NULL;
    } else {
        t->left = make(depth - 1);
        t->right = make(depth - 1);
    }
    return t;
}

long check(const Node *t) {
    if (!t)
        return 0;
    return 1 + check(t->left) + check(t->right);
}

void drop(Node *t) {
    if (!t)
        return;
    drop(t->left);
    drop(t->right);
    free(t);
}

void run(long max_depth) {
    Node *stretch = make(max_depth + 1);
    printf("%ld %ld\n", max_depth + 1, check(stretch));
    drop(stretch);
    Node *long_lived = make(max_depth);
    for (long depth = 4; depth <= max_depth; depth += 2) {
        long iterations = 1L << (max_depth - depth + 4);
        long total = 0;
        for (long i = 0; i < iterations; i++) {
            Node *t = make(depth);
            total += check(t);
            drop(t);
        }
        printf("%ld %ld %ld\n", depth, iterations, total);
    }
    printf("%ld %ld\n", max_depth, check(long_lived));
    drop(long_lived);
}

int main(int argc, char **argv) {
    run(atol(argv[1]));
    return 0;
}

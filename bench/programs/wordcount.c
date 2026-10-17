#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// an open-addressing hash table from strings to counts, grown at half load
typedef struct {
    char *key;
    long count;
} Entry;

typedef struct {
    Entry *entries;
    long capacity;
    long len;
} Table;

static unsigned long hash(const char *s) {
    unsigned long h = 14695981039346656037UL;
    for (; *s; s++)
        h = (h ^ (unsigned char)*s) * 1099511628211UL;
    return h;
}

static Entry *slot(Entry *entries, long capacity, const char *key) {
    unsigned long i = hash(key) & (capacity - 1);
    while (entries[i].key && strcmp(entries[i].key, key) != 0)
        i = (i + 1) & (capacity - 1);
    return &entries[i];
}

static void grow(Table *t) {
    long capacity = t->capacity * 2;
    Entry *entries = calloc(capacity, sizeof(Entry));
    for (long i = 0; i < t->capacity; i++)
        if (t->entries[i].key)
            *slot(entries, capacity, t->entries[i].key) = t->entries[i];
    free(t->entries);
    t->entries = entries;
    t->capacity = capacity;
}

static void add(Table *t, const char *key) {
    Entry *e = slot(t->entries, t->capacity, key);
    if (e->key) {
        e->count++;
        return;
    }
    e->key = strdup(key);
    e->count = 1;
    if (++t->len * 2 > t->capacity)
        grow(t);
}

static long get(const Table *t, const char *key) {
    return slot(t->entries, t->capacity, key)->count;
}

Table count_words(long n, long k) {
    Table t = {calloc(16, sizeof(Entry)), 16, 0};
    long x = 1;
    char word[24];
    for (long i = 0; i < n; i++) {
        x = (x * 1103515245 + 12345) % 2147483648;
        sprintf(word, "%ld", x / 64 % k);
        add(&t, word);
    }
    return t;
}

long checksum(const Table *t, long k) {
    long total = t->len;
    char word[24];
    for (long j = 0; j < k; j++) {
        sprintf(word, "%ld", j);
        total += get(t, word) * (j % 7 + 1);
    }
    return total;
}

int main(int argc, char **argv) {
    long k = atol(argv[2]);
    Table t = count_words(atol(argv[1]), k);
    printf("%ld\n", checksum(&t, k));
    for (long i = 0; i < t.capacity; i++)
        free(t.entries[i].key);
    free(t.entries);
    return 0;
}

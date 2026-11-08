// json_records: writing a list of records as JSON text, reading it back, and walking what was read
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum kind { NUL, BOOL, INT, FLOAT, STR, ARR, OBJ };

typedef struct value value;
struct value {
    enum kind kind;
    int64_t i;
    double f;
    char *s;
    size_t len; // the string's bytes, or the items or entries
    value *items;
    char **keys;
    size_t *key_lens;
};

typedef struct {
    int64_t id;
    char name[32];
    double score;
    int active;
    int tags;
    int64_t x, y;
} record;

typedef struct {
    char *data;
    size_t len, cap;
} buffer;

static void put(buffer *b, const char *s, size_t n) {
    if (b->len + n > b->cap) {
        while (b->len + n > b->cap)
            b->cap = b->cap ? b->cap * 2 : 4096;
        b->data = realloc(b->data, b->cap);
    }
    memcpy(b->data + b->len, s, n);
    b->len += n;
}

static void put_str(buffer *b, const char *s) { put(b, s, strlen(s)); }

static void put_int(buffer *b, int64_t n) {
    char text[24];
    put(b, text, (size_t)snprintf(text, sizeof text, "%lld", (long long)n));
}

// a multiple of 0.25, written as Python writes it: at least one digit after the point
static void put_score(buffer *b, double x) {
    char text[32];
    int n = snprintf(text, sizeof text, "%.2f", x);
    while (text[n - 1] == '0' && text[n - 2] != '.')
        n--;
    put(b, text, (size_t)n);
}

static void put_quoted(buffer *b, const char *s) {
    put(b, "\"", 1);
    for (; *s; s++) {
        if (*s == '"')
            put(b, "\\\"", 2);
        else if (*s == '\\')
            put(b, "\\\\", 2);
        else if (*s == '\n')
            put(b, "\\n", 2);
        else
            put(b, s, 1);
    }
    put(b, "\"", 1);
}

static void write_records(buffer *b, record *rs, int n) {
    put(b, "[", 1);
    for (int i = 0; i < n; i++) {
        record *r = &rs[i];
        if (i > 0)
            put(b, ",", 1);
        put_str(b, "{\"id\":");
        put_int(b, r->id);
        put_str(b, ",\"name\":");
        put_quoted(b, r->name);
        put_str(b, ",\"score\":");
        put_score(b, r->score);
        put_str(b, r->active ? ",\"active\":true" : ",\"active\":false");
        put_str(b, ",\"tags\":[");
        for (int k = 0; k < r->tags; k++) {
            char tag[16];
            snprintf(tag, sizeof tag, "tag%d", k);
            if (k > 0)
                put(b, ",", 1);
            put_quoted(b, tag);
        }
        put_str(b, "],\"pos\":{\"x\":");
        put_int(b, r->x);
        put_str(b, ",\"y\":");
        put_int(b, r->y);
        put_str(b, "},\"note\":null}");
    }
    put(b, "]", 1);
}

static const char *p;

static void space(void) {
    while (*p == ' ' || *p == '\n' || *p == '\r' || *p == '\t')
        p++;
}

// reads the string at p, which is at most as long as its text in the source
static char *string(size_t *len) {
    p++; // the quote
    const char *end = p;
    while (*end != '"')
        end += *end == '\\' ? 2 : 1;
    char *out = malloc((size_t)(end - p) + 1);
    size_t n = 0;
    while (*p != '"') {
        if (*p == '\\') {
            p++;
            out[n++] = *p == 'n' ? '\n' : *p == 't' ? '\t' : *p;
            p++;
        } else {
            out[n++] = *p++;
        }
    }
    p++;
    out[n] = 0;
    *len = n;
    return out;
}

static void parse(value *v) {
    space();
    memset(v, 0, sizeof *v);
    if (*p == '{') {
        v->kind = OBJ;
        size_t cap = 0;
        p++;
        space();
        if (*p == '}') {
            p++;
            return;
        }
        for (;;) {
            space();
            if (v->len == cap) {
                cap = cap ? cap * 2 : 4;
                v->items = realloc(v->items, cap * sizeof *v->items);
                v->keys = realloc(v->keys, cap * sizeof *v->keys);
                v->key_lens = realloc(v->key_lens, cap * sizeof *v->key_lens);
            }
            v->keys[v->len] = string(&v->key_lens[v->len]);
            space();
            p++; // ':'
            parse(&v->items[v->len++]);
            space();
            if (*p++ == '}')
                return;
        }
    } else if (*p == '[') {
        v->kind = ARR;
        size_t cap = 0;
        p++;
        space();
        if (*p == ']') {
            p++;
            return;
        }
        for (;;) {
            if (v->len == cap) {
                cap = cap ? cap * 2 : 4;
                v->items = realloc(v->items, cap * sizeof *v->items);
            }
            parse(&v->items[v->len++]);
            space();
            if (*p++ == ']')
                return;
        }
    } else if (*p == '"') {
        v->kind = STR;
        v->s = string(&v->len);
    } else if (*p == 't') {
        v->kind = BOOL;
        v->i = 1;
        p += 4;
    } else if (*p == 'f') {
        v->kind = BOOL;
        p += 5;
    } else if (*p == 'n') {
        p += 4;
    } else {
        const char *start = p;
        int exact = 1;
        if (*p == '-')
            p++;
        while ((*p >= '0' && *p <= '9') || *p == '.' || *p == 'e' || *p == 'E' || *p == '+' ||
               *p == '-') {
            if (*p == '.' || *p == 'e' || *p == 'E')
                exact = 0;
            p++;
        }
        if (exact) {
            v->kind = INT;
            v->i = strtoll(start, NULL, 10);
        } else {
            v->kind = FLOAT;
            v->f = strtod(start, NULL);
        }
    }
}

static int64_t checksum(value *v) {
    int64_t total;
    switch (v->kind) {
    case NUL:
        return 1;
    case BOOL:
        return v->i ? 3 : 2;
    case INT:
        return v->i;
    case FLOAT:
        return (int64_t)(v->f * 4.0);
    case STR:
        return (int64_t)v->len;
    case ARR:
        total = 7;
        for (size_t i = 0; i < v->len; i++)
            total += checksum(&v->items[i]);
        return total;
    default:
        total = 11;
        for (size_t i = 0; i < v->len; i++)
            total += (int64_t)v->key_lens[i] + checksum(&v->items[i]);
        return total;
    }
}

static void release(value *v) {
    if (v->kind == STR)
        free(v->s);
    for (size_t i = 0; i < (v->kind == ARR || v->kind == OBJ ? v->len : 0); i++) {
        release(&v->items[i]);
        if (v->kind == OBJ)
            free(v->keys[i]);
    }
    free(v->items);
    free(v->keys);
    free(v->key_lens);
}

static int64_t run(int rounds, int n) {
    record *rs = malloc((size_t)n * sizeof *rs);
    int64_t x = 12345;
    for (int i = 0; i < n; i++) {
        record *r = &rs[i];
        x = (x * 1103515245 + 12345) % 2147483648;
        r->id = i;
        if (x % 7 == 0)
            snprintf(r->name, sizeof r->name, "say \"hi\"\nuser%lld", (long long)(x % 1000));
        else
            snprintf(r->name, sizeof r->name, "user%lld", (long long)(x % 1000));
        r->score = (double)(x % 4000) / 4.0;
        r->active = x % 3 == 0;
        r->tags = (int)(x % 4);
        r->x = x % 640;
        r->y = x / 640 % 480;
    }
    int64_t total = 0;
    for (int round = 0; round < rounds; round++) {
        buffer text = {0};
        write_records(&text, rs, n);
        put(&text, "", 1);
        p = text.data;
        value v;
        parse(&v);
        total += (int64_t)(text.len - 1) + checksum(&v);
        release(&v);
        free(text.data);
    }
    free(rs);
    return total;
}

int main(int argc, char **argv) {
    (void)argc;
    printf("%lld\n", (long long)run(atoi(argv[1]), atoi(argv[2])));
    return 0;
}

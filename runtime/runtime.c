#include <stdio.h>
#include <stdlib.h>

#define min(a, b) \
            ((a) < (b) ? (a) : (b))

typedef struct {
    int tag;
    char data[0];
} seq_t;

extern int read()
{
    int d;
    printf("> ");
    scanf("%d", &d);
    return d;
}

extern void write(int x)
{
    printf("%d\n", x);
}

extern seq_t* strmake(int n, int c)
{
    seq_t* str = malloc(sizeof(str->tag) + n);
    str->tag = n;
    memset(str->data, c, n);
    return str;
}

extern seq_t* strset(seq_t* str, int i, int c)
{
    str->data[i] = c;
    return str;
}

extern int strget(seq_t* str, int i)
{
    return str->data[i];
}

extern seq_t* strdup(seq_t* str)
{
    int l = sizeof(str->tag) + str->tag;
    seq_t* dup = malloc(l);
    memcpy(dup, str, l);
    return dup;
}

extern seq_t* strcat(seq_t* a, seq_t* b)
{
    seq_t* ab = malloc(sizeof(ab->tag) + a->tag + b->tag);
    ab->tag = a->tag + b->tag;
    memcpy(ab->data, a->data, a->tag);
    memcpy(ab->data + a->tag, b->data, b->tag);
    return ab;
}

extern int strcmp(seq_t* s1, seq_t* s2)
{
    int i = 0;
    while (i < min(s1->tag, s2->tag) && s1->data[i] == s2->data[i]) {
         ++i;
    }

    if (i == min(s1->tag, s2->tag)) {
        if (s1->tag < s2->tag)
            return -1;
        if (s1->tag > s2->tag)
            return 1;
        return 0;
    }

    return (s1->data[i] < s2->data[i] ? -1 : 1);
}

extern int strlen(seq_t* s)
{
    return s->tag;
}

extern seq_t* strsub(seq_t* s, int i, int l)
{
    seq_t* r = malloc(sizeof(r->tag) + l);
    r->tag = l;
    memcpy(r->data, s->data + i, l);
    return r;
}

extern seq_t* arrmake(int n, int v)
{
    return strmake(n, v);
}

extern seq_t* Arrmake(int n, int v)
{
    return arrmake(n, v);
}

extern int arrlen(seq_t* arr)
{
    return arr->tag;
}

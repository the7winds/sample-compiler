#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define min(a, b) \
            ((a) < (b) ? (a) : (b))

typedef struct {
    int tag;
    char data[0];
} str_t;

typedef struct {
    int tag;
    int data[0];
} seq_t;

extern int _read()
{
    int d;
    printf("> ");
    scanf("%d", &d);
    return d;
}

extern void _write(int x)
{
    printf("%d\n", x);
}

extern str_t* _strmake(int n, int c)
{
    str_t* str = malloc(sizeof(str->tag) + n);
    str->tag = n;
    memset(str->data, c, n);
    return str;
}

extern str_t* _strset(str_t* str, int i, int c)
{
    str->data[i] = c;
    return str;
}

extern int _strget(str_t* str, int i)
{
    return str->data[i];
}

extern str_t* _strdup(str_t* str)
{
    int l = sizeof(str->tag) + str->tag;
    str_t* dup = malloc(l);
    memcpy(dup, str, l);
    return dup;
}

extern str_t* _strcat(str_t* a, str_t* b)
{
    str_t* ab = malloc(sizeof(ab->tag) + a->tag + b->tag);
    ab->tag = a->tag + b->tag;
    memcpy(ab->data, a->data, a->tag);
    memcpy(ab->data + a->tag, b->data, b->tag);
    return ab;
}

extern int _strcmp(str_t* s1, str_t* s2)
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

extern int _strlen(str_t* s)
{
    return s->tag;
}

extern str_t* _strsub(str_t* s, int i, int l)
{
    str_t* r = malloc(sizeof(r->tag) + l);
    r->tag = l;
    memcpy(r->data, s->data + i, l);
    return r;
}

extern seq_t* _arrmake(int n, int v)
{
    seq_t* arr = malloc(sizeof(arr->tag) + n * sizeof(int));
    arr->tag = n;
    for (int i = 0; i < n; ++i) {
        arr->data[i] = v;
    }
    return arr;
}

extern seq_t* _Arrmake(int n, int v)
{
    return _arrmake(n, v);
}

extern int _arrlen(str_t* arr)
{
    return arr->tag;
}

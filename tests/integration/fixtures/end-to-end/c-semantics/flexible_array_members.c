#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

struct Bytes {
    int length;
    unsigned char data[];
};

struct Words {
    char count;
    int data[];
};

struct Padded {
    double value;
    char tag;
    char data[];
};

union Either {
    struct Words words;
    int raw;
};

int main() {
    printf("layout %d %d %d %d %d %d\n",
        (int)sizeof(struct Bytes), (int)offsetof(struct Bytes, data),
        (int)sizeof(struct Words), (int)offsetof(struct Words, data),
        (int)sizeof(struct Padded), (int)offsetof(struct Padded, data));

    struct Words* words = malloc(sizeof(struct Words) + 4 * sizeof(int));
    words->count = 4;
    for (int i = 0; i < words->count; i++) {
        words->data[i] = i * 10;
    }
    int sum = 0;
    for (int i = 0; i < words->count; i++) {
        sum += words->data[i];
    }
    int* decayed = words->data;
    printf("sum %d %d\n", sum, decayed[3]);
    free(words);

    struct Words header = { 7 };
    union Either either;
    either.raw = 0;
    printf("header %d\n", header.count + either.raw);
    return 0;
}

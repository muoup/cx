/* CX-STDOUT: 0 8 16 8 16 24 32 12 */

#include <stddef.h>
#include <stdio.h>

struct Padded {
    char tag;
    union {
        double number;
        void *pointer;
    } payload;
    int trailing;
};

struct Anonymous {
    char prefix;
    struct {
        short marker;
        union {
            int items[4];
            long full;
        };
    };
    unsigned char trailing;
};

struct Named {
    char prefix;
    struct {
        int values[3];
    } inner;
};

int main(void) {
    const size_t payload = offsetof(struct Padded, payload);

    printf("%d %d %d %d %d %d %d %d\n", (int)offsetof(struct Padded, tag),
           (int)payload, (int)__builtin_offsetof(struct Padded, trailing),
           (int)offsetof(struct Anonymous, marker),
           (int)offsetof(struct Anonymous, full),
           (int)offsetof(struct Anonymous, items[1 + 1]),
           (int)offsetof(struct Anonymous, trailing),
           (int)offsetof(struct Named, inner.values[2]));
    return 0;
}

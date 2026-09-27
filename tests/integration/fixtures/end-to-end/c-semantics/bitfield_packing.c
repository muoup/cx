/* CX-STDOUT: size 8 4 */
/* CX-STDOUT-NEXT: init 5 3 9 1 */
/* CX-STDOUT-NEXT: neighbours 6 3 9 1 */
/* CX-STDOUT-NEXT: signed -3 7 -8 -1 */
/* CX-STDOUT-NEXT: compound 0 1 4 */
/* CX-STDOUT-NEXT: chained 3 3 */
/* CX-STDOUT-NEXT: copy 6 3 9 */
/* CX-STDOUT-NEXT: pointer 2 3 9 */
/* CX-STDOUT-NEXT: global 7 2 5 */
/* CX-STDOUT-NEXT: arg 11 */

#include <stdint.h>
#include <stdio.h>

struct Packed {
    uint32_t a : 3;
    uint32_t b : 2;
    uint32_t c : 4;
    uint32_t d : 1;
};

struct Split {
    uint32_t a : 3;
    uint32_t : 0;
    uint32_t b : 4;
};

struct Signed {
    int32_t x : 4;
    int32_t y : 4;
};

struct Packed global_packed = { 7, 2, 5, 0 };

static int identity(int value) {
    return value;
}

int main() {
    printf("size %d %d\n", (int)sizeof(struct Split), (int)sizeof(struct Packed));

    struct Packed packed = { 5, 3, 9, 1 };
    printf("init %d %d %d %d\n", packed.a, packed.b, packed.c, packed.d);

    packed.a = 6;
    printf("neighbours %d %d %d %d\n", packed.a, packed.b, packed.c, packed.d);

    struct Signed s;
    s.x = -3;
    s.y = 7;
    int first = s.x;
    int second = s.y;
    s.x = 8;
    s.y = -1;
    printf("signed %d %d %d %d\n", first, second, s.x, s.y);

    struct Packed counter = { 7, 0, 3, 0 };
    counter.a += 1;
    counter.b++;
    counter.c += counter.b;
    printf("compound %d %d %d\n", counter.a, counter.b, counter.c);

    struct Split split;
    split.a = 0;
    split.b = 3;
    split.a = split.b;
    printf("chained %d %d\n", split.a, split.b);

    struct Packed copy = packed;
    printf("copy %d %d %d\n", copy.a, copy.b, copy.c);

    struct Packed *pointer = &packed;
    pointer->a = 2;
    printf("pointer %d %d %d\n", pointer->a, pointer->b, pointer->c);

    printf("global %d %d %d\n", global_packed.a, global_packed.b, global_packed.c);

    printf("arg %d\n", identity(packed.c) + packed.a);

    return 0;
}

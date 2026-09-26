/* CX-STDOUT: 4294967295 -1 3 7 */

#include <stdio.h>

struct Counts {
    int signed_count;
    unsigned int unsigned_count;
};

static int seven(void) {
    return 7;
}

typedef int (*int_fn)(void);
typedef void (*void_fn)(void);

int main(void) {
    struct Counts counts = { -1, 4294967295u };
    unsigned int as_unsigned = counts.signed_count;
    int as_signed = counts.unsigned_count;

    int values[4] = { 0, 1, 2, 3 };
    void *erased = &values[3];
    int *restored = (int *) erased;

    void_fn erased_fn = (void_fn) seven;
    int_fn restored_fn = (int_fn) erased_fn;

    printf("%u %d %d %d\n", as_unsigned, as_signed, *restored, restored_fn());
    return 0;
}

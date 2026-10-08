/* CX-STDOUT: 5 12 */

#include <stdio.h>

#define ADD(a, b) ((a) + (b))
#define DEPRECATED(message) __attribute__((deprecated(message)))

static int twice(int value) DEPRECATED
    ("use ADD instead");

static int twice(int value) { return value * 2; }

int main(void) {
    int sum = ADD
        (2, 3);

    printf("%d %d\n", sum, twice(6));
    return 0;
}

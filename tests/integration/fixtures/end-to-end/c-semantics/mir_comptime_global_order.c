#include <stdio.h>

static int base = (3 * 7) - 4;
static int derived = base + 25;

int main(void) {
    printf("%d\n", derived);
    return derived == 42 ? 0 : 1;
}

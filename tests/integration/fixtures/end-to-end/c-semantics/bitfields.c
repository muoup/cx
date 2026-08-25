/* CX-STDOUT: 5 17 99 */
/* CX-STDOUT-NEXT: 7 7 */

#include <stdint.h>
#include <stdio.h>

struct Flags {
    uint32_t low : 3;
    uint32_t high : 5;
    int value;
};

union Bits {
    uint32_t low : 3;
    uint32_t all;
};

int main() {
    struct Flags flags;
    flags.low = 13;
    flags.high = 17;
    flags.value = 99;

    printf("%d %d %d\n", flags.low, flags.high, flags.value);

    union Bits bits;
    bits.all = 0;
    bits.low = 15;

    printf("%d %d\n", bits.low, bits.all);

    return 0;
}

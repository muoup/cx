/* CX-STDOUT: 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 */

#include <stdio.h>

int main(void) {
    unsigned long long wide = 0x8410afb240a3d8bbull;

    printf(
        "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
        sizeof(2147483647) == 4,
        sizeof(2147483648) == 8,
        sizeof(4294967295) == 8,
        sizeof(0xffffffff) == 4,
        sizeof(0x100000000) == 8,
        sizeof(037777777777) == 4,
        sizeof(1u) == 4,
        sizeof(1l) == 8,
        sizeof(1ll) == 8,
        sizeof(1ul) == 8,
        sizeof(1llu) == 8,
        wide == 0x8410afb240a3d8bbULL,
        sizeof(1.0f) == 4,
        sizeof(1.0) == 8,
        16777217.0f == 16777216.0f
    );
    return 0;
}

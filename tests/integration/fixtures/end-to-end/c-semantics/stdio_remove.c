#include <stdio.h>

int main(void) {
    return remove("/dev/null/cx-parity-nonexistent") == 0 ? 1 : 0;
}

#include <stdio.h>

int main(void) {
    return remove("/dev/null/chl-parity-nonexistent") == 0 ? 1 : 0;
}

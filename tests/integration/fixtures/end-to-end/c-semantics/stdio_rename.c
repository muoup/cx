#include <stdio.h>

int main(void) {
    return rename("/dev/null/cx-parity-old", "/dev/null/cx-parity-new") == 0 ? 1 : 0;
}

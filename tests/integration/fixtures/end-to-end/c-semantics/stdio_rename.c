#include <stdio.h>

int main(void) {
    return rename("/dev/null/chl-parity-old", "/dev/null/chl-parity-new") == 0 ? 1 : 0;
}

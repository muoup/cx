#include <stdio.h>

int main(void) {
    double value = 0x1.8p1 + 0x1p-1;

    printf("%.1f\n", value);
    return value == 3.5 ? 0 : 1;
}

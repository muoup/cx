#include <stdio.h>

int main(void) {
    int quotient = -7 / 3;
    int remainder = -7 % 3;

    printf("%d %d\n", quotient, remainder);
    return quotient == -2 && remainder == -1 ? 0 : 1;
}

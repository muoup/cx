#include <stdio.h>

int main() {
    int left = 3;
    unsigned int right = 16;

    left <<= 2;
    right >>= 2;

    printf("%d %u\n", left, right);
}

#include <stdio.h>

int main(void) {
    int value = 7;
    void *location = &value;
    printf("%d\n", *((int *) location));
    *((int *) location) = 9;
    printf("%d\n", value);
    return 0;
}

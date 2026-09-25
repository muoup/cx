#include <stdio.h>

int main(void) {
    int value = 2;
    printf("%d\n", --value);
    printf("%d\n", value--);
    printf("%d\n", value);
    printf("%d\n", ++value);
    return 0;
}

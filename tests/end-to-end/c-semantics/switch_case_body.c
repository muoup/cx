#include <stdio.h>

int main(void) {
    int value = 2;
    int result = 0;

    switch (value) {
        case 1:
            result = 10;
            result = result + 1;
            break;
        case 2:
            result = 20;
            result = result + 1;
            break;
        default:
            result = 30;
            result = result + 1;
            break;
    }

    printf("%d\n", result);
    return result == 21 ? 0 : 1;
}

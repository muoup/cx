#include <stdio.h>

int main(void) {
    unsigned char value = 66;
    int if_result = 0;
    int while_result = 0;
    int for_result = 0;

    if (value) {
        if_result = 1;
    }

    while (value) {
        while_result = 1;
        value = 0;
    }

    value = 66;
    for (; value; value = 0) {
        for_result = 1;
    }

    printf("%d %d %d\n", if_result, while_result, for_result);
    return 0;
}

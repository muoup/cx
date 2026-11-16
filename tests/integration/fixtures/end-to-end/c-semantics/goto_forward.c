#include <stdio.h>

int main(void) {
    printf("before\n");
    goto done;
    printf("skipped\n");
done:
    printf("after\n");
    return 0;
}

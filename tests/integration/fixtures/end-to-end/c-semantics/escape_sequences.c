#include <stdio.h>

int main(void) {
    printf("%d %d %d %d %d %d\n", '\"', '\\', '\x41', '\101', '\a', '\0');
    printf("a\\n\x41\101 \"q\"\n");
    return 0;
}

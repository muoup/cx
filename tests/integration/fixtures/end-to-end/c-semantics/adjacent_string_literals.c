#include <stdio.h>

void divider(void) {
    printf("divider\n");
}

int main(void) {
    if (1 >= 0 && 0 < 1) {
        printf("first\n"
               "second\n"
               "http://example.com\n");
        divider();
    }
    return 0;
}

/* CX-STDOUT: ok */

#include <stdio.h>

extern char *environ[];
static volatile char first_char;

static void touch_first(char **values) {
    if (values[0] != 0) {
        first_char = values[0][0];
    }
}

int main(void) {
    touch_first(environ);
    printf("ok\n");
    return 0;
}

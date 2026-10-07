/* CX-STDOUT: 1 d */

#include <stdio.h>

static int calls = 0;

static int bump(void) { return ++calls; }

int main(void) {
    const char *a = "ab", *b = "cd";
    int unused = 3;

    (void)unused;
    (void)bump();
    for (; *a; (void)a++, b++) {
    }

    printf("%d %c\n", calls, b[-1]);
    return 0;
}

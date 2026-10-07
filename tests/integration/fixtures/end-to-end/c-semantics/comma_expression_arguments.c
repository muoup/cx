/* CX-STDOUT: 15 14 16 9 */

#include <stdio.h>

#define checked(condition, value) ((void)(condition), (value))

static int add3(int a, int b, int c) { return a + b + c; }
static int twice(int a) { return a * 2; }

int main(void) {
    int state = 1;
    int a = add3(((void)0, 4), 5, 6);
    int b = ((void)state, twice(7));
    int c = twice(((void)state, 8));
    int d = add3(checked(state, 2), checked(state, 3), (state, 1, 4));

    printf("%d %d %d %d\n", a, b, c, d);
    return 0;
}

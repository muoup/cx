/* CX-STDOUT: 10 */

#include <stdio.h>

#define SUM(a, b, \
            c, d) \
    ((a) + (b) + (c) + (d))

int main(void) {
    printf("%d\n", SUM(1, 2, 3, 4));
    return 0;
}

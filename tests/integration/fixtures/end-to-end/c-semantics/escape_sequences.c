/* CX-STDOUT: 34 92 65 65 7 0 */
/* CX-STDOUT-NEXT: a\nAA "q" */

#include <stdio.h>

int main(void) {
    printf("%d %d %d %d %d %d\n", '\"', '\\', '\x41', '\101', '\a', '\0');
    printf("a\\n\x41\101 \"q\"\n");
    return 0;
}

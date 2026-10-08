/* CX-STDOUT: 5 5 1 3 12 */
/* CX-STDOUT-NEXT: 4 */

#include <stdio.h>

#define KEYWORD "self"
#define LITERAL_LENGTH(literal) (sizeof(literal) / sizeof(char) - 1)

int main(void) {
    printf("%d %d %d %d %d\n", (int)sizeof("self"), (int)sizeof(KEYWORD), (int)sizeof(""),
           (int)sizeof("a\n"), (int)sizeof(KEYWORD "\0" "method"));
    printf("%d\n", (int)LITERAL_LENGTH(KEYWORD));
    return 0;
}

/* CX-STDOUT: if */
/* CX-STDOUT-NEXT: for */
/* CX-STDOUT-NEXT: else */

#include <stdio.h>

int main() {
    if (1)
        printf("if\n");
    else
        printf("bad\n");

    int i = 0;
    while (i < 1)
        i++;

    for (int j = 0; j < 1; j++)
        printf("for\n");

    if (1)
        if (0)
            printf("bad\n");
        else
            printf("else\n");
}

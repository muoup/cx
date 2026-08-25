/* CX-STDOUT: Value of x: 10 */

#include <stdio.h>

int x;

void procedure() {
    x = 10;
}

int main() {
    procedure();

    printf("Value of x: %d\n", x);
}

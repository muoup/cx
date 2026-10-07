/* CX-STDOUT: 15 20 */

#include <stdio.h>

int main(void) {
    int until_six = 0;
    int evens = 0;

    for (int i = 0; i < 10; i++) {
        if (i == 6)
            break;
        else {
            until_six += i;
        }
    }

    for (int i = 0; i < 10; i++) {
        if (i % 2)
            continue;
        else
            evens += i;
    }

    printf("%d %d\n", until_six, evens);
    return 0;
}

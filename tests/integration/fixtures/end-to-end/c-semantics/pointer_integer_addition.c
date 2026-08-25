/* CX-STDOUT: pointer integer addition */

#include <stdio.h>

int main(void) {
    int values[2] = {0, 0};
    int *middle = 1 + values;

    if (middle != &values[1]) {
        return 1;
    }

    puts("pointer integer addition");
    return 0;
}

/* CX-STDOUT: 20 */

#include <stdio.h>

int main(void) {
    int value = 2;
    int result = 0;
    switch (value) {
        case 1:
            result = 10;
            break;
        case 2:
            result = 20;
            break;
        default:
            result = 30;
            break;
    }
    printf("%d\n", result);
    return result == 20 ? 0 : 1;
}

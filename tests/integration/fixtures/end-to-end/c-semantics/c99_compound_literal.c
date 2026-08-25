/* CX-STDOUT: 12 */

#include <stdio.h>

struct Point {
    int x;
    int y;
};

int main(void) {
    struct Point point = (struct Point){ .y = 8, .x = 4 };
    int result = point.x + point.y;

    printf("%d\n", result);
    return result == 12 ? 0 : 1;
}

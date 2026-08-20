#include <stdio.h>

struct Point {
    int x;
    int y;
};

struct Point points[] = {
    { 1, 2 },
    { 3, 4 },
    { .y = 6, .x = 5 }
};

struct Point nested = { .x = 7 };

int main(void) {
    printf("%d %d %d %d\n", points[0].x, points[1].y, points[2].y, nested.y);
    return points[0].x == 1 && points[1].y == 4 && points[2].y == 6 && nested.y == 0 ? 0 : 1;
}

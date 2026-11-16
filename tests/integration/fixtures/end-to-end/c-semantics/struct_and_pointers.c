#include <stdio.h>

struct Point {
    int x;
    int y;
};

void print_point(struct Point* p) {
    printf("Point(%d, %d)\n", p->x, p->y);
}

int main() {
    struct Point p1;
    p1.x = 10;
    p1.y = 20;

    print_point(&p1);

    struct Point* p2 = &p1;
    p2->x = 30;

    print_point(&p1);
    print_point(p2);

    return 0;
}
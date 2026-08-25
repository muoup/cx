#include <stdio.h>

struct color {
    int value;
};

#define STORE(xx, yy, cc) fb[(yy)*f_w+(xx)]=(cc)

void draw(unsigned char *fb, int f_w, int color) {
    int x = 1;
    int y = 0;
    STORE(x, y, color);
}

int main() {
    unsigned char values[4];
    draw(values, 4, 7);
    printf("%d\n", values[1]);
}

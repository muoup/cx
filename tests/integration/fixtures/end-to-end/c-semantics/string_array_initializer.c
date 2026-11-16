#include <stdio.h>

struct Banner {
    char text[4];
};

struct Banner banner = { "cx" };

int main(void) {
    printf("%s\n", banner.text);
    return banner.text[0] == 'c' && banner.text[1] == 'x' && banner.text[2] == 0 && banner.text[3] == 0 ? 0 : 1;
}

#include <stdio.h>

int counter() {
    static int i = 0;

    return i++;
}

int main() {
    for (int i = 0; i < 10; i++) {
        printf("%d\n", counter());
    }
}
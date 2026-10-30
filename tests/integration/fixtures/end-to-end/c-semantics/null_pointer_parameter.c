#include <stdio.h>

void inspect(char *value) {
    printf("%d\n", value == 0);
}

int main() {
    inspect(0);
    return 0;
}

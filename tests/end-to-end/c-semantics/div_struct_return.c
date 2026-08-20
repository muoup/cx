#include <stdio.h>
#include <stdlib.h>

int main() {
    div_t result = div(17, 5);
    printf("%d %d\n", result.quot, result.rem);
    return 0;
}

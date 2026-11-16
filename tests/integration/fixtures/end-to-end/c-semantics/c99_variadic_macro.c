#include <stdio.h>

#define ADD(first, ...) ((first) + (__VA_ARGS__))
#define VALUE(...) __VA_ARGS__
#define PRINT(...) printf(__VA_ARGS__)

int main(void) {
    int result = ADD(1, 2, 3) + VALUE(4);

    PRINT("%d\n", result);
    return result == 8 ? 0 : 1;
}

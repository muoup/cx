#include <stdio.h>

struct Pair {
    int first;
    int second;
};

int main(void) {
    // C99 spelling for the existing boolean representation.
    _Bool enabled = 1;
    struct Pair values = { .second = 9, .first = 3 };
    int total = 0;

    for (int i = 0; i < 2; i++) {
        total += values.first + values.second;
    }

    printf("%d %d %d\n", enabled, values.first, total);
    return enabled && total == 24 ? 0 : 1;
}

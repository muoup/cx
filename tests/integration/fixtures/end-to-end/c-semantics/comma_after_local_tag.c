#include <stdio.h>

static int local_sequence(void) {
    struct Alignment {
        char c;
        union {
            long integer;
            double real;
        } value;
    };
    int i = 0;
    while ((void)++i, i < 3) {
    }
    return i;
}

static int later_sequence(void) {
    int i = 0;
    i = 1, i += 2;
    return i;
}

int main(void) {
    printf("%d %d\n", local_sequence(), later_sequence());
    return 0;
}

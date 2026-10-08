/* CX-STDOUT: 1 2 16 */

#include <stdio.h>

struct tagged {
    int kind;
    union {
        int as_int;
        double as_double;
        struct {
            short lo;
            short hi;
        };
    };
};

int main(void) {
    struct tagged value;
    struct tagged *pointer = &value;

    value.kind = 1;
    value.as_int = 0x00020001;

    printf("%d %d %d\n", pointer->lo, value.hi, (int)sizeof(value));
    return 0;
}

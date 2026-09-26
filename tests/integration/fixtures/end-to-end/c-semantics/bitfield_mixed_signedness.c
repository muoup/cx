/* CX-STDOUT: size 4 */
/* CX-STDOUT-NEXT: local 5 -3 17 */
/* CX-STDOUT-NEXT: global 6 -8 31 */

#include <stdio.h>

struct Mixed {
    unsigned int a : 3;
    int b : 4;
    unsigned int c : 5;
};

struct Mixed global_mixed = { 6, -8, 31 };

int main() {
    printf("size %d\n", (int)sizeof(struct Mixed));

    struct Mixed local;
    local.a = 13;
    local.b = -3;
    local.c = 17;
    printf("local %d %d %d\n", local.a, local.b, local.c);

    printf("global %d %d %d\n", global_mixed.a, global_mixed.b, global_mixed.c);
    return 0;
}

/* CX-STDOUT: 23 8 4096 */

#include <stdio.h>

int main(void) {
    int i16 = 7;
    long unsigned wide = 4096;
    __SIZE_TYPE__ size = sizeof(long long unsigned int);

    i16 += 16;

    printf("%d %d %lu\n", i16, (int)size, wide);
    return 0;
}

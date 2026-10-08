/* CX-STDOUT: 2 2 2 2 */
/* CX-STDOUT-NEXT: 30 40 */
/* CX-STDOUT-NEXT: 65535 */

#include <stdio.h>

static const unsigned short int table[4] = {10, 20, 30, 40};

int main(void) {
    const unsigned short int *entries = table;
    unsigned short int wrapped = 0;

    printf("%d %d %d %d\n", (int)sizeof(short), (int)sizeof(short int),
           (int)sizeof(unsigned short), (int)sizeof(unsigned short int));
    printf("%d %d\n", entries[2], *(entries + 3));

    wrapped--;
    printf("%d\n", wrapped);
    return 0;
}

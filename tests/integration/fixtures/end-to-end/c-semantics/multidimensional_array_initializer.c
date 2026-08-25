/* CX-STDOUT: one two */

#include <stdio.h>

char names[2][4] = { "one", "two" };

int main(void) {
    printf("%s %s\n", names[0], names[1]);
    return names[0][0] == 'o' && names[1][0] == 't' ? 0 : 1;
}

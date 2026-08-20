#include <stdio.h>

const char *messages[] = { "first", "second" };

int main(void) {
    printf("%s %s\n", messages[0], messages[1]);
    return messages[0][0] == 'f' && messages[1][0] == 's' ? 0 : 1;
}

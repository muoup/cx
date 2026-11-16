#include <ctype.h>
#include <stdio.h>

int main(void) {
    const char *characters = "a7%";

    for (; *characters != '\0'; characters++) {
        unsigned char character = (unsigned char)*characters;
        printf("alpha=%d digit=%d punct=%d\n", isalpha(character) != 0, isdigit(character) != 0,
               ispunct(character) != 0);
    }
    return 0;
}

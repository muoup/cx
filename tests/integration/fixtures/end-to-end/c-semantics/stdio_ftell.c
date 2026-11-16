#include <stdio.h>

int main(void) {
    FILE *stream = fopen("/dev/null", "r");
    if (stream == 0) {
        return 1;
    }

    int result = ftell(stream) == 0 ? 0 : 1;
    fclose(stream);
    return result;
}

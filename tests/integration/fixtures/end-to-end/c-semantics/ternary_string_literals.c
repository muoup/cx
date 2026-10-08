/* CX-STDOUT: version none */

#include <stdio.h>

int main(int argc, char **argv) {
    const char *first = argc ? "version" : "none";

    printf("%s %s\n", first, argc ? "none" : "version");
    return 0;
}

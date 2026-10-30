#include <stddef.h>
#include <stdio.h>

static int *value = NULL;

int main() {
    printf("%d\n", value == NULL);
}

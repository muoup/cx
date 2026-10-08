#include <stdio.h>

#define INTERNAL static
#include "_inherited_function_linkage.h"

extern int inherited_plain(int value);

int inherited_plain(int value) {
    return value + 1;
}

extern int inherited_extern(int value) {
    return value + 2;
}

static int inherited_recursive(int value) {
    extern int inherited_recursive(int value);
    if (value == 0) {
        return 0;
    }
    return 1 + inherited_recursive(value - 1);
}

int main(void) {
    int inherited_plain(int value);
    extern int inherited_extern(int value);
    int (*plain_pointer)(int) = inherited_plain;
    int (*extern_pointer)(int) = inherited_extern;
    printf("%d %d %d\n", plain_pointer(6), extern_pointer(7), inherited_recursive(4));
    return 0;
}

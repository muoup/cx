#include <stddef.h>

struct Incomplete;

int main(void) {
    return (int)offsetof(struct Incomplete, value);
}

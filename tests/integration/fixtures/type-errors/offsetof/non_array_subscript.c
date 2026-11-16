#include <stddef.h>

struct HasScalar {
    int value;
};

int main(void) {
    return (int)offsetof(struct HasScalar, value[0]);
}

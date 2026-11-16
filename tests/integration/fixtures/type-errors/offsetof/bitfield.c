#include <stddef.h>

struct Bitfields {
    unsigned int flag : 1;
};

int main(void) {
    return (int)offsetof(struct Bitfields, flag);
}

#include <stddef.h>

struct HasMember {
    int value;
};

int main(void) {
    return (int)offsetof(struct HasMember, missing);
}

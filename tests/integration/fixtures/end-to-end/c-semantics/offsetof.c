/* CX-STDOUT: 0 8 16 */

#include <stddef.h>
#include <stdio.h>

struct Padded {
    char tag;
    union {
        double number;
        void *pointer;
    } payload;
    int trailing;
};

int main(void) {
    const size_t payload = offsetof(struct Padded, payload);

    printf("%d %d %d\n", (int)offsetof(struct Padded, tag), (int)payload,
           (int)__builtin_offsetof(struct Padded, trailing));
    return 0;
}

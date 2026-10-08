/* CX-STDOUT: 14 13 2 12 7 */

#include <stdio.h>

static const struct {
    unsigned char left;
    unsigned char right;
} priority[] = {
    {10, 10},
    {14, 13},
};

static struct {
    int first;
    int second;
} pair = {1, 2};

int main(void) {
    struct {
        unsigned char left;
        unsigned char right;
    } local = {10, 12};
    static struct {
        int value;
    } counter = {7};

    printf("%d %d %d %d %d\n", priority[1].left, priority[1].right, pair.second, local.right,
           counter.value);
    return 0;
}

/* CX-STDOUT: 3 4 */

#include <stdio.h>

struct Pair {
    int left;
    int right;
};

static struct Pair pairs[] = {
    { 1, 2 },
    { 3, 4 },
    { 5, 6 }
};
static struct Pair *selected = pairs + 1;

int main(void) {
    printf("%d %d\n", selected->left, selected->right);
    return selected->left == 3 && selected->right == 4 ? 0 : 1;
}

/* CX-STDOUT: 3 */

#include <stdio.h>

struct item {
    int value;
};

static struct item items[] = {
    { 0 },
    { 0 },
    { 0 },
};
static int item_count = sizeof(items) / sizeof(*items);

int main(void) {
    printf("%d\n", item_count);
    return 0;
}

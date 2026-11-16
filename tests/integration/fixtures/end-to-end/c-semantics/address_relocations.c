#include <stdio.h>

struct Pair { int first; int second; };
struct Pair pair = { 7, 11 };
int *second = &pair.second;
void *object = &pair;

int plus_one(int value) { return value + 1; }
int (*callback)(int) = &plus_one;

int main(void) {
    int local;
    int *address = &local;
    *address = *second;
    printf("%d %d %d\n", local, ((struct Pair *)object)->first, callback(local));
    return 0;
}

#include <stdio.h>

struct Large {
    int a;
    int b;
    int c;
    int d;
    int e;
    int f;
};

struct Padded {
    int a;
    int b;
    int c;
    int d;
    int e;
};

void inspect(struct Large large, struct Padded padded) {
    printf("%d %d %d %d %d %d\n", large.a, large.b, large.c, large.d, large.e, large.f);
    printf("%d %d %d %d %d\n", padded.a, padded.b, padded.c, padded.d, padded.e);
}

int main() {
    struct Large large = {1, 2, 3, 4, 5, 6};
    struct Padded padded = {7, 8, 9, 10, 11};
    inspect(large, padded);
    return 0;
}

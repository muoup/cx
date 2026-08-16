#include <stdio.h>

struct Inner {
    int a;
    int b;
};

struct Test {
    struct Inner inner;
    int c;
    int d;
};

void pass_by_reference(struct Test* t);
void pass_by_value(struct Test t, int c);
struct Test struct_return(void);

int main() {
    struct Test t = struct_return();

    pass_by_reference(&t);
    pass_by_value(t, 1);
    return 0;
}

void pass_by_reference(struct Test* t) {
    printf("Pass By Reference:\n");
    printf("t.inner.a: %d\n", t->inner.a);
    printf("t.inner.b: %d\n", t->inner.b);
    printf("t.c: %d\n", t->c);
    printf("t.d: %d\n", t->d);
}

void pass_by_value(struct Test t, int c) {
    printf("Pass By Value:\n");
    printf("t.inner.a: %d\n", t.inner.a);
    printf("t.inner.b: %d\n", t.inner.b);
    printf("t.c: %d\n", t.c);
    printf("t.d: %d\n", t.d);
}

struct Test struct_return(void) {
    return (struct Test) {
        .inner = {
            .a = 1,
            .b = 2
        },
        .c = 3,
        .d = 4
    };
}

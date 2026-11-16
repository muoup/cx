struct S {
    int length;
    int data[];
};

union U {
    struct S inner;
    int raw;
};

struct T {
    union U wrapped;
};

int main() {
    return 0;
}

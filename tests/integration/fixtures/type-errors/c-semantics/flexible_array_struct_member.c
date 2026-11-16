struct S {
    int length;
    int data[];
};

struct T {
    struct S inner;
    int after;
};

int main() {
    return 0;
}

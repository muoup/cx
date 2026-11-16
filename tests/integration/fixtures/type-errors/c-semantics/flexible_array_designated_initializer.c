struct S {
    int length;
    int data[];
};

int main() {
    struct S value = { .data = { 2 } };
    return value.length;
}

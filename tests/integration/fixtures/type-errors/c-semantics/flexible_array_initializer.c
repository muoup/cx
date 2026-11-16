struct S {
    int length;
    int data[];
};

int main() {
    struct S value = { 1, { 2 } };
    return value.length;
}

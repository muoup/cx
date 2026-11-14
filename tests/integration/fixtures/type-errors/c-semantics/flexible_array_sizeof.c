struct S {
    int length;
    int data[];
};

int main() {
    struct S* value = 0;
    return sizeof(value->data);
}

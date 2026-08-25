struct Value;

enum Value {
    ValueA,
};

int main(void) {
    struct Value *value;
    return value == 0 ? 0 : 1;
}

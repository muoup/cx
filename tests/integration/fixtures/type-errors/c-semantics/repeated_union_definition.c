union Value {
    int integer;
};

union Value {
    int integer;
};

int main(void) {
    union Value value = { .integer = 1 };
    return value.integer;
}

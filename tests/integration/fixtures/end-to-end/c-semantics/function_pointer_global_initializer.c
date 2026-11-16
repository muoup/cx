int add_one(int value) {
    return value + 1;
}

int (*operation)(int) = add_one;

int main(void) {
    return operation(4) == 5 ? 0 : 1;
}

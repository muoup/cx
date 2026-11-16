int *identity(int *value) {
    return value;
}

int main(void) {
    return identity(0) == 0 ? 0 : 1;
}

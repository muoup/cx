struct stat {
    int value;
};

int stat(struct stat *value) {
    return value->value;
}

int main(void) {
    struct stat value = { 1 };
    return stat(&value) == 1 ? 0 : 1;
}

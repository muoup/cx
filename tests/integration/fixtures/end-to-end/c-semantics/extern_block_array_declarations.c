typedef struct {
    int value;
} item;

item values[] = {{1}, {2}};

int declarations_only(void) {
    extern int unrelated[];
    extern int unrelated[3];
    return 0;
}

int main(void) {
    extern item values[];
    extern item values[2];
    return values[1].value - 2 + declarations_only();
}

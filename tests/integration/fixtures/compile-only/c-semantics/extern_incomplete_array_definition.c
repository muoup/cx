typedef struct {
    int value;
} item;

extern item values[];

item values[] = {
    { 1 },
};

int main(void) {
    return values[0].value - 1;
}

typedef struct {
    int value;
} item;

extern item values[1];

item values[1] = {
    { 1 },
};

int main(void) {
    return values[0].value - 1;
}

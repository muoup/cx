struct point {
    int value;
};

typedef struct point point;

int main(void) {
    point value = { 3 };
    return value.value == 3 ? 0 : 1;
}

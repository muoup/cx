typedef struct opaque opaque;

opaque *value;

int main(void) {
    return value == 0 ? 0 : 1;
}

int values[3];

int main(void) {
    int *current = values;
    current += 2;
    return current - values == 2 ? 0 : 1;
}

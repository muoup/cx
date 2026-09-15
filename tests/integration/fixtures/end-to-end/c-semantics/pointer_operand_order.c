int values[4] = {3, 5, 7, 11};
int order = 0;

int offset(void) {
    order = order * 10 + 1;
    return 1;
}

int *pointer(void) {
    order = order * 10 + 2;
    return values;
}

int main(void) {
    int *first = offset() + pointer();
    if (order != 12 || first != &values[1]) return 1;
    order = 0;
    int *second = pointer() + offset();
    if (order != 21 || second != &values[1]) return 2;
    int index = 1;
    int *third = index + (index = 2, values);
    if (third != &values[1] || index != 2) return 3;
    int *base = values;
    int *fourth = base + (base = values + 2, 1);
    if (fourth != &values[1] || base != &values[2]) return 4;
    return 0;
}

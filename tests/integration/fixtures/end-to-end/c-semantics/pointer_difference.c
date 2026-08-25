int values[3];

int main(void) {
    int *first = &values[0];
    int *last = &values[2];
    return last - first == 2 ? 0 : 1;
}

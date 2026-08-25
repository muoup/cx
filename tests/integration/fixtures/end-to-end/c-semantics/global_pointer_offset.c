int values[3];
int *middle = &values[1];

int main(void) {
    values[1] = 7;
    return *middle == 7 ? 0 : 1;
}

/* CX-STDOUT: */

int values[2];

int main(void) {
    values[0] = 1;
    values[0]++;

    int *current = values;
    current++;

    return values[0] == 2 && current - values == 1 ? 0 : 1;
}

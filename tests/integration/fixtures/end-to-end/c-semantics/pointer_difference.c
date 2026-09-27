/* CX-STDOUT: */

int values[3];
int initial_difference = &values[2] - &values[0];

struct Wide {
    int first;
    int second;
    int third;
};

struct Wide wide_values[4];

int main(void) {
    if (initial_difference != 2) return 5;
    int *first = &values[0];
    int *last = &values[2];
    if (last - first != 2) return 1;
    if (first - last != -2) return 2;

    struct Wide *wide_first = &wide_values[0];
    struct Wide *wide_last = &wide_values[3];
    if (wide_last - wide_first != 3) return 3;
    if (wide_first - wide_last != -3) return 4;

    char bytes[8];
    char *byte_first = &bytes[1];
    char *byte_last = &bytes[6];
    if (byte_last - byte_first != 5) return 6;
    if (byte_first - byte_last != -5) return 7;

    return 0;
}

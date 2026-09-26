int values[4] = {3, 5, 7, 11};
int *element = &values[2];
char *suffix = "abcd" + 2;
char *empty = 0;
char letters[] = "xyz";

int *select_element(int take) {
    return take ? element : 0;
}

int main(void) {
    if (*element != 7 || element - values != 2) return 1;
    if (suffix[0] != 'c' || suffix[1] != 'd' || suffix[2] != 0) return 2;
    if (empty != 0 || select_element(0) != 0 || select_element(1) != &values[2]) return 3;
    if (letters[0] != 'x' || letters[2] != 'z' || letters[3] != 0) return 4;
    return 0;
}

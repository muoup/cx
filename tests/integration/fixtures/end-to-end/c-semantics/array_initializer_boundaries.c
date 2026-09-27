char exact[2] = "cx";
char padded[4] = "cx";
char inferred[] = "xyz";
int *pointer = { 0 };

int main(void) {
    int local[] = { 7, 8 };
    return exact[0] == 'c' && exact[1] == 'x'
        && padded[0] == 'c' && padded[1] == 'x'
        && padded[2] == 0 && padded[3] == 0
        && inferred[0] == 'x' && inferred[2] == 'z' && inferred[3] == 0
        && sizeof(inferred) == 4
        && local[0] == 7 && local[1] == 8
        && sizeof(local) == 2 * sizeof(int)
        && pointer == 0 ? 0 : 1;
}

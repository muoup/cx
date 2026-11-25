int *pointer, scalar;
int *initialized = &scalar, value = 7, values[2] = {11, 13}, *other = &value;

struct pair {
    int *pointer, scalar;
};

int main(void) {
    int *local = &scalar, selected = 1 ? 2, 3 : 4, *chosen = &selected;
    struct pair p = {local, 5};
    scalar = p.scalar;
    pointer = p.pointer;
    return *pointer + *initialized + *other + values[1] + *chosen == 33 ? 0 : 1;
}

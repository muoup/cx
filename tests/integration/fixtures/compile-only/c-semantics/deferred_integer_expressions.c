enum {
    First = 2 + 3,
    Second
};

int values[First + Second];

int main(void) {
    int selected = 0;
    switch (First) {
        case 1 + 4:
            selected = 1;
            break;
        default:
            selected = 2;
            break;
    }
    return selected == 1 && Second == 6 ? 0 : 1;
}

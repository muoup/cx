/* CX-STDOUT: */

int add_one(int value) {
    return value + 1;
}

int add_two(int value) {
    return value + 2;
}

int (*operations[])(int) = {add_one, add_two};

int main(void) {
    return (*operations[1])(3) == 5 ? 0 : 1;
}

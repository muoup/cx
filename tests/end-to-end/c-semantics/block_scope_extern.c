int shared_value = 0;

int main(void) {
    extern int shared_value;

    shared_value = 7;
    return shared_value == 7 ? 0 : 1;
}

int main(void) {
    static const int values[] = { 1 };
    values[0] = 2;
    return values[0];
}

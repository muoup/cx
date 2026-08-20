int main(void) {
    goto inside;
    return 1;
    if (0) {
    inside:
        return 0;
    }
    return 1;
}

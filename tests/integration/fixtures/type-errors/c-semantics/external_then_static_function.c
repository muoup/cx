int conflicting_linkage(int value);

static int conflicting_linkage(int value) {
    return value;
}

int main(void) {
    return conflicting_linkage(0);
}

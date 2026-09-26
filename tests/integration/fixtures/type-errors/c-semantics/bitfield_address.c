struct Flags {
    unsigned int low : 3;
    unsigned int high : 5;
};

int main() {
    struct Flags flags;
    unsigned int *low = &flags.low;
    return (int)*low;
}

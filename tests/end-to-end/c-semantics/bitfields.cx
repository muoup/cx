int printf(char* fmt, ...);

struct Flags {
    u32 low : 3;
    u32 high : 5;
    int value;
};

union Bits {
    u32 low : 3;
    u32 all;
};

int main() {
    struct Flags flags;
    flags.low = 13;
    flags.high = 17;
    flags.value = 99;

    printf("%d %d %d\n", flags.low, flags.high, flags.value);

    union Bits bits;
    bits.all = 0;
    bits.low = 15;

    printf("%d %d\n", bits.low, bits.all);

    return 0;
}

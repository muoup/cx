struct __attribute__((packed)) packed_pair {
    char tag;
    int value;
} __attribute__((aligned(4)));

int format_like(const char *format, ...) __attribute__((format(printf, 1, 2), __nonnull__(1)));

__attribute__((unused)) static int unused_helper(int value) {
    return value;
}

int attribute_names_inside_arguments(void) __attribute__((alias("noreturn"), deprecated("noreturn")));

int returns_normally(void) {
    return unused_helper(1);
}

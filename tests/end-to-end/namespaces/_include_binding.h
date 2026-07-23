int puts(const char* text);

typedef int included_int;

static int call_header_puts() {
    return puts("included module");
}

static int included_parameter(int puts) {
    return puts;
}

T selected<T>(T value) {
    return value;
}

#include <stdarg.h>
#include <stdio.h>

int sum(int count, ...) {
    va_list args;
    va_start(args, count);
    int total = 0;
    for (int i = 0; i < count; i++)
        total += va_arg(args, int);
    va_end(args);
    return total;
}

void mixed(const char* label, ...) {
    va_list args;
    va_start(args, label);
    int i = va_arg(args, int);
    double d = va_arg(args, double);
    char c = (char) va_arg(args, int);
    va_end(args);
    printf("%s %d %.1f %c\n", label, i, d, c);
}

void format(char* buffer, unsigned long size, const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vsnprintf(buffer, size, fmt, args);
    va_end(args);
}

int main() {
    printf("sum %d\n", sum(4, 1, 2, 3, 4));
    mixed("mixed", 7, 2.5, 'x');

    char buffer[32];
    format(buffer, sizeof(buffer), "%d-%s-%.1f", 3, "four", 5.0);
    printf("format %s\n", buffer);
    return 0;
}

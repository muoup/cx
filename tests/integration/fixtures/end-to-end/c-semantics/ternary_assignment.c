#include <stdio.h>

int main(void) {
    int value = 0;

    value = value < 16 ? value + 1 : 8;
    int first = value;

    value = 16;
    value = value < 16 ? value + 1 : 8;

    int assigned_condition = 0;
    int parenthesized = (assigned_condition = 1) ? 7 : 9;

    printf("%d %d %d %d\n", first, value, assigned_condition, parenthesized);
    return first == 1 && value == 8 && assigned_condition == 1 && parenthesized == 7 ? 0 : 1;
}

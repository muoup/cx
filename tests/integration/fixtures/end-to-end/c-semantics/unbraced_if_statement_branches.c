/* CX-STDOUT: 2 2 */
/* CX-STDOUT-NEXT: 0 1 */

int printf(const char* fmt, ...);

int main() {
    int values[4] = {0};
    int* p = values;
    int b = 2;
    if (b != 0)
        p = values + b;
    else
        b = (int) (p - values);
    printf("%d %d\n", (int) (p - values), b);

    b = 0;
    if (b != 0)
        p = values + 1;
    else
        b = (int) (p - values) - 1;
    if (b)
        p = values;
    printf("%d %d\n", (int) (p - values), b);
    return 0;
}

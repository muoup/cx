int printf(char* fmt, ...);

int main() {
    float x = 1.5;
    int y = 2;
    printf("%.1f %d\n", x, y < 3);
    return 0;
}

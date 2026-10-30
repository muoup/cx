int printf(const char* fmt, ...);

int main() {
    unsigned endian;
    endian = 1;
    if (*(unsigned char*)&endian)
        printf("little\n");
    else
        printf("big\n");

    unsigned x = 0x11223344u;
    unsigned char c = *(unsigned char*)&x;
    printf("%x %x\n", c, *(unsigned char*)&x);

    unsigned char* bytes = (unsigned char*)&x;
    printf("%x %x %x\n", bytes[0], bytes[1], ((unsigned char*)&x)[2]);

    *(unsigned char*)&x = 0xff;
    printf("%x\n", x);
    return 0;
}

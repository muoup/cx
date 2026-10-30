void declaration_only(void) {
}

int main(void) {
    extern void declaration_only(void);
    int in;
    in = 0;
    declaration_only();
    return in;
}

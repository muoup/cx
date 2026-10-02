typedef struct s s_t;
struct s {
    int value;
    s_t inner;
};

int main(void) {
    s_t value;
    value.value = 1;
    return value.value;
}

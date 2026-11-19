typedef char T;

static int multiply(int T, int n) {
    T * n;
    return T * n;
}

static int increment(int n) {
    return n + 1;
}

static int apply(int (*T)(int)) {
    return (T)(2);
}

int main(void) {
    int sum = 0;
    {
        int T = sizeof(T);
        sum += T;
    }
    for (int T = 0; T < 3; ++T) {
        sum += T;
    }
    T character = 0;
    return multiply(3, 2) != 6 || apply(increment) != 3
        || sum != sizeof(int) + 3 || sizeof(character) != 1;
}

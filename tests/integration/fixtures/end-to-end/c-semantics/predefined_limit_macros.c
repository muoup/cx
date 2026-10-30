#include <float.h>
#include <limits.h>
#include <stdio.h>

int main(void) {
    double nan = __builtin_nan("");

    printf("%d %lld %d %d\n", INT_MAX, LLONG_MAX, DBL_EPSILON < 1e-15, nan != nan);
    return 0;
}

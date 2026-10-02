/* CX-STDOUT: 0 1 10 11 12 12 13 */
/* CX-STDOUT-NEXT: 999 1000 */

#include <stdio.h>

enum {
    A,
    B,
    C = 10,
    D,
    E,
    F = C + 2,
    G
};

#define T10(p) p##0, p##1, p##2, p##3, p##4, p##5, p##6, p##7, p##8, p##9
#define T100(p) T10(p##0), T10(p##1), T10(p##2), T10(p##3), T10(p##4), \
                T10(p##5), T10(p##6), T10(p##7), T10(p##8), T10(p##9)
#define T1000(p) T100(p##0), T100(p##1), T100(p##2), T100(p##3), T100(p##4), \
                 T100(p##5), T100(p##6), T100(p##7), T100(p##8), T100(p##9)

enum { T1000(S_), S_LAST };

int main() {
    printf("%d %d %d %d %d %d %d\n", A, B, C, D, E, F, G);
    printf("%d %d\n", S_999, S_LAST);
}

/* CX-STDOUT: 4 1 5 | 4 6 9 | 4 -1 q | 8 7 123456789 */
/* CX-STDOUT-NEXT: 8 777777 555555555555 | 5 | 4 -4 61 -200 */
/* CX-STDOUT-NEXT: 5 9 | 1000000 1099511627775 | -3 -50 200 */

#include <stdio.h>

struct A { _Bool x : 1; unsigned int y : 3; };
struct B { unsigned char x : 3; unsigned int y : 4; };
struct C { int x : 1; char c; };
struct X { char a; unsigned int y : 30; };
struct L { unsigned int a : 20; unsigned long long b : 40; };
struct Z { char c; int : 0; char d; };
struct S { signed char s : 3; int t : 7; short u : 9; };
struct B gb = { 5, 9 };
struct L gl = { 1000000, 1099511627775ULL };
struct S gs = { -3, -50, 200 };

int main() {
    struct A a = { .x = 1, .y = 5 };
    struct B b = { .x = 6, .y = 9 };
    struct C c = { .x = -1, .c = 'q' };
    struct X x = { .a = 7, .y = 123456789 };
    struct L l = { .a = 777777, .b = 555555555555ULL };
    struct S s = { .s = -4, .t = 60, .u = -200 };
    
    s.t = s.t + 1;
    
    printf("%d %d %d | %d %d %d | %d %d %c | %d %d %u\n", (int)sizeof(struct A), (int)a.x, (int)a.y,
        (int)sizeof(struct B), (int)b.x, (int)b.y, (int)sizeof(struct C), (int)c.x, c.c,
        (int)sizeof(struct X), (int)x.a, (unsigned)x.y);
    printf("%d %u %llu | %d | %d %d %d %d\n", (int)sizeof(struct L), (unsigned)l.a, (unsigned long long)l.b,
        (int)sizeof(struct Z), (int)sizeof(struct S), (int)s.s, (int)s.t, (int)s.u);
    printf("%d %d | %u %llu | %d %d %d\n", (int)gb.x, (int)gb.y, (unsigned)gl.a, (unsigned long long)gl.b,
        (int)gs.s, (int)gs.t, (int)gs.u);
    
    return 0;
}

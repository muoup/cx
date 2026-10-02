/* CX-STDOUT: 1 2 4 3 1 */

#include <stdio.h>

typedef struct list_s* list_ptr;
struct list_s {
    int value;
    list_ptr next;
};

typedef struct a_s a_t;
typedef struct b_s b_t;
struct a_s { b_t* other; int x; };
struct b_s { a_t* other; int y; };

typedef struct thinker_s {
    struct thinker_s* prev;
    void (*function)(struct thinker_s*);
} thinker_t;

static int ticked;
static void tick(thinker_t* t) { ticked = t->prev == NULL; }

int main(void) {
    list_ptr head = NULL;
    struct list_s n2 = { 2, head };
    struct list_s n1 = { 1, &n2 };
    a_t a;
    b_t b;
    a.other = &b;
    b.other = &a;
    a.x = 3;
    b.y = 4;
    thinker_t t = { NULL, tick };
    t.function(&t);
    printf("%d %d %d %d %d\n", n1.value, n1.next->value, a.other->y, b.other->x, ticked);
    return 0;
}

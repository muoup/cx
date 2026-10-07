/* CX-STDOUT: 11 5 */

#include <stdio.h>

typedef struct Thread Thread;

struct Thread {
    int status;
    struct Global *global;
};

typedef struct Extended {
    char extra[8];
    Thread thread;
} Extended;

typedef struct Global {
    int seed;
    Extended main_thread;
} Global;

typedef struct Node Node;

struct Node {
    int value;
    Node *next;
};

static int status(Thread *thread) { return thread->status + thread->global->seed; }

int main(void) {
    Global global;
    global.seed = 9;
    global.main_thread.thread.status = 2;
    global.main_thread.thread.global = &global;

    struct Node node;
    node.value = 5;
    node.next = &node;

    printf("%d %d\n", status(&global.main_thread.thread), node.next->next->value);
    return 0;
}

/* CX-STDOUT: 12 */

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

int status(Thread *thread) { return thread->status + thread->global->seed; }

int main(void) {
    Thread thread;
    Global global;
    thread.status = 3;
    thread.global = &global;
    global.seed = 9;

    printf("%d\n", status(&thread));
    return 0;
}

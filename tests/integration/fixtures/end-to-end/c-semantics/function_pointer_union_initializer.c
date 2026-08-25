/* CX-STDOUT: */

typedef void (*actionf_v)(void);
typedef void (*actionf_p2)(void *, void *);

typedef union {
    actionf_p2 acp2;
    actionf_v acv;
} actionf_t;

typedef struct {
    int sprite;
    int frame;
    int tics;
    actionf_t action;
    int nextstate;
} state_t;

int called;

void action(void *first, void *second) {
    called = first == 0 && second == 0;
}

state_t states[1] = {
    {0, 0, 1, {action}, 0}
};

int main(void) {
    states[0].action.acp2(0, 0);
    return called == 1 ? 0 : 1;
}

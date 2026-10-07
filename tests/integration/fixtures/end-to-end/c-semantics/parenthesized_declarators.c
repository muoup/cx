/* CX-STDOUT: 12 9 30 */

#include <stdio.h>

typedef struct State State;
typedef int (*Callback)(State *state, int *value);

struct State {
    int base;
    Callback callback;
};

extern int (scale)(int value, int factor);
static int *(pick)(int *first, int *second, int which);

int (scale)(int value, int factor) { return value * factor; }

static int *(pick)(int *first, int *second, int which) { return which ? second : first; }

static int add_base(State *state, int *value) { return state->base + *value; }

int main(void) {
    int low = 4;
    int high = 9;
    State state;
    state.base = 21;
    state.callback = add_base;

    printf("%d %d %d\n", scale(low, 3), *pick(&low, &high, 1), state.callback(&state, &high));
    return 0;
}

/* CX-STDOUT: */

typedef void (*handler_t)(void);

typedef union {
    handler_t handler;
    int value;
} handler_or_value_t;

static handler_or_value_t handlers[] = {{0}};

int main(void) {
    return handlers[0].handler == 0 ? 0 : 1;
}

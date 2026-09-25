void stop(void) __attribute__((__noreturn__));

int calls_stop(void) {
    stop();
}

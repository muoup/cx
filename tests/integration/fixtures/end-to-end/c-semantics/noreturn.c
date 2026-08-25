/* CX-STDOUT: */

_Noreturn void stop_forever(void) {
    while (1) {
    }
}

int value_after_stop(void) {
    stop_forever();
}

int main(void) {
    return 0;
}

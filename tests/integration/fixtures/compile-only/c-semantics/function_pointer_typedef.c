typedef void (*callback_t)(int);

void callback(int value) {}

void use_callback(callback_t value) {
    value = (callback_t)callback;
}

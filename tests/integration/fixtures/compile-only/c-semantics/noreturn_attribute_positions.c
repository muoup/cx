typedef int unreachable;

__attribute__((noreturn)) void prefix_attribute(void);
void __attribute__((__noreturn__)) after_type_attribute(void);
static _Noreturn void after_storage_class(void) {
    while (1) {
    }
}
extern void mixed_attributes(void) __attribute__((unused, noreturn, cold));
extern void (*callback)(void) __attribute__((noreturn));
typedef void callback_type(void) __attribute__((noreturn));

int calls_prefix_attribute(void) {
    prefix_attribute();
}

int calls_after_type_attribute(void) {
    after_type_attribute();
}

int calls_after_storage_class(void) {
    after_storage_class();
}

int calls_mixed_attributes(void) {
    mixed_attributes();
}

int calls_callback(void) {
    callback();
}

int calls_callback_type(callback_type *stop) {
    stop();
}

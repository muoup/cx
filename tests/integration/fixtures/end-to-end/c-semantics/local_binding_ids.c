#include <stdio.h>

struct Resource {
    int data;
};

void drop(struct Resource resource) {
    printf("drop %d\n", resource.data);
}

void shadow_parameter(int value) {
    if (value > 0) {
        int value = 30;
        printf("inner parameter %d\n", value);
    }

    printf("outer parameter %d\n", value);
}

int main() {
    int value = 10;

    if (value > 0) {
        int value = 20;
        printf("inner local %d\n", value);
    }

    printf("outer local %d\n", value);
    shadow_parameter(value);

    struct Resource resource = (struct Resource) {
        .data = 40
    };
    if (value > 0) {
        struct Resource resource = (struct Resource) {
            .data = 50
        };
        drop(resource);
    }
    drop(resource);

    return 0;
}

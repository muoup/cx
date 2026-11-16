#include <stdio.h>

struct Node {
    int value;
    struct Node* next;
};

int pair_sum(struct Node* head) {
    return head->value + head->next->value;
}

int main() {
    struct Node first;
    struct Node second;

    first.value = 1;
    first.next = &second;

    second.value = 2;
    second.next = &first;

    printf("%d\n", pair_sum(&first));
    printf("%d %d %d\n", first.value, first.next->value, first.next->next->value);

    return 0;
}

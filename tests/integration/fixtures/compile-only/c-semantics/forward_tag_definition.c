struct Node *head;

struct Node {
    int value;
    struct Node *next;
};

int main(void) {
    struct Node value = { 1, 0 };
    head = &value;
    return head->value - 1;
}

typedef int callback_t(int x);
typedef int triple_t[3];
typedef const char label_t[4];

struct node;
typedef void visit_t(struct node, struct node *[]);
struct node {
    struct node *next;
    void (*visit)(struct node *, int[]);
    void (*copy)(struct node, visit_t *);
};

int accepts_callback(callback_t callback) {
    return 0;
}

void fill(int *values) {}
void fill_row(triple_t *row) {}
void print(const char *text) {}
void visit(struct node *node, int *values) {}

int main() {
    void (*from_array)(int[]) = fill;
    void (*from_typedef)(triple_t) = fill;
    void (*to_row)(triple_t *) = fill_row;
    void (*to_label)(label_t) = print;
    int (*to_callback)(int (*)(int)) = accepts_callback;
    int (*to_function)(callback_t) = accepts_callback;
    struct node node;
    node.visit = visit;
    return 0;
}

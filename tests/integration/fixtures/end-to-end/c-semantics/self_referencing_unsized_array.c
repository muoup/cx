int printf(const char* fmt, ...);

typedef struct node {
    const char* name;
    struct node* link;
    int value;
} node;

node table[] = {
    { "a", 0, 1 },
    { "b", &table[0], 2 },
    { "c", &table[1], 3 },
};

node self = { "s", &self, 3 };

int main() {
    printf("%s %d %s\n", table[1].link->name, table[1].link->value, table[2].link->name);
    printf("count %d\n", (int) (sizeof(table) / sizeof(table[0])));
    printf("self %s\n", self.link->name);
    return 0;
}

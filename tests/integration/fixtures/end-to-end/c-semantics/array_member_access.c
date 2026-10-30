typedef struct {
    int value;
} item_t;

item_t items[1];

int main(void) {
    items->value = 7;
    return items[0].value == 7 ? 0 : 1;
}

typedef struct Item { int member; } Item;
struct Item *pointer;
Item value;

int read_item(Item item) {
    return item.member;
}

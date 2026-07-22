---
title: Associated Functions
---

# Associated Functions

For cleaner organization, inside of any module an associated function can be created by naming it using `inner_namespace::function` syntax. This declares a function as if it was declared inside of an inner module `inner_namespace`, e.g. if module `mod1::mod2` declares a function `inner::function`, it can be accessed via symbol `mod1::mod2::inner::function`. The intended main application of this is for type-associated functions (see below), but the inner namespace need not correspond to the name of any symbol.

```c
struct counter {
    int value;
};

void counter::print(counter& this) {
    printf("%d\n", this.value);
}

void counter::increment(counter& this) {
    this.value++;
}

int main() {
    counter c = (counter) { .value = 25 };

    c   |> counter::increment()
        |> counter::print();
}
```

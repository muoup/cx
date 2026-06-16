---
title: Associated Functions
---

# Associated Functions

Associated functions are functions declared in a type's namespace with `Type::function` syntax. They are declared separately from the type itself and may be defined anywhere the type is available, including external modules.

When an associated function takes a value of the type as its first parameter, call it with the pipe operator:

```c
struct counter {
    int value;
};

void counter::print(counter& this) {
    printf("%d\n", this.value);
}

int main() {
    counter c = (counter) { .value = 25 };

    c |> counter::print;
}
```

The pipe expression passes the value on the left as the first argument to the function on the right. Additional arguments are written normally after the function name:

```c
value |> Type::function(arg1, arg2);
```

Associated functions are semantically ordinary functions. The pipe form is just a convenient way to keep receiver-style APIs readable without giving `.` a second meaning beyond field access.

## Borrowing and Consuming

If the first parameter is a reference, the pipe call borrows the value:

```c
void counter::print(counter& this) {
    printf("%d\n", this.value);
}

counter c = (counter) { .value = 25 };
c |> counter::print();
```

If the first parameter is taken by value, the caller must pass an owned value. For `@nocopy` and `@nodrop` resources, this usually means moving the value into the call.

```cpp
struct file : @nodrop {
    FILE* fd;
};

void write_to_file(file& file) { ... }

void file::drop(file this) {
    fclose(this.fd);
    @leak(this);
}

void foo(file file) {
    write_to_file(file);
    move file |> file::drop();

    // The binding 'file' is no longer accessible and its resources have been cleaned up.
}
```

This is the idiomatic shape for explicit `drop` functions: resource-owning values are consumed by a cleanup function rather than dropped implicitly.

## Static Associated Functions

If an associated function does not take a value of the type as its first parameter, call it directly with its qualified name. This is useful for factory functions and other type-scoped helpers.

```c
struct int_vector {
    int* data;
    usize length, capacity;
};

int_vector int_vector::create_empty() {
    return (int_vector) { .data = NULL, .length = 0, .capacity = 0 };
}

int_vector int_vector::with_capacity(usize capacity) {
    return (int_vector) { .data = calloc(sizeof(int), capacity), .length = 0, .capacity = capacity };
}

int main() {
    int_vector v1 = int_vector::create_empty();
    int_vector v2 = int_vector::with_capacity(16);

    // ...
}
```

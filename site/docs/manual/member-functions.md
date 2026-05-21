---
title: Member Functions
---

# Member Functions

Member functions, that is functions declared on a type called via `object.method(param)` syntax, is designed to be used liberally as a form of syntactic sugar. Member functions are declared separate from the type declaration itself and may be defined anyway, including in external modules as long as the type is available. They are equivalent semantically to a function taking in the object as its first parameter and provide no unique functionality, they should be used primary for their alternative calling syntax for cleaner APIs.

```c
struct counter {
    int value;
};

void counter::print(*this) {
    printf("%d\n", this.value);
}

int main() {
    counter c = (counter) { .value = 25 };

    c.print();
}
```

In the example above, the member functions is declared with a `*this` self-parameter. This is called a borrowed receiver, the asterisk indicating that the value is passed in as a reference and the function does not own the calling value. If the asterisk is removed, the member function becomes a borrowed receiver in which the receiver object is implicitly moved (see [Move Semantics](move-semantics.md)). This allows for the idiomatic structure of "drop" functions, the alternative to RAII in which a resource-owning object type is cleaned up explicitly.

```cpp
struct file : @nodrop {
    FILE* fd;
};

void write_to_file(file& file) { ... }

void file::drop(this) {
    fclose(this.fd);
    @leak(this);
}

void foo(file file) {
    write_to_file(file);
    file.drop();

    // The variable 'file' is no longer accessible and its resources have been cleaned up.
}

```

## Static Member Functions

There is a third case for member functions. If the 'this' parameter is elided entirely, the member function has no implicit parameter behavior and is defined as a static member function. A static member function is a standard free function existing in the type's namespace, and proves useful for patterns such as factory functions.

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

// ...

int main() {
    int_vector v1 = int_vector::create_empty();
    int_vector v2 = int_vector::with_capacity(16);

    // ...
}

```
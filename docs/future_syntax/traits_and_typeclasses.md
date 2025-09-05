Traits:

```c++

trait Addable {
    Self add(this, Self other);
}

struct Vector {
    int x; int y;
}

// (Potential) Syntax 1:
impl Addable for Vector {
    Vector add(this, Vector other) { ... }
} 

// (Potential) Syntax 2:
Vector add_vectors(Vector *a, Vector b) { ... }

instance Addable for Vector {
    add :: add_vectors;
}

```

The second syntax here seems useful to make typeclass implementations seem more natural.

Typeclasses:

```c++
// Assume the same definitions as above

Vector add_vectors(Vector a, Vector b);

Addable get_addable_object() {
    return instance Addable {
        data :: Vector { x: 1, y : 2 },
        impl :: {
            add = add_vectors
        }
    };
}
```
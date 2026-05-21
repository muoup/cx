---
title: Base Syntax
---

# Base Syntax



## Primitive Types

On top of C's base intrinsic types like `int` and `float`, CX provides a set of fixed-size types for predictable
behavior across different architectures and operating systems:

- `i8`, `i16`, `i32`, `i64`, `i128`: n-bit signed integers
- `u8`, `u16`, `u32`, `u64`, `u128`: n-bit unsigned integers
- `f32`, `f64`: n-bit floating-point values
- `usize`, `isize`: pointer-sized signed/unsigned integer types

## Reference Types

Reference are declared with a `&` type suffix and represents an alternative to C's pointer type with a stronger invariant. A reference type 
T& is non-null and thus does not require explicit dereferencing and may be used as a standard value. 

```c
void increment(int& x) {
    x = x + 1;
}
```

## Match Statements

Match statements are an alternative to C's `switch` statements that forbids fall-throughs. All cases correspond to a single scoped block which
breaks after completion. Cases are denoted with a pattern on the left and a block to the right. Complex patterns are not currently supported,
so in its current form, patterns must be direct matches to an int or float, or a tagged union variant with an optional binding variable.

```cpp
void foo(int i) {
    match (i) {
        1 => printf("i = 1\n");
        5 => printf("i = 5\n");
        _ => {
            printf("Other\n");
            bar(i);
        }
    }
}
```

## Member Access

The `.` and `->` operators from C are interchangeable. If the left-hand side of a `.` operator is a pointer, it will be implicitly dereferenced
before the access is evaluated.

```c
struct Data {
    i32 x;
};

void print_data(Data* data) {
    printf("%d\n", data->x);
    printf("%d\n", data.x);
}
```
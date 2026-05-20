---
title: Base Syntax
---

# Base Syntax

CX is a partially-stable superset of C99. That is, most C99 code will compile as valid CX code
with identical semantics, with more supported as the compiler develops. This page will not provide detailed
explanations for C syntax unless necessary, so it is highly recommended to learn the syntax of the C programming
language if you haven't already.

Recommended Resources:
- [W3Schools' C Programming Tutorial](https://www.w3schools.com/c/) for the basics of the language
- [CPPReference](https://cppreference.com/c/language) for an approachable technical overview of C's semantics
- [The GNU C Reference Manual](https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html) as a precise and verbose authoritative source

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

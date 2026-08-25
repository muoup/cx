---
title: Language Basics
---

# Language Basics

## Primitive Types

On top of C's base intrinsic types like `int` and `float`, CX provides a set of fixed-size types for predictable
behavior across different architectures and operating systems:

- `i8`, `i16`, `i32`, `i64`, `i128`: n-bit signed integers
- `u8`, `u16`, `u32`, `u64`, `u128`: n-bit unsigned integers
- `f32`, `f64`: n-bit floating-point values
- `usize`, `isize`: pointer-sized signed/unsigned integer types

## Reference Types

References are declared with a `&` type suffix and provide an alternative to C's pointer types with a stronger invariant. A reference type
`T&` is non-null, does not require explicit dereferencing, and may be used as a standard value.

```cx
void increment(int& x) {
    x = x + 1;
}
```

References are non-owning views of values. The [Ownership and Moves](./move-semantics.md) chapter explains the distinction between references and owned values, while [Safe Functions and Verification](./safe-functions.md) describes the narrower rules for references in safe code.

## Member Access

The `.` and `->` operators from C are interchangeable. If the left-hand side of a `.` operator is a pointer, it will be implicitly dereferenced
before the access is evaluated.

```cx
struct Data {
    i32 x;
};

void print_data(Data* data) {
    printf("%d\n", data->x);
    printf("%d\n", data.x);
}
```

## String Literals

In C, a string literal is of type `const char*`; CX instead uses an intermediate type `_str&`. A `_str&` can be implicitly cast to `const char*` and has an identical size, so C code using string literals functions the same. The extra type provides stronger guarantees where desired.

A `_str` is an unsized type representing string data that terminates with a null value. Given its unsized nature, it cannot be directly assigned to a variable; it may only be applied as an assertion over an existing string of data. For string literals, that data is in static storage. This distinction is useful because not all values of type `const char*` in idiomatic C are zero-terminated.

Functions similar to those in C's `string.h` can take `const _str&` instead of `const char*` to document that zero termination is required and prevent accidental use of non-zero-terminated character arrays. A zero-terminated buffer annotated as `const char*`, such as a string created at runtime, can be explicitly cast to `_str&` with a C-style cast. This operation is unsafe, and casting a non-zero-terminated value is undefined behavior.

```cx
usize string_length(const _str& value) {
    return strlen(value);
}

usize literal_length = string_length("hello");

const char* runtime_string = create_zero_terminated_string();
usize runtime_length = string_length((const _str&)runtime_string);
```

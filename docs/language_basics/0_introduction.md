# Introduction

This documentation will assume you have an understanding of the C programming language. CX seeks to be fully backward compatible with at least the C99 standard, meaning all C code compilable via `clang -std=c99 [file]` or `gcc -std=c99 [file]` will eventually be compilable using the `cx` compiler.

For easy reference to C language features and what version they were introduced, you may refer to [CPP Reference](https://en.cppreference.com/w/c/language.html). As this language is work in progress, not all C features are yet fully supported, these include but are not limited to:

- Full C Preprocessor Support
- Bit Fields
- Goto
- <stdarg.h\>
    - Calls to libc functions like printf are supported, but defining functions that take in variadic arguments is currently not supported.
- Ternary Operators
- Volatile, Restrict, Inline
- Static Function Variables
- Anonymous Unions

## Small-Scale C Changes

Besides the new syntax that will be introduced in later sections, there are a few small additions to the C syntax not quite important enough for their own page. They are:

1. Sized integer and float types by default:
    - `i8`, `i16`, `i32`, `i64`, `i128`: Signed integers with N bits (equivalent to int, long, long long, etc.)
    - `u8`, `u16`, `u32`, `u64`, `u128`: Unsigned integers with N bits (equivalent to unsigned int, unsigned long, etc.)
    - `f32`, `f64`: Floating-point values with N bits
    - `usize`, `isize`: Signed and unsized variants of a size integer, i.e. the result of sizeof(T)

2. The '->' and '.' operators are now interchangable:

```c

struct Data { i32 x; ... }

void test_routine(Data* data) {
    // Traditional C Syntax -- the `->` operator indicates that we need to dereference data, i.e. this is a
    // short-hand for (*data).x
    printf("data.x: %d\n", data->x);
    
    // Also valid in CX, the compiler recognizes that data is a pointer and automatically dereferences it
    printf("data.x: %d\n", data.x);
}

```

3. Order agnostic evaluation:

```c

// This would be an error in C as `NumericData` and `QualitativeData` are not yet declared in the compilation unit,
// here however it is fine as the compiler preparses before parsing to detect all type symbols. This also would be
// an error as struct types not defined with 'typedef' would need a 'struct'-prefix to its identifier, this also
// has been removed, `typedef struct { ... } [name]` and `struct [name] { ... }` both produce a standalone [name]
// type identifier.
struct Data {
    NumericdData n_data;
    QualitativeData q_data;
};

struct NumericData { ... };
struct QualitativeData { ... };

```

```c

u32 subroutine(u32 x) {
    // This similarly would be invalid in C as the functions are not yet declared at this point
    // of the file. One usually would need to predeclare the methods if they wanted this structure.
    if (x < 256) return (u32) subroutine_2(x);
    else return subroutine_1(x);
}

u32 subroutine_1(u32 x) {
    ...
}

u8 subroutine_2(u8 c) {
    ...
}

```
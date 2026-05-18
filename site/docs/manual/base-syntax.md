---
title: Base Syntax
---

# Base Syntax

CX remains intentionally close to C syntax. It is not order-dependent within a
module: types and functions may be declared in any order, so forward
declarations are generally unnecessary.

## Primitive Aliases

CX provides sized integer and floating-point aliases by default:

- `i8`, `i16`, `i32`, `i64`, `i128`: signed integers
- `u8`, `u16`, `u32`, `u64`, `u128`: unsigned integers
- `f32`, `f64`: floating-point values
- `usize`, `isize`: size integer variants

## Reference Types

Reference types use `&`. A reference is pointer-like, must be non-null, and
points to a single value.

```c
void increment(int& x) {
    x = x + 1;
}
```

## Global Variables

Variables may be declared at module scope:

```c
int counter = 0;
```

## C-Style Enumerations

Plain `enum` declarations define named integer constants:

```c
enum Color {
    Red,
    Green,
    Blue
};
```

Plain enums are distinct from tagged unions declared with `enum union`.

## Member Access

The `.` and `->` operators are interchangeable when the compiler can determine
that the receiver is a pointer.

```c
struct Data {
    i32 x;
};

void print_data(Data* data) {
    printf("%d\n", data->x);
    printf("%d\n", data.x);
}
```

## Lexical Conventions

Names beginning with `_` are reserved for the implementation.

In addition to C keywords, CX reserves:

- `import`
- `template`
- `type`
- `match`
- `is`
- `class`
- `safe`
- `where`
- `move`

CX also uses `@`-prefixed compiler identifiers. These are not ordinary user
identifiers:

- `@nocopy`
- `@nodrop`
- `@unsafe`
- `@leak`
- `@adopt`
- `@unpack`

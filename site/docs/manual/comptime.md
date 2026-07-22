---
title: Comptime and Staged Expressions
---

# Comptime and Staged Expressions

CX provides compile-time functions that can compute values and produce typed runtime code. The latter mechanism uses staged expressions: runtime expressions retained in a typed form so a comptime function can compose them and emit the result at its call site.

Functions that can be evaluated at compile time as well as runtime, similar to C++ `constexpr`, are not yet implemented but are planned for the future. A `comptime` function is different: it must be evaluated during compilation and is not emitted as a runtime function.

## Comptime Functions

A comptime function is declared with `comptime`. Its ordinary parameters are compile-time values, and calling it evaluates the function during compilation:

```c
comptime int add(int lhs, int rhs) {
    return lhs + rhs;
}
```

Comptime functions may also be templated or associated with a namespace using the syntax described in the preceding chapters.

## Staged Expressions

The syntax `expr T` denotes a staged expression that will produce a runtime value of type `T`. It is not a compile-time-known `T`, and the comptime function cannot inspect its eventual runtime value. Instead, the function can place the expression inside other runtime code and return the composed expression.

```c
comptime expr T twice<T>(expr T value) {
    return emit value * 2;
}
```

Here, `value` represents the caller's runtime expression. A call such as `twice(number + 1)` is evaluated at compile time to produce runtime code equivalent to `(number + 1) * 2`.

The `expr` modifier can appear on parameters and the return type. An `expr T` parameter accepts a staged expression of type `T`, while an `expr T` return type requires the comptime function to return an emitted expression producing `T`.

## `emit`

`emit` takes an expression and stages it for use as the result of a comptime function:

```c
comptime expr T add_ten<T>(expr T value) {
    return emit value + 10;
}
```

The emitted expression is lowered in place at the caller. Its runtime operations, ownership effects, and control flow therefore belong to the caller's context. A `return` inside emitted code returns from the runtime function containing the comptime call, rather than from the comptime function that produced the code.

## Staged Block Expressions

The syntax `.{ ... }` creates a `void` block expression in a position where CX expects an expression:

```c
.{
    release(resource);
    log_cleanup();
}
```

This syntax is commonly passed to a parameter of type `expr void`, which retains the block as a staged expression. It is not a closure: it creates no function or captured environment and hides no call. The block remains code from the caller's lexical context and is inserted directly wherever the comptime function emits that parameter.

The leading `.` distinguishes a standalone block expression from a normal scoped body or structured initializer. An empty block is written as `.{}`.

## Optional-Like Control Flow

The standard library uses staged expressions to implement an optional helper similar to Rust's `?` operator. Its definition returns an emitted `match` expression:

```c
comptime expr T opt::try<T>(expr opt<T> self) {
    return emit match (self) {
        opt::some<T>(value) => yield move value;
        opt::none<T>() => return opt::none();
    };
}
```

The pipe call supplies its left operand as the staged `self` argument:

```c
opt<i64> get_option() { ... }

opt<u8> routine() {
    i64 value = get_option()
        |> opt::try();

    ...
}
```

At the use site, the call produces code equivalent to:

```c
i64 value = match (get_option()) {
    opt::some<i64>(value) => yield move value;
    opt::none<i64>() => return opt::none<u8>();
};
```

Because the early return is part of the caller's runtime control flow, it must also satisfy the caller's ownership obligations. `opt::try_or` accepts an additional `expr void` cleanup block for that case:

```c
opt<i64> routine() {
    vector<i64> values = ...;

    i64 number = values
        |> find_specific_number()
        |> opt::try_or(.{
            move values |> vector::drop();
        });

    ...

    move values |> vector::drop();
    return ...;
}
```

The cleanup block is emitted only into the `none` arm. `opt::try` is equivalent to using `opt::try_or(.{})` when no cleanup is required.

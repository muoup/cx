---
title: Functions and Calls
---

# Functions and Calls

CX uses C function declarations and calls, and adds associated function names and pipe-style calls for organizing related operations.

## Associated Functions

Inside any module, an associated function can be created by naming it with `inner_namespace::function` syntax. This declares a function as if it were inside an inner module named `inner_namespace`. For example, if module `mod1::mod2` declares `inner::function`, the function can be accessed through `mod1::mod2::inner::function`.

The primary application is type-associated functions, but the inner namespace does not need to correspond to the name of a type or any other symbol. Association organizes the function's name; it does not introduce implicit receiver or method-dispatch behavior.

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
```

## Pipe Calls

The pipe operator `|>` passes the expression on its left as the first argument to the function call on its right. These calls are equivalent:

```c
counter::increment(c);
c |> counter::increment();
```

The remaining arguments follow the inserted first argument, and the result can be piped into another call:

```c
int transform(int value, int scale);
void print_int(int value);

value
    |> transform(2)
    |> print_int();
```

Pipe calls are ordinary function calls, so ownership and type coercion apply to the inserted argument in the same way as an explicitly written first argument. In particular, `move value |> consume()` passes `move value` as the consuming first argument.

## Backward Pipe Calls

The backward pipe operator `<|` appends its right-hand expression to the argument list of the function call on its left. These calls are equivalent:

```c
combine(first, second, third);
combine(first, second) <| third;
```

The operator has one concrete purpose: supplying the final argument after the callee and its earlier arguments have been written. Forward and backward pipes can be combined:

```c
value |> combine(second) <| third;
// combine(value, second, third)
```

This is particularly useful when the final argument is a parameterized [staged expression](./comptime.md#parameterized-staged-expressions), because the operation being applied remains at the beginning of the expression.

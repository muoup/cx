---
title: Pattern Matching
---

# Pattern Matching

CX provides `is` for testing one tagged-union variant and `match` for branching over integer values or tagged-union variants.

## The `is` Operator

The simplest method to determine whether a tagged union contains a given variant is the `is` operator. Given a value `val` of type `T`, the expression `val is T::variant(inner)` returns a boolean value and binds `inner` to the payload with the type declared by `T::variant`.

The binding is created regardless of whether the comparison succeeds, but using it after the comparison evaluates to false is undefined behavior. The idiomatic way to keep the binding valid is to use the `is` operator as an `if` condition:

```c
float get_rectangle_area(shape& s) {
    if (s is shape::rectangle(r)) {
        return r.width * r.height;
    }

    return -1;
}
```

## Match Statements

Match statements are an alternative to C's `switch` statements that forbid fall-through. Each arm contains a pattern on the left and a single statement or scoped block on the right. Integer matches use integer-literal patterns, while tagged-union matches use variant patterns with an optional payload binding.

```c
void print_number(int i) {
    match (i) {
        1 => printf("i = 1\n");
        5 => printf("i = 5\n");
        _ => {
            printf("Other\n");
            log_number(i);
        }
    }
}
```

Tagged-union patterns name the variant and may bind its payload inside the corresponding arm. A match listing every variant is exhaustive; `_` can handle all variants or integer values that were not listed explicitly.

```c
float get_area(shape& s) {
    match (s) {
        shape::circle(radius) => return radius * radius * 3.14;
        shape::rectangle(r) => return r.width * r.height;
        shape::point() => return 0;
    }
}
```

## Match Expressions and `yield`

A `match` can produce a value. `yield` supplies that value from an arm and exits the match expression, including from inside a nested block:

```c
int value = match (maybe) {
    MaybeInt::some(inner) => {
        if (use_inner) yield inner;
        yield 2;
    }
    MaybeInt::none() => yield 3;
};
```

Every arm that can complete in a value-producing match must yield a compatible value. The match must also list every tagged-union variant or provide a `default` arm so that every runtime path produces a result.

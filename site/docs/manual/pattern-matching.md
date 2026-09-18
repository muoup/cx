---
title: Pattern Matching
---

# Pattern Matching

CX provides `is` for testing one tagged-union variant and `match` for branching over integer values or tagged-union variants.

## Patterns

Patterns are value skeletons that may be applied to a given value of the appropriate type to determine if it matches the given 'pattern'. They take on the following shapes:

```cx
T::variant(val)     // A pattern to match against a given variant of a tagged union, 
                    // and a named capture over its inner value
1                   // A single number to match an integer against
binding             // Matches anything and bindings the value to a new identifier
```

Note that in the case of tagged union patterns, while the type constructor to build a tagged union, in the case of a templated type, takes on the form of `T::variant<U, V, W, X>(inner)`, the template arguments must be elided in the case of a pattern, i.e. one must use the pattern `T::variant(inner)`. As a pattern is always used to match against a value, the template instantiation can always be deduced.

## The `is` Operator

The simplest method to determine whether a tagged union contains a given variant is the `is` operator. Given a value `val` of type `T`, the expression `val is T::variant(inner)` returns a boolean value and binds `inner` to the payload with the type declared by `T::variant`.

The binding is created regardless of whether the comparison succeeds, but using it after the comparison evaluates to false is undefined behavior. The idiomatic way to keep the binding valid is to use the `is` operator as an `if` condition:

```cx
float get_rectangle_area(shape& s) {
    if (s is shape::rectangle(r)) {
        return r.width * r.height;
    }

    return -1;
}
```

## Match Statements

Match statements are an alternative to C's `switch` statements that forbid fall-through. Each arm contains a pattern on the left and a single statement or scoped block on the right. Integer matches use integer-literal patterns, while tagged-union matches use variant patterns with an optional payload binding. A bare name is a catch-all pattern that binds the whole matched value within its arm; use `_` when the binding is intentionally unused. Bindings borrow an existing value or own an owned match subject, including one passed with `move`. Discarding a binding does not waive ownership or `@nodrop` requirements.

```cx
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

Tagged-union patterns name the variant and may bind its payload inside the corresponding arm. Every match must be exhaustive. A tagged-union match must list every variant or end with a catch-all binding; an integer match must end with a catch-all binding. Arms after a catch-all or complete variant coverage are unreachable and rejected, as are duplicate literal or variant patterns. Only top-level variants and payload bindings are supported.

```cx
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

```cx
int value = match (maybe) {
    MaybeInt::some(inner) => {
        if (use_inner) yield inner;
        yield 2;
    }
    MaybeInt::none() => yield 3;
};
```

Every arm that can complete in a value-producing match must yield a compatible value. As with statement matches, every possible input must be covered so that every runtime path produces a result.

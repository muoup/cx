---
title: Ownership and Moves
---

# Ownership and Moves

CX differentiates between two categories of value: an owned value and a reference. A reference, declared with the `&` suffix, is an non-owning view of a value of its inner type. Every value otherwise is considered owned.

An owned value is responsible for leasing references to its data. Initializing `T var = ...` creates a binding named `var` to an owned value of type `T`; referring to `var` provides a reference to that owned value. Any use of a reference that outlives the value leasing it is undefined behavior.

## Regions

CX semantics are described in terms of regions rather than memory. A region of type `T` is an accessible container of data large enough to contain the runtime storage of a valid value of `T`. An integer literal is an owning value over a region of its integer type, just as a structured initializer is an owning value over a region of its struct type. While a region is often stored in memory, it may also be stored across one or more registers.

This distinction explains why CX describes moves as transferring responsibility for a value rather than transferring a particular memory allocation. It also allows references to describe non-owning access independently of where the value is stored.

## Type Restrictions

Structs and tagged unions may be marked with the following type restrictions:

- `@nocopy` forbids implicit copying of the underlying type.
- `@nodrop` marks a type as linear and prevents its values from dropping out of scope.
- `@copy_traits(T)` copies the above restrictions from type `T` and applies them to the declared type.
- `@unsafe_move` requires explicit moves of the type to occur in an unsafe context when the enclosing function is `safe`.

Restrictions follow a type declaration after a colon:

```c
struct Data : @nocopy {
    int payload[32];
};
```

Restriction application is monotonic: `@nodrop` also applies `@nocopy`, and duplicate applications are silently ignored. The [Linear Resources](./linear-resources.md) chapter covers `@nodrop` and explicit resource cleanup in detail.

`@unsafe_move` is independent of copy and drop restrictions. It is intended for values that may become pinned or otherwise require a runtime condition before relocation:

```c
struct guarded_cell<T> : @copy_traits(T), @unsafe_move {
    T value;
    int borrow_count;
};
```

An aggregate containing an `@unsafe_move` field must also be marked `@unsafe_move`. In a `safe` function, moving such a value requires an explicit `@unsafe` block. Functions not marked `safe` are already unsafe contexts, so the attribute adds no further syntax there.

## Move Expressions

Operations such as variable initialization, assignment, and function pass-by-value require a coercion from `T&` to `T`. For standard C types this coercion can be implicit and denotes a copy. Types marked `@nocopy` produce an error if copied.

The expression `move var` transfers the value and kills the `var` binding, avoiding the need for a copy. Any subsequent use of that binding is caught by the typechecker. If a variable is moved in only one control-flow path, the compiler pessimistically treats it as dead in all paths that join the moving path.

```c
struct Data : @nocopy {
    int payload[32];
};

void consume(Data data);

void example(bool condition) {
    Data data = create_data();

    if (condition) {
        // Passes data directly to consume. The data binding is now dead.
        consume(move data);
    } else {
        // This path does not move data.
    }

    // This fails at compile time because data may have been moved above.
    consume(move data);
}
```

A dead binding can be initialized again before it is reused. Until that reinitialization occurs on every path reaching a use, the compiler continues to reject the use.

# Comptime and Staged Emission

AI-generated summary.

This note sketches a possible direction for a general comptime language feature. It is intended as a syntax and semantics reference for future design work, not as an implementation plan.

## Goals

The language should support both ordinary compile-time evaluation and more advanced type-safe metaprogramming. These are related, but they are not quite the same feature.

- `constexpr` should describe ordinary runtime-callable functions that can also be evaluated at compile time when their inputs are known.
- `comptime` should describe functions that run only inside the compile-time VM.
- `emit` should describe staged runtime code produced by a comptime function and lowered at the caller's use site.

This keeps traditional constant computation simple while giving metaprogramming code a clear place to interact with types, runtime expressions, and the compiler's abstract lowering model.

## Function Categories

Normal functions are runtime-only:

```cx
int add(int lhs, int rhs) {
    return lhs + rhs;
}
```

`constexpr` functions are ordinary functions that may also be evaluated at compile time:

```cx
constexpr int square(int value) {
    return value * value;
}
```

The intended model is similar to C++ `constexpr`: the function remains callable at runtime, but it can participate in compile-time evaluation when the language context requires or permits it.

`comptime` functions are compile-time VM functions:

```cx
comptime Type fast_vector(Type inner) {
    if (inner == bool) {
        return std::bitset;
    }

    return std::vector<inner>;
}
```

A `comptime` function is not a normal runtime function. It may work with type values, compile-time values, staged runtime expressions, and other compile-time-only concepts.

## Staged Runtime Expressions

A comptime function may accept a runtime expression as a typed staged value. The preferred spelling is:

```cx
expr T value
```

This means `value` is not a comptime-known `T`; it is a typed runtime expression of type `T` that the comptime VM may compose into emitted code.

For example:

```cx
comptime expr T opt::try<T>(expr opt<T> self) {
    emit match (self) {
        opt::some(value) => value;
        opt::none() => return opt::none<T>();
    };
}
```

The parameter `self` is runtime data. The comptime function does not inspect the runtime value of `self`; it describes how the expression should be lowered at the call site.

## `emit`

`emit` marks the staged runtime expression produced by a comptime function.

```cx
emit expression;
```

An emitted expression is lowered in place at the caller. It is not returned as an ordinary comptime value. It represents caller-spliced runtime code with normal typechecking and ownership semantics.

Inside emitted code, runtime control flow belongs to the caller's runtime context. For example:

```cx
comptime expr T opt::try<T>(expr opt<T> self) {
    emit match (self) {
        opt::some(value) => value;
        opt::none() => return opt::none<T>();
    };
}
```

The `return` in the `opt::none()` arm means a runtime return from the function where the emitted expression is used, not a return from the comptime function itself.

This allows library-defined control-flow constructs without requiring each one to be compiler magic.

## Optional-Like Try

The motivating example is an optional helper similar to a `?` operator:

```cx
opt<int> parse() {
    opt<int> first = get_first();
    int value = first |> opt::try();

    return opt::some(value + 1);
}
```

The pipe operator can feed the left-hand expression into a comptime staged function. Conceptually, this can lower as if the caller had written:

```cx
match (first) {
    opt::some(value) => value;
    opt::none() => return opt::none<int>();
}
```

The important point is that the standard library can define the behavior in typed staged code rather than relying on a built-in `?` operator.

## Types as Comptime Values

Types may be ordinary values in the comptime VM:

```cx
comptime Type fast_vector(Type inner) {
    if (inner == bool) {
        return std::bitset;
    }

    return std::vector<inner>;
}
```

This can express patterns that might otherwise require template specialization. The type computation is explicit, traceable, and can be given a normal name.

To avoid confusing functions with types in declarations, direct function-call syntax in type position should be treated carefully. A named type binding may be clearer:

```cx
Type BoolVec = std::fast_vector(bool);
BoolVec values;
```

Another possible style is to keep type-level values in expression-oriented pipelines:

```cx
auto object = std::fast_array(bool, 32)
    |> std::types::construct_object(...);
```

The exact shape of `std::types::construct_object` is still open. It may eventually wrap lower-level intrinsic functionality for constructing an object from a comptime type value, but that design should be iterated separately.

## Open Syntax Questions

The exact varargs story for staged expression parameters is intentionally left open. The design should preserve the ability for function prototypes to describe useful constraints on each argument, rather than collapsing staged arguments into an untyped variadic blob.

Possible future categories include:

- `expr T value`: a runtime expression of type `T`.
- `place T value`: a runtime assignable place of type `T`.
- `Type T`: a comptime type value.
- ordinary `T value`: a comptime-known value of type `T` inside a `comptime` function, or an ordinary runtime parameter inside a non-comptime function.

These names and categories should be refined as the rest of the comptime system becomes clearer.

## Guiding Principle

`constexpr` computes values.

`comptime` computes language artifacts: types, staged expressions, places, declarations, and policies.

`emit` bridges the comptime VM and the runtime abstract machine by producing typed runtime code at the caller's use site.

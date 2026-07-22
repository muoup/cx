---
title: Comptime
---

# Comptime

CX provides support for a few different means for comptime metaprogramming, with some more feature expansion planned for the future. This section will cover traditional templates, and function comptime semantics. 

Functions which are able to be evaluated at compile-time as well as run-time similar to C++'s constexpr keyword are not yet implemented but are planned to be in the future.

# Templates

CX supports standard templated functions and types. Note that in contrast with other languages, templates are restricted to one symbol per definition. Therefore, features like partial specialization are strictly prohibited as they pollute the traceability and clarity of code. Additionally, specialization of a template over non-types, i.e. constexpr value specialization, is still a work-in-progress.

*Side Note*: Current template design uses C++ copy-and-paste semantics. There are a few limitations with this approach, including lack of safety, poor compilation speed impact, and overly verbose error reporting. This is a known issue and in the near-ish future, template syntax will be overhauled to require type bounds, and thus code written with this current template system will be broken in coming updates.

Function example:

```c
T add<T>(T a, T b) {
    return a + b;
}
```

Type example:

```c
struct Box<T> {
    T value;
};
```

`typedef` creates template type aliases:

```c
typedef<T> T* Ptr;
```

# Comptime Functions

Functions may also be declared explicitly as 'comptime' which indicates that they **must** be evaluated compilation time. This enables them to, unlike standard functions, take in a special category of type, denoted via the 'expr' keyword, which represents frozen expressions rather than evaluated values. One can conceptualize this as reasoning and evaluating over the AST representation of an expression rather than a value. This enables functions that act as type-safe macros, producing expressions rather than just values.

For instance, if one wanted to recreate Rust's `?` operator for optionals, which act as a shorthand for early-return-if-none, e.g:

```rust
fn get_option() -> Option<i64> { ... }

fn routine() -> Option<u8> {
    // The 'get_option' function will either return us Some(i64) or None, the `?` operator 
    // unwraps the plain i64 if we have a value, otherwise we return None from the function
    let val : i64 = get_option()?;

    ...
}
```

This behavior can be replicated with a function that returns a generated 'match' statement. The standard library's implementation looks as follows:

```cpp
comptime expr T opt::try<T>(expr opt<T> self) {
    return emit match (self) {
        opt::some<T>(value) => yield move value;
        opt::none<T>() => return opt::none();
    };
}
```

Note that 'emit' here takes in an expression and freezes it into an `expr T`, where T indicates the type produced by the expression.

This function then can be used to reproduce the above example as such:

```cpp
import std::opt as std;

std::opt<i64> get_option() { ... }

std::opt<u8> routine() {
    i64 val = get_option()
        |> std::opt::try();

    // After constexpr evaluation, we would be left with an expression equivalent to:
    // 
    // i64 val = match (get_option()) {
    //      std::opt::some(val) => yield val;
    //      std::opt::none => return std::opt::none;
    // };
}
```

One additional unique thing the `expr T` syntax allows is for pseudo-closures. A common issue that arises when trying to use the above opt::try implementation is accidentally leaking undropped resources with the early return. For instance:

```cpp
import std::opt as std;
import std::vector as std;

std::opt<i64> find_specific_number(std::vector<i64>& values) { ... }

std::opt<i64> routine() {
    std::vector<i64> values = ...;

    i64 number = values 
        |> find_specific_number()
        |> std::opt::try();

    ...

    values |> std::vector::drop();
    return ...;
}
```

This code above, with valid implementation details elided in the elipses, will lead to a type error, as the std::opt::try() call will not implicitly clean up the `values` variable in its generated early return path. The standard library exposes a `std::opt::try_or` function for this case. The correct implementation of `routines` as shown above would look like:

```cpp
import std::opt as std;
import std::vector as std;

std::opt<i64> find_specific_number(std::vector<i64>& values) { ... }

std::opt<i64> routine() {
    std::vector<i64> values = ...;

    i64 number = values 
        |> find_specific_number()
        |> std::opt::try_or(.{
            values |> std::vector::drop();
        });

    ...

    values |> std::vector::drop();
    return ...;
}
```

Note however that the `.{ ... }` syntax is **not** a closure and hides no hidden behavior, in this example we are passing to std::opt::try_or a block expression that is injected directly into the early-return path. `std::opt::try` would thus be equivalent to invoking this routine with a no-op expression.
---
title: Move Semantics
---

# Move Semantics

Structs and tagged unions may be marked with the following type restrictions:

- `@nocopy`: Forbids implicit copying of the underlying struct.
- `@nodrop`: Marks a type as linear, prevents any value of said type from dropping out of scope.
- `@copy_traits(T)`: Copies the above restrictions from type T and applies it to itself.

Restriction application is monotonic, a `@nodrop` restriction also applies a `@nocopy` restriction, and duplicate applications are silently
ignored.

## Move Statements

CX differentiates between two categories of value: an "owned" value, and a "reference". A reference corresponds approximately to the reference type declared with the `&` suffix. They are addressable non-owning views of a value of its inner type. Every value otherwise is considered "owned". An owned value is responsible for leasing references to its data. When initializing some variable `T var = ...`, one is creating an binding "var" to an owned value of type "T", referring to identifier "var" provides to you a reference to its owned value. Any usage of a reference that outlives the leasing value is undefined behavior.

For many cases where owned values need to be created, such as in variable assignment or function pass-by-value, a coercion from type `T&` to `T` needs to occur. For standard C types, this can be implicit and denotes a copy. Types denoted `@nocopy` however will cause an error if they are copied, to get around this one may use `move var` syntax to "kill" the var identifier and prevent the need for a copy. Any subsequent usage of the var binding will be caught by the typechecker. If a variable is only killed in one path, e.g. in an if-statement, the compiler is pessimistic and kills the variable in all paths that come in contact with the moving path.

```cpp
struct Data {
     int payload[32];
};

void bar(Data data);

void foo(bool condition) {
    Data data = create_data();

    // Duplicates the struct and passes it to bar.
    bar(data);

    if (condition) {
        // Passes data directly to bar, "data" is now dead and cannot be used again.
        bar(move data); 
    } else {
        // Do other things that don't move data
    }

    // This fails at compile time, as there is a chance 'data' was killed 
    // in the above if-statement, even if it may also still be alive
    bar(move data); 
}
```

## "Nodrop" Types

For complex types which need to manage some kind of resource, it is import not just that the type is not copied, but that the value is not
destroyed until the resources are cleaned up. For this case, we use the `@nodrop` restriction, which implicitly also applies the restrictions
of `@nocopy`. A `@nodrop` type cannot fall out of scope, and must either be moved to another function, or destroyed unsafely via usage of
`@leak` to ensure the compiler that it's inner resources have been taken care of.

```cpp

struct string {
    char* data;
    size_t length;
    size_t capacity;
};

void bad_string_usage1(string s) {
    for (usize i = 0; i < s.length; i++) { ... }

    puts(s.data);

    // This function will error because s falls out of scope here and has not been taken care of. In C this would lead to a memory leak.
}

void good_string_usage1(string& s) {
    for (usize i = 0; i < s.length; i++) { ... }

    puts(s.data);

    // This function is okay, since s is passed as a reference, we do not own the inner value despite our mutations, it is up to the callee
    // to handle cleaning up the value as they own it
}

void good_string_usage2(string s) {
    for (usize i = 0; i < s.length; i++) { ... }

    puts(s.data);

    free(s.data);
    @leak(s); // Kills the 's' binding without moving the value anywhere.

    /*
        This function will compile as well as we have ensured to the compiler that we know that 's' is falling out scope and that is okay as
        we have freed its inner memory.
    
        The more idiomatic way to handle this is to have a "drop" function. If one declares:
    
        void string::drop(string s) {
            free(s.data);
            @leak(s);
        }

        With the above function, we could instead call `move s |> string::drop` here. The 's' binding is killed because
        the function consumes the value.
    */ 
}

```

## 'Adopting' a Value

An interesting edge case that arises in some complex data types occurs when handling dropping values which do not represent a unique allocation or memory region. For instance, consider this implementation of vector over some `@nodrop` type Inner:

```cpp
struct InnerVector : @nodrop {
    Inner* data;
    size_t length;
    size_t capacity;
};

void Inner::drop(Inner this) { ... }

void InnerVector::drop(InnerVector this) {
    for (usize i = 0; i < length; i++) {
        // This call is not valid because data[i] only produces a reference. Inner::drop consumes 
        // its first parameter, so we must own a value of type `Inner`, not `Inner&`.
        data[i] |> Inner::drop();
    }

    @leak(this);
}
```

Since in the above example, `data` is heap allocated as one contiguous memory buffer, there isn't a sound way to represent "owning" a part of the buffer. What is needed is some escape hatch that enables unsafely upgrading a reference to an arbitrary owned value. CX allows for the concept of "adopting" a foreign region, where a reference can be transformed into an owning value for edge cases such as this.

```cpp
void InnerVector::drop(InnerVector this) {
    for (usize i = 0; i < length; i++) {
        Inner index = @adopt(data[i]);

        // This is valid, index here acts as a reference that is asserting to the compiler that it wants to be treated
        // as if it owns the value.
        move index |> Inner::drop();
    }
}
```

As "owning" a value does not have directly implications on memory management itself, this can be safe in certain circumstances. Generally speaking, if one adopts a value which already has a live mapping, one is very likely to run into undefined behavior. It is highly recommended that usage of `@leak` and `@adopt` statements are used in abstractions in very well-understood and narrow implementation details.

## A Note on Regions

CX semantics are handled without a notion of memory, instead opting in favor of regions. This does not impact much in terms of semantics, but is very important for why language features are described as they are. A region of type `T` is simply an accessible container of data at least the size of `T` containing the runtime storage of some valid value of that type. Every integer literal is an owning value over a region of that integer type, just as every structured initializer is a region of that struct type. While a region is often stored in memory, it may also be stored across one or multiple registers.

One important difference from C with these semantics is with string literals. In C, a string literal is of type `const char*`, however CX uses an intermediate type `_str&`. A `_str&` can be implicitly cast to and will have an identical size to `const char*`, thus  C code using string literals will function the same, however the use of this extra type helps ensure safer guarantees where desired. 

A `_str` much like in other languages is an unsized type, it represents a string of data such that it terminates with a null value. Given its unsized nature, it cannot be directly assigned to a variable, it may only be applied via assertion onto an already-existing string of data, where for string literals, said data is static memory. This distinction can prove helpful, as while static memory helps ensure that string literals meet these guarantees, not all values of type `const char*` in idiomatic C use will be zero-terminated.

It is thus recommended that for functions in and similar to C's "string.h" library, one takes in a `const _str&` instead of a `const char*` for better documentation and preventing accidental usage of non-zero-terminated char arrays where zero termination is required. If one owns a zero-terminated char array annotated as a `const char*`, for instance if creating a string at runtime, said buffer can be explicitly cast to a `_str&` via C-style casting, this operation is however unsafe and casting a non-zero-terminated value is undefined behavior.

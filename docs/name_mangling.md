# Name Mangling

## Introduction

### Motivation

Consider we have some CX function that looks as such:

```c++

T add<T>(T a, T b) {
    return a + b;
}

```

If we were to instantiate two versions of this function, one for say `i32` and another for `f32`,
we would end up with two functions with the exact same name, `add`. Since assembly is a (mostly)
typeless language, the only identifier for some function is its name, meaning we need some system
to create unique names for each function instantiation, or as it's called, *mangle* the names.

## Current Spec

### Type Mangling

For the exact format of type mangling, see the type_mangle function in 
[the typechecker code](../compiler/cx-typechecker-data/src/format.rs).

The basic idea of mangling a type's name is to create a string representation of the entire
nested type. This means that the pointed-to type of a pointer must be included, as well as
all of the fields in a struct or a union. For information as to why this is the case, see
the [considerations](#considerations) section below.

Specifics of the type mangling are not going to be documented here, as they are fairly
arbitrary and may change in the future, the important part is the uniqueness of the mangled
name.

### Function Mangling

One of the goals of CX is to avoid name mangling where possible to ensure that it adheres to
a C ABI where it can. This means that standard functions without generics or other special
features will not be mangled, as there is no risk of collision.

This means that the function name mangling spec essentially boils down to this:

1. Apply a function-kind mangle, for a standard function, this is a no-op, but for the rest
   of the function kinds, we do the following:
   - Member functions: `_M[type_base_name]_{function_name}`
   - Destructors: `_D[type_base_name]`
   - Deconstructor: `_DC[type_base_name]`
2. If the function is generic, i.e. we expect there may be multiple instantiations of it
   and symbol collision may occur, we create a generic mangle, which acts as a prefix
   to the function name. The exact specifics are for now left unspecified in case they
   need to be changed later, but the general format is as follows:
   - Template Prefix: `_t[return_type]_[param1_type]_[param2_type]..._`
3. If the prefix was generated in step 2, the final mangled name is:
   - `[template_prefix][function_kind_mangle]`
   or if not, the final name is simply the name generated in step 1.
   
## Considerations
   
### Why include full type / function information in mangling?

For the sake of symbol conciseness, one may consider basing the mangling on the arguments of
the template instantiation rather than the full type, this was how the system was implemented in
its initial stage, however this runs into a problem. Consider the code below:

```c++
struct TemplatedStruct<T1, T2> { ... some fields ... }

typedef<T> SpecialTemplatedStruct = TemplatedStruct<T, i32>;

~TemplatedStruct<T1, T2> {
    ... some generic cleanup code ...
}

void function() {
    TemplatedStruct<i32, i32> var1 = ...;
    SpecialTemplatedStruct<i32> var2 = ...;
    
    ... some code ...
    
    // end of function
    
    // We need to deconstruct var1 and var2 here since they falls out of scope, and we know of each:
    //   - the mangled name of the struct
    //   - the base name of the struct (i.e. TemplatedStruct)
    // 
    // We can't (feasibly) know:
    //   - the base templates used to create the struct
    //   - what template arguments were used to instantiate it
    // 
    // Theoretically, both var1 and var2 are of the same type, however if we base the mangling of
    // the type on only what was used to instantiate it, we run the risk of either creating two
    // different mangled names for the same type, or requiring a semi-commital template mangling
    // system that only mangles if it is one-degree-from a templatable type, which is a mess
    // best avoided.
}
```
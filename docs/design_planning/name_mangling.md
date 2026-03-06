# Name Mangling

## Introduction

### Motivation

Consider we have some CX function that looks as such:

```c++

T add<T>(T a, T b) {
    return a + b;
}

```

If we were to instantiate two versions of this function, one for say `i32` and another for `f32`, we would end up with two functions with the exact same name, `add`. Since assembly is a (mostly) typeless language, the only identifier for a function is its name, meaning we need some system to create unique names for each function instantiation, or as it's called, *mangle* the names.

## Current Spec

### Type Mangling

For the exact format of type mangling, see the type_mangle function in [the typechecker code](../compiler/cx-typechecker-data/src/format.rs).

The basic idea of mangling a type's name is to create a string representation of the entire nested type. This means that the pointed-to type of a pointer must be included, as well as all of the fields in a struct or a union. For information as to why this is the case, see the [considerations](#considerations) section below.

Specifics of the type mangling are not going to be documented here, as they are fairly arbitrary and may change in the future, the important part is the uniqueness of the mangled name.

### Function Mangling

The main point of complication to consider when designing a name mangling scheme is what exactly it means to 'lookup' a function.

```c++

struct Data<T> { ... };

void Data<int>::procedure { ... };
void Data<T>::procedure<T> { ... };

```

For instance, in the following code, a simple model of function lookup would take some name, and through a map-like data structure, follow its key to a function definition. However, if we attempt to call one of these procedure implementations, via say:

```c++
int main() {
    Data<int> d;
    d.procedure();
}
```

We could extract from the "d.procedure()" line that:

1. The base name of 'd', the name it was instantiated from, is 'Data'.
2. The type parameter(s) used to instantiate 'd' is a single 'int'.
3. The mangled name of the type is some TypeMangle("Data", [int])

With this in mind, the question becomes, what does it mean to query a function with this information? Do note that function templates cannot be instantiated until they are acknowledged (i.e. called) for the first time, so if d was of any other Data<T> type, the function would not exist yet. Our query cannot even simply look through existing functions to find a match, it must broadly query under the impression it may need to find a generic prototype that has not yet been typechecked as it does not have full type information of its symbols.

For storing queryable function information, there is a tale of two function types here. CX does not allow for overloading functions (based solely on parameter types), so if a function contains no template parameters, it can be storied simply as its mangled name, and looked up directly. Despite how it may look, that is the case for the first procedure implementation above, as there is nothing generic about the function once the struct has been instantiated. However, for functions that do have template information, especially in the future when partial specialization is allowed, you need to query a generic template key that gives you back a list of potential matches, requiring you to then find the first one the invocation can use.

The simple solution to this dichotomy is to search for both cases. When the typechecker is looking for the function to call with the above code, it will:

1. Mangle Data<int>::procedure() to some MangleFunction(Data, [int])
2. Search the typechecked function corpus to see if that exact function exists. If it does, use it.
3. If it does not, create a generic query, say Key { name: procedure, type: MemberFunction(Data) }
4. Look through all functions matching that key, and see if any of them can be instantiated with the given type information. If one can, instantiate it and use it.
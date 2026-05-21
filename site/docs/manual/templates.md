---
title: Templates
---

# Templates

CX supports templated functions and aggregate types. Note that in contrast with other languages, templates are restricted to one symbol per definition. Therefore, features like partial specialization are strictly prohibited as they pollute the traceability and clarity of code. 

*Side Note*: Current template design uses C++ copy-and-paste semantics. There are a few limitations with this approach, including lack of safety, performance limitations, and poor error reporting. This is a known issue and in the near-ish future, template syntax will be overhauled to require type bounds, and thus code written with this current template system will be broken in coming updates.

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
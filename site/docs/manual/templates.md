---
title: Templates
---

# Templates

CX supports templated functions and types. In contrast with other languages, templates are restricted to one symbol per definition. Features such as partial specialization are prohibited because they pollute the traceability and clarity of code. Specialization over non-types, such as compile-time value specialization, remains a work in progress.

### **NOTE:**

The current template design uses C++-style copy-and-paste semantics, which come at the cost of worse error reporting, slower compilation, and worse overall traceability. Template syntax is expected to be overhauled to require type bounds, so code using this current template system will be broken by future updates.

## Function Templates

Template parameters follow the function name:

```cx
T add<T>(T a, T b) {
    return a + b;
}
```

Template arguments can be written explicitly or deduced from the function arguments when the parameter types provide enough information:

```cx
int explicit_sum = add<int>(1, 2);
int deduced_sum = add(1, 2);
```

## Type Templates

Template parameters follow the declared type name:

```cx
struct Box<T> {
    T value;
};
```

`typedef` can be used to create templated type aliases:

```cx
typedef<T> T* Ptr;
```

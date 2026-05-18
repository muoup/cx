---
title: Templates
---

# Templates

CX supports templated functions and aggregate types.

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

Current limitations:

- Template argument inference is not implemented.
- Specialization is effectively one definition per function identifier or
  realized member slot.

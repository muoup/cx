---
title: Modules and Visibility
---

# Modules and Visibility

CX uses explicit imports and visibility sections.

## Imports

`import` brings public declarations from another module into scope. Imported
declarations are not implicitly re-exported.

```c
import std::io;
import math::vec;
```

Module paths map to file paths. For example, `std::io` resolves to `lib/std/io.cx`
or the equivalent path relative to the project root. Imports are resolved during
preparse, and public declarations are merged into the importing module's symbol
environment.

## Visibility

Visibility is controlled by `public:` and `private:` section headers.
Declarations are private by default.

```c
public:

i32 api_function(i32 x) {
    return helper(x);
}

struct Point {
    i32 x;
    i32 y;
};

private:

i32 helper(i32 x) {
    return x + 1;
}
```

All declarations following a `public:` header are visible to importers. A
`private:` header switches back to module-internal visibility.

## `.cxh` Library Entry Files

Files with the `.cxh` extension serve as library entry points. They use the same
syntax as `.cx` files. When a `.cxh` file is compiled as a library target, its
non-static, non-external functions become the library's exported symbols.

See the [build system](./build-system.md) guide for library configuration and
C header generation.

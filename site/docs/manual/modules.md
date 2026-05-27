---
title: Modules and Visibility
---

# Modules and Visibility

CX uses explicit imports and visibility sections.

## Imports

`import` makes public declarations from another module available to the current
module. Imported declarations can be referenced through their module namespace.

```c
import std::io;
import math::vec;

int main() {
    std::io::println("hello");
    return 0;
}
```

Module paths map to file paths. For example, `std::io` resolves to `lib/std/io.cx`
or the equivalent path relative to the project root.

During the namespace migration, imported public symbols are still also available
by their legacy unqualified names for compatibility with older code. New code
should prefer qualified names because imports are intended to become reachability
declarations rather than symbol copies.

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

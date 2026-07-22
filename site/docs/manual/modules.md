---
title: Modules and Visibility
---

# Modules and Visibility

CX uses explicit imports and visibility sections.

## Imports

`import` makes public declarations from another module available to the current
module. Imported declarations can be referenced through their module namespace.

```cpp
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

## Import Aliases

An import can provide an alternative namespace with `as`:

```cpp
import std::vector as v;

v::vector<int> values = v::vector::new<int>();
```

The special alias `_` places the imported module at the root namespace, allowing its public declarations to be referenced without the module prefix:

```cpp
import std::vector as _;

vector<int> values = vector::new<int>();
```

Aliases may overlap, however any symbol name which refers to multiple definitions due to alias overlap cannot be used. However, if this occurs with `_`-aliasing and one definition refers to a symbol defined in the current module, the namespace symbol will deduce to that definition:

```cpp
// Okay:
import std::vector as std;
import std::file as std;
import std::optional as std;

std::vector<int> vec = std::vector::new<int>();
std::file file = std::file::open("test.txt")
    |> std::optional::unwrap();

// Error:
import std::vector as mod;
import other::vector as mod; // Where `other` contains some definition of a `vector` type

mod::vector vec = mod::vector::new(); // Type Error: The symbol `mod::vector` is ambiguous

// Okay:
import std::vector as _;

struct vector { ... };

vector vec = ...; // Okay: the current module's definition of `vector` takes precedence
```

## Visibility

Visibility is controlled by `public:` and `private:` section headers.
Declarations are private by default.

```cpp
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

Files with the `.cxh` extension serve as library entry points and use the same syntax as `.cx` files. The [Libraries and C Interop](../getting-started/c-interop.md) guide explains how their public declarations become C-compatible library exports.

---
title: Libraries and C Interop
description: Explicit C exports from ordinary .chl modules, generated headers, and linking from C.
---

# Libraries and C Interop

A `.chl` module uses namespace-based symbol names by default. A library target can generate a C header for that same module without changing its naming rules or its availability through normal module imports.

## Declaring the C Interface

Use a `public extern "C":` section for definitions intended for the C interface:

```chl title="mathlib.chl"
public extern "C":

struct Point {
    i32 x;
    i32 y;
};

typedef i32 Count;

i32 calls = 0;

i32 add(i32 a, i32 b) {
    calls += 1;
    return a + b;
}

public:

i32 twice(i32 value) {
    return value * 2;
}
```

The generated header contains `add`, an `extern` declaration for `calls`, and the standalone `Point` and `Count` declarations. `twice` retains its namespaced symbol and is available to module importers, but is absent from the C header.

Public C named types are included even when no exported function uses them. Types required transitively by exported functions, globals, or types are included using their own naming scheme. A dependency declared outside an `extern "C"` section keeps its mangled name in the header, making that dependency visible to the developer. Named aliases are emitted as typedefs; integer enums are represented by an integer typedef and macros for their public C enumerators.

An `extern "C":` section is private unless prefixed with `public`. A later `public:` or `private:` section restores namespace-based naming. A function declaration without a body imports an external symbol; a global without an initializer in an `extern "C"` section does likewise. Define exported globals with an initializer, including `= 0` for zero initialization.

Static definitions and imported declarations are omitted from the generated interface. Generic and compile-time functions cannot be exported in a C header.

## Library Targets

Request headers explicitly in `chl.lua`:

```lua title="chl.lua"
package("mathlib")
stdlib("std")

local lib = target("mathlib", "library")
lib:match("mathlib.chl")
lib:headers("mathlib.chl")
```

Run `chl build mathlib`. With the default debug profile, the output is `.internal/debug/mathlib.o` and `.internal/debug/include/mathlib/mathlib.h`.

Header selection controls the C interface. The object also retains ordinary namespaced symbols from the library target's source modules.

## Calling from C

```c title="main.c"
#include <stdio.h>
#include "mathlib.h"

int main(void) {
    struct Point point = {3, 4};
    Count sum = add(point.x, point.y);
    printf("sum = %d, calls = %d\n", sum, calls);
    return 0;
}
```

```bash
cc main.c .internal/debug/mathlib.o -I.internal/debug/include/mathlib -o main
```

Generated headers include the types they need and guard function and variable declarations with `extern "C"` when compiled as C++.

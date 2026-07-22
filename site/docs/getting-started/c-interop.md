---
title: Libraries and C Interop
description: CX library targets, .cxh entry files, generated C headers, and linking from C.
---

# Libraries and C Interop

When interfacing a CX codebase with C, or with another language supporting C FFI, a project may define a library target. A library target consists of an object file containing the compiled CX code and a C header generated from its CX header entry file.

## `.cxh` Library Entry Files

Files with the `.cxh` extension serve as library entry points and use the same syntax as `.cx` files. When a `.cxh` file is compiled as a library target, its non-static, non-external functions become the library's exported symbols and declarations in the generated C header.

```c title="mathlib.cxh"
i32 add(i32 a, i32 b) {
    return a + b;
}

i32 multiply(i32 a, i32 b) {
    return a * b;
}
```

## Library Targets

The entry file is configured in a `libraries` array under a workspace target:

```toml title="cx.toml"
[project]
name = "mathlib"

[workspace.targets.default]
libraries = [
  { name = "mathlib", entry = "mathlib.cxh" },
]
```

Building this project with `cx build` performs three steps:

1. It compiles all source files to object files with per-function ELF sections.
2. It uses `ld -r --gc-sections` to merge the object files while marking exported symbols as roots, which eliminates unreachable code during the relocatable link.
3. It generates a C header from the entry file's LMIR data.

The result is `.internal/output/default/mathlib.o` and `.internal/output/default/mathlib.h`.

## Calling the Library from C

Compiling the example above will provide a C header file as mentioned in `.internal/output/default/mathlib.h` that looks like so:
```c title=".internal/output/default/mathlib.h
#pragma once

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

extern int32_t add(int32_t a, int32_t b);
extern int32_t multiply(int32_t a, int32_t b);

#ifdef __cplusplus
}
#endif
```

Include this header in your designated external C header directory, and it may be used as if it was a standard C header:

```c title="main.c"
#include <stdio.h>
#include "mathlib.h"

int main(void) {
    printf("3 + 4 = %d\n", add(3, 4));
    printf("3 * 4 = %d\n", multiply(3, 4));
    return 0;
}
```

Then link the generated object file with the C program:

```bash
gcc -o main main.c .internal/output/default/mathlib.o
```

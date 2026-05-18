---
title: Build System
description: Project builds, cx.toml configuration, library targets, and C header generation.
---

# Build System

CX includes a project-oriented build system driven by `cx.toml`. It supports
named targets with binaries and libraries, dead code elimination through
per-function sections, and automatic C header generation for library targets.

## Project Initialization

```bash
cx init <project_name>
```

Creates a new directory containing a `cx.toml` and `main.cx`:

```toml title="cx.toml"
[project]
name = "my_project"

[build]
backend = "cranelift"
optimization = "O0"

[workspace.targets.default]
binaries = [
  { name = "my_project", entry = "main.cx" },
]
```

```c title="main.cx"
import std::io;

i32 main() {
    println("Hello, world!");
    return 0;
}
```

## `cx.toml` Reference

`[project]` fields:

| Field | Type | Required | Description |
| --- | --- | --- | --- |
| `name` | string | yes | Project name |

`[build]` fields are optional. CLI flags override these values.

| Field | Type | Default | Description |
| --- | --- | --- | --- |
| `backend` | `"cranelift"` or `"llvm"` | `"cranelift"` | Code generation backend |
| `optimization` | `"O0"` through `"O3"`, `"Osize"`, `"Ofast"` | `"O0"` | Optimization level |
| `analysis` | bool | `false` | Run FMIR safe-function analysis |

Each `[workspace.targets.<name>]` target can contain binaries, libraries, and
link dependencies.

| Field | Type | Description |
| --- | --- | --- |
| `binaries` | array of `{ name, entry }` | Executable targets. `entry` is a `.cx` source file. |
| `libraries` | array of `{ name, entry }` | Library targets. `entry` is typically a `.cxl` file. |
| `link` | array of `{ name, kind }` | External link dependencies. `kind` is `"system"`, `"static"`, or `"dynamic"`. |

## Building

```bash
cx build
cx build <target>
```

Project mode requires a `cx.toml` in the current directory or a parent
directory. CLI flags override the `[build]` section:

```bash
cx build --backend-llvm -O3
cx build --analysis
cx build --verbose
```

Single-file compilation still works without a `cx.toml`:

```bash
cx main.cx -o my_app
```

## Output Structure

Build artifacts are placed under `.internal/`:

```text
.internal/
├── output/
│   └── <target>/
│       ├── <binary_name>
│       ├── <library_name>.o
│       └── <library_name>.h
└── <profile_hash>/
    ├── <module>.o
    ├── <module>.tok
    ├── <module>.ast
    └── <module>.lmir
```

## Library Compilation

Library entry points use the `.cxl` extension. Syntax is identical to `.cx`.
The entry file determines exported symbols: all non-static, non-external
functions defined in it become public API.

Library compilation:

1. Compile all source files to object files with per-function ELF sections.
2. Use `ld -r --gc-sections` to merge object files, marking exported symbols as
   roots.
3. Generate a C header from the entry file's LMIR data.

Dead code elimination happens automatically during the relocatable link.

## C Interop Example

```c title="mathlib.cxl"
i32 add(i32 a, i32 b) {
    return a + b;
}

i32 multiply(i32 a, i32 b) {
    return a * b;
}
```

```toml title="cx.toml"
[project]
name = "mathlib"

[workspace.targets.default]
libraries = [
  { name = "mathlib", entry = "mathlib.cxl" },
]
```

Build the library:

```bash
cx build
```

This produces `.internal/output/default/mathlib.o` and
`.internal/output/default/mathlib.h`.

Use the generated header and object file from C:

```c title="main.c"
#include <stdio.h>
#include "mathlib.h"

int main(void) {
    printf("3 + 4 = %d\n", add(3, 4));
    printf("3 * 4 = %d\n", multiply(3, 4));
    return 0;
}
```

```bash
gcc -o main main.c .internal/output/default/mathlib.o
```

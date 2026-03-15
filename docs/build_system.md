# Build System

CX includes a project-oriented build system driven by `cx.toml` configuration files. It supports named targets with multiple binaries and libraries, dead code elimination via per-function sections, and automatic C header generation for library targets.

## Project Initialization

```bash
cx init <project_name>
```

Creates a new directory containing a `cx.toml` and `main.cx`:

**Generated `cx.toml`:**
```toml
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

**Generated `main.cx`:**
```c
import std::io;

i32 main() {
    io::println("Hello, world!");
    return 0;
}
```

## `cx.toml` Reference

### `[project]`

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | yes | Project name |

### `[build]`

All fields are optional. CLI flags override these values.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `backend` | `"cranelift"` \| `"llvm"` | `"cranelift"` | Code generation backend |
| `optimization` | `"O0"` – `"O3"`, `"Osize"`, `"Ofast"` | `"O0"` | Optimization level |
| `analysis` | bool | `false` | Run FMIR safe-function analysis |

### `[workspace.targets.<name>]`

Each named target can contain any combination of binaries, libraries, and link dependencies.

| Field | Type | Description |
|-------|------|-------------|
| `binaries` | array of `{ name, entry }` | Executable targets. `entry` is a `.cx` source file. |
| `libraries` | array of `{ name, entry }` | Library targets. `entry` is typically a `.cxl` file. |
| `link` | array of `{ name, kind }` | External link dependencies. `kind` is `"system"`, `"static"`, or `"dynamic"`. |

### Full Example

```toml
[project]
name = "my_project"

[build]
backend = "cranelift"
optimization = "O2"

[workspace.targets.default]
binaries = [
  { name = "my_app", entry = "main.cx" },
]

[workspace.targets.math]
libraries = [
  { name = "mathlib", entry = "mathlib.cxl" },
]
link = [
  { name = "m", kind = "system" },
]
```

## Building

### Project Mode

```bash
cx build              # build all targets
cx build <target>     # build a specific target
```

Requires a `cx.toml` in the current directory or a parent directory. CLI flags override the `[build]` section:

```bash
cx build --backend-llvm -O3    # override backend and optimization
cx build --analysis             # enable FMIR analysis
cx build --verbose              # print each compilation step
```

### Legacy Mode

Single-file compilation still works without a `cx.toml`:

```bash
cx main.cx -o my_app
```

## Output Structure

Build artifacts are placed under `.internal/`:

```
.internal/
├── output/
│   └── <target>/
│       ├── <binary_name>       # executable
│       ├── <library_name>.o    # merged relocatable object
│       └── <library_name>.h    # generated C header
└── <profile_hash>/             # cached intermediate files
    ├── <module>.o
    ├── <module>.tok
    ├── <module>.ast
    └── <module>.lmir
```

The `<profile_hash>` directory is derived from the backend, optimization level, and compiler version, allowing different configurations to coexist without cache conflicts.

## Library Compilation

### `.cxl` Files

Library entry points use the `.cxl` extension. The syntax is identical to `.cx` files. The entry file determines which symbols are exported: all non-static, non-external functions defined in it become public API.

### Pipeline

1. **Compile** — all source files are compiled to individual object files with per-function ELF sections (`.text.<function_name>`)
2. **Relocatable link** — `ld -r --gc-sections` merges object files into a single `.o`, using `--undefined=<sym>` to mark exported symbols as roots
3. **Header generation** — a C header is generated from the entry file's LMIR data

Dead code elimination happens automatically: any function not reachable from an exported symbol is stripped during the relocatable link.

### Exported Symbols

A function is exported if it appears in the library entry file and has standard or ODR linkage. Functions with `static` linkage and external declarations are excluded.

## C Header Generation

The generated header provides a C-compatible interface to the library.

### Structure

```c
#pragma once

// Link dependencies:
//   -lm (system)

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations
struct MyStruct;

// Type definitions
struct MyStruct {
    int32_t x;
    int32_t y;
};

// Function declarations
int32_t add(int32_t a, int32_t b);

#ifdef __cplusplus
}
#endif
```

### Type Mapping

| CX / LMIR Type | C Type |
|-----------------|--------|
| `bool` | `bool` |
| `i8` | `int8_t` |
| `i16` | `int16_t` |
| `i32` / `int` | `int32_t` |
| `i64` | `int64_t` |
| `i128` | `__int128` |
| `float` | `float` |
| `double` | `double` |
| pointer | `void*` |
| `void` | `void` |
| struct | `struct <name>` |
| union | `union <name>` |
| array | `<type>[<size>]` |
| opaque | `uint8_t[<bytes>]` |

Link dependency comments at the top of the header indicate which system/static/dynamic libraries must be linked when using the generated `.o` file.

## C Interop Example

### 1. Write a library

**mathlib.cxl:**
```c
i32 add(i32 a, i32 b) {
    return a + b;
}

i32 multiply(i32 a, i32 b) {
    return a * b;
}
```

### 2. Configure `cx.toml`

```toml
[project]
name = "mathlib"

[workspace.targets.default]
libraries = [
  { name = "mathlib", entry = "mathlib.cxl" },
]
```

### 3. Build

```bash
cx build
```

This produces:
- `.internal/output/default/mathlib.o`
- `.internal/output/default/mathlib.h`

### 4. Use from C

**main.c:**
```c
#include <stdio.h>
#include "mathlib.h"

int main(void) {
    printf("3 + 4 = %d\n", add(3, 4));
    printf("3 * 4 = %d\n", multiply(3, 4));
    return 0;
}
```

**Compile and link:**
```bash
gcc -o main main.c .internal/output/default/mathlib.o
```

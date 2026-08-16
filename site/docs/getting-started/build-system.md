---
title: Build System
description: Project builds, cx.toml configuration, library targets, and C header generation.
---

# Build System

CX provides a built-in, opinionated project structure and supports multi-target compilation with outputs such as executables and libraries. Project settings live in `cx.toml` and can be overridden for an individual build through command-line options.

For compatibility reasons, CX allows for compilation in "single-file compilation mode", handling files as individual compilation units in the same manner C files are processed by compilers like GCC and Clang. Language semantics works mostly the same between modes, single-file compilation however heavily restricts the usage of "import" statements, only "std::*" standard library files may be currently used if compiling in this mode.

```bash
cx [args] file1.cx file2.cx file3.cx ...
```

Supported Arguments:
- `-c`: Disables linking and compiles to an object file instead of an executable
- `-o path/to/file`: Use custom output directory + file name

## Starting a Project

A CX project is a directory containing a `cx.toml` configuration file. It is recommended to begin with the compiler's provided base project template as such.

```bash
cx init <project_name>
```

This file creates a new directory entitled the provided project name, inside the directory you will find a `cx.toml` file and a `main.cx`.

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
    std::io::println("Hello, world!");
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

Each `[workspace.targets.<name>]` target can contain binaries, libraries, and
link dependencies.

| Field | Type | Description |
| --- | --- | --- |
| `binaries` | array of `{ name, entry }` | Executable targets. `entry` is a `.cx` source file. |
| `libraries` | array of `{ name, entry }` | Library targets. `entry` is typically a `.cxh` file. |
| `link` | array of `{ name, kind }` | External link dependencies. `kind` is `"system"`, `"static"`, or `"dynamic"`. |

## Building

When building, the compiler will search the working directory and any parent for a `cx.toml` file and build the project based on its provided configuration. If no file is found, the compilation will fail. Many settings in the project configuration can also be overwritten on individual builds.

```bash
cx build
cx build <target>
cx build --backend-llvm -O3
cx build --verbose
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
    └── module artifacts
```

Library targets and generated C headers are covered in [Libraries and C Interop](./c-interop.md).

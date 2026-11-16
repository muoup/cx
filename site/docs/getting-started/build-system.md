---
title: Build System
description: Project builds, chl.lua configuration, library targets, and C header generation.
---

# Build System

Project builds enable the module system and read their configuration from `chl.lua`. Every Chloro source module uses the `.chl` extension. The same module can be imported normally and selected for C header generation by a library target.

## Starting a Project

```bash
chl init my_project
cd my_project
chl build
```

The starter project contains `main.chl` and a Lua configuration:

```lua title="chl.lua"
package("my_project")
stdlib("std")

local main = target("my_project", "executable")
main:match("main.chl")
```

`package` names the package. `stdlib` mounts the compiler's standard library. `source_root("src")` optionally changes the module source directory; otherwise it is the directory containing `chl.lua`.

## Targets and Headers

Targets select source files with `:match(...)` and optionally remove matches with `:exclude(...)`. Both accept file paths and glob patterns relative to the project directory.

```lua title="chl.lua"
package("mathlib")
stdlib("std")

local lib = target("mathlib", "library")
lib:match("src/**/*.chl")
lib:headers("src/api.chl")
```

An `executable` target links a runnable program. A `library` target produces a relocatable object. Its `:headers(...)` patterns select source modules for generated C headers; they must also be target sources.

Header generation includes public `extern "C"` definitions and standalone named types, together with their required type dependencies. Selecting a module for a header does not change its symbol naming. See [Libraries and C Interop](./c-interop.md).

## Profiles

The default profile is `debug`. Configure it or create another profile in Lua:

```lua
debug_profile():optimize("O0")

local release = profile("release")
release:optimize("O2")
release:backend("cranelift")
```

## Building

The compiler searches the current directory and its parents for `chl.lua`. Build all targets or select them by name:

```bash
chl build
chl build mathlib
chl build --profile release --backend-llvm -O3
```

Build artifacts live under `.internal/<profile>/`. An executable is named after its target. A library produces `<target>.o`, with generated headers under `include/<target>/`, preserving source paths relative to the package source root.

## Single-File Compilation

Direct file compilation uses a C-like command line:

```bash
chl main.chl -o main
chl -c library.chl -o library.o
```

Imports in this mode are restricted to compiler-owned libraries. Use project mode for ordinary module imports. `-I <directory>` adds a header search path and `-D <name>[=value]` defines a preprocessor macro.

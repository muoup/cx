---
title: Build System
description: Project builds, chl.lua configuration, library targets, and C header generation.
---

# Build System

Project builds enable the module system and read their configuration from `chl.lua`. Every Chloro source module uses the `.chl` extension. The same module can be imported normally and selected for C header generation by a library target.

## Starting a Project

```bash
chloro init my_project
cd my_project
chloro build
```

The starter project contains `main.chl` and a Lua configuration:

```lua title="chl.lua"
package("my_project")
stdlib("std")

local main = target("my_project", "executable")
main:match("main.chl")
```

`package` names the package. `stdlib` mounts the compiler's standard library with the specified module prefix for use in import statements. `source_root("src")` optionally changes the module source directory; otherwise it is the directory containing `chl.lua`.

## Profiles

A profile defines the flags used to compile a project. While the build system allows for extra flags to be provided per-compilation, e.g. `cx build -O3`, it is recommended to prefer compiling based on profiles, as they allow for incremental compilation -- i.e. the reuse of compiled modules that have been unchanged since last compilation (to be implemented).

By default, every project has a defined `debug` profile. If no profile is provided at compilation time, `debug` will be chosen. If one wants to override the debug profile, it may be accessed via the `debug_profile()` function. You can also create a new custom profile using the `profile(name)` method. Any parameter not explicitly set will default to the base state of `debug`, note that modifying the `debug` profile will NOT change the default values used for other profiles.

Profiles should be modified using `profile:flag(args...)` syntax, supported functions of this shape include:

```lua
local new_profile = profile("release")

-- Change compilation backend 
-- Note: the compiler will error if not built with support of the specified backend
new_profile:backend("cranelift" | "llvm")

-- Change optimization level
new_profile:optimize("O0" | "O1" | "O2" | "O3" | "Ofast" | "Osize")

-- Disable automatically generated assertions during potentially unsafe operations
new_profile:no_assert(true | false)

-- Disable automatic compile-time analysis
new_profile:no_analysis(true | false)

-- Force all .chl files to provide explicit returns on non-void returning functions
new_profile:explicit_return(true | false)

-- Appends an `#define [symbol] [value]` macro to the preprocessor
new_profile:define("symbol", "value")
```

Example:

```lua
local debug = debug_profile();
release:backend("cranelift")

local release = profile("release")
release:backend("llvm")
release:optimize("O2")
```

## Targets

A target defines a desired compilation output, defined as a target type along with a list of files that need to be compiled and linked. A target is created, similar to profiles, using the `target(name, target_type)` method, and may be configured similarly.

```lua title="chl.lua"
package("mathlib")
stdlib("std")

local exec = target("cli", "executable")

-- Defines the base of the compilation
exec:match("main.chl")

exec:match("deps/c-library/*.c")

-- Excludes a file or files from the match
exec:exclude("src/not_this_file.c")

-- Note: one need not match all Chloro files, any dependency inferrable via 'import' statements
-- will be included in the final compilation.  

local lib = target("mathlib", "library")
lib:headers("src/api.chl")
-- 
```

### Executable

### Library

Header generation includes public `extern "C"` definitions and standalone named types, together with their required type dependencies. Selecting a module for a header does not change its symbol naming. See [Libraries and C Interop](./c-interop.md).

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

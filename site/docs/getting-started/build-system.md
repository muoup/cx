---
title: Build System
description: Project builds, chl.lua configuration, library targets, and C header generation.
---

## Single-File Compilation

One may compile without Chloro's built-in build system just as one would use `gcc` or `clang`. This makes the compiler an
experimental drop-in replacement for said compilers if desired, as the compiler can compile directly `.c` files.

```bash
chl main.chl -o main
chl -c library.chl -o library.o
```

Imports in this mode are restricted to compiler-owned libraries. Use project mode for ordinary module imports. `-I <directory>` adds a header search path and `-D <name>[=value]` defines a preprocessor macro.

## Starting a Project

A project is defined as a directory containing a `chl.lua` file. It acts as a streamlined alternative to "Single-File Compilation" making use of Chloro's built-in build system. One may start a project using the `chloro init` command:

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

## Building

The compiler searches the current directory and its parents for `chl.lua`. Build all targets or select them by name:

```bash
chl build
chl build mathlib
chl build --profile release --backend-llvm -O3
```

Build artifacts live under `.internal/<profile>/`. An executable is named after its target. A library produces `<target>.o`, with generated headers under `include/<target>/`, preserving source paths relative to the package source root.

## Package

A package corresponds to a `chl.lua` file, comprising of all of the targets, the library, and the mounts as defined by the script. All methods regarding access, defining what dependencies are needed, what C header includes are available, etc. are defined at the package-level such that each target can be compiled in a uniform manner, where each external symbol mentioned in a file corresponds to one meaning regardless of the target. 

```lua title="chl.lua"
package("my_project")

-- Mounts the standard library and makes it available to all files directly 
-- within the target compilation -- i.e. not including dependencies.
stdlib("std")

-- Imports an external package defined by some dependency/directory/chl.lua
-- see `Library` section below:
dep("dependency/directory")

-- Creates an include alias such that all headers in dependency/include are
-- available via `#include <dep/path/to/file.h>`
include("dependency/include", "dep")

-- Adds a link dependency to the provided object file path. Compilations of
-- all targets must link against the specified file.
object("artifacts/raylib.o")

-- Adds mandatory flags for the linking stage, equivalent to `gcc -l[link]` 
link("GL", "m", "pthread", "dl", "rt", "X11")

```

## Profiles

A profile defines the compiler flags used to compile a project. While the build system allows for extra flags to be provided per-compilation, e.g. `cx build -O3`, it is recommended to prefer compiling based on profiles, as they allow for incremental compilation -- i.e. the reuse of compiled modules that have been unchanged since last compilation (to be implemented).

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
local lib = target("mathlib", "ffi-lib")

-- Each file matched by a 'headers' call will produce a corresponding .h file in the output
-- directory containing all of the `extern "C"` public symbols available in the file. Any C
-- project may link to the compiled object file and use the C headers as-generated
lib:headers("src/api.chl")
```

### Executable

An executable target produces one executable file after compiling all project files and linking their objects together.

```lua title="chl.lua"
package("cli")
stdlib("std")

local exec = target("cli", "executable")

-- Defines the base of the compilation, these files *must* be compiled in the production of the executable.
exec:match("main.chl")

-- Also works with patterns and .c files
exec:match("deps/c-library/*.c")

-- Excludes a file or files from the match
exec:exclude("src/not_this_file.c")

-- Note: one need not match all Chloro files, any file imported via an `import` statement
-- from a matched file is automatically imported. 
```

### FFI Library

The other supported target is entitled 'ffi-library'. FFI (Foreign Function Interface) targets act as a built-in way for Chloro files to be usable cross-language. On compilation, it produces an object file along with autogenerated C headers that may be used alongside it. The generation is such that any C project that links against the produced object artifact may directly include and use the headers as-is.

C headers produced in this system will include all public `extern "C"` definitions. Any type dependencies that these symbols contain which are not marked `extern "C"` will still be included, but with their mangled symbol name.

```lua title="chl.lua"
package("mathlib")
stdlib("std")

local ffi = target("mathlib", "ffi-lib")

-- Each file matched by the provided pattern will be prompted to produce an according C header file. 
-- Any Chloro files it imports transitively or directly will also be compiled.
ffi:headers("src/api.chl")
```

## Library

Every package can optionally define its 'library' interface. Libraries act very similarly to targets but are not directly compilable, instead acting as the interface by which another module can interact with itself.

```lua title="chl.lua"
package("dependency")
stdlib("std")

local lib = library()

-- Files matched by lib:match define what files an external package which depends on this can import.
lib:match("*.chl")

```

Assuming the above script is in a subdirectory inner_lib/chl.lua, an outer script can depend on the package using the `dep(path, prefix)` method, making the publically visible modules (as defined above) importable by using the `prefix::` module prefix. 

```lua title="chl.lua
package("base")
stdlib("std")

-- For any file foo/bar.chl matched in dependency, files in this package may import it via `import lib::foo::bar`
dep("inner_lib", "lib")

...

```
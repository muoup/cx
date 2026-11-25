---
title: Modules and Visibility
description: "Imports, aliases, visibility sections, and C symbol names."
---

# Modules and Visibility

Chloro uses explicit imports and visibility sections.

## Imports

`import` makes public declarations from another module available to the current
module. Imported declarations can be referenced through their module namespace.

```chl
import std::io;
import math::vec;

int main() {
    std::io::println("hello");
    return 0;
}
```

Module paths map to file paths. For example, `std::io` resolves to `lib/std/io.chl`
or the equivalent path relative to the project root.

Related modules can be imported in one statement. Each leaf is expanded into a
normal module import, and an alias applies to every leaf:

```chl
import std::{collections::vector, optional, net::{udp, address}} as std;
```

This is equivalent to importing `std::collections::vector`, `std::optional`,
`std::net::udp`, and `std::net::address` separately with the same alias.
Trailing commas are allowed in grouped import lists.

During the namespace migration, imported public symbols are still also available
by their legacy unqualified names for compatibility with older code. New code
should prefer qualified names because imports are intended to become reachability
declarations rather than symbol copies.

## Import Aliases

An import can provide an alternative namespace with `as`:

```chl
import std::collections::vector as v;

v::vector(int) values = v::vector::new(int);
```

The special alias `_` places the imported module at the root namespace, allowing its public declarations to be referenced without the module prefix:

```chl
import std::collections::vector as _;

vector(int) values = vector::new(int);
```

Aliases may overlap, however any symbol name which refers to multiple definitions due to alias overlap cannot be used. However, if this occurs with `_`-aliasing and one definition refers to a symbol defined in the current module, the namespace symbol will deduce to that definition:

```chl
// Okay:
import std::{collections::vector, io::file, optional} as std;

std::vector(int) vec = std::vector::new(int);
std::file file = std::file::open("test.txt", "r")
    |> std::opt::unwrap();

// Error:
import std::collections::vector as mod;
import other::vector as mod; // Where `other` contains some definition of a `vector` type

mod::vector vec = mod::vector::new(); // Type Error: The symbol `mod::vector` is ambiguous

// Okay:
import std::collections::vector as _;

struct vector { ... };

vector vec = ...; // Okay: the current module's definition of `vector` takes precedence
```

Official examples typically put portable standard-library modules under the
`std` alias. Use `std::posix` for typed POSIX wrappers and `std::posix::raw`
for the underlying C bindings.

```chl
import std::{collections::vector, io::file, net::{address, tcp}} as std;
import std::platform::posix::socket as std::posix;
import std::platform::posix::base as std::posix::base;
```

## Visibility

Visibility is controlled by `public:` and `private:` section headers.
Declarations are private by default.

```chl
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

## C Symbol Names

For handwritten declarations, an `extern "C":` section disables namespace-based
name mangling without moving the declarations out of their Chloro module.
`extern "C":` is private by default, while `public extern "C":` makes subsequent
declarations visible to importers. `private extern "C":` is an explicit spelling
of the default, and a later `public:` or `private:` header returns to normal Chloro
name mangling.

Additionally, declarations produced while processing a `#include` use unmangled C linker names
for backward compatibility reasons. This mode is scoped to the included file,
including its nested includes; after the include ends, the previous Chloro naming and
visibility configuration is For handwritten declarations, an `extern "C":` section disables namespace-based
name mangling without moving the declarations out of their Chloro module.

```chl title="lib/c_stdio.chl"
#include <stdio.h>

public extern "C":

int puts(const char* text);

public:

void write_message(const char* text) {
    puts(text);
}
```

An include inherits the current visibility, so declarations are private when
included under the default visibility and public when included after a
`public:` header:

```chl title="lib/c_api.chl"
public:

#include "api.h"
```

Importers reference public included declarations through the owning module:

```chl
import c_api as c;

int main() {
    c::api_function();
    return 0;
}
```

Included declarations occupy the owning module's ordinary namespace, just like
handwritten `extern "C"` declarations. They are referenced without qualification
inside that module and through the module namespace by importers:

```chl
#include "api.h" // Declares the C function `status`.

int check_status() {
    return status();
}
```

An include behaves like a scoped `extern "C":` section except that it preserves
the incoming visibility. An explicit `extern "C"` declaration may redeclare a
symbol from an included header when its type and linkage-relevant attributes
match. Visibility may differ, in which case the most permissive declared
visibility is used.

An ordinary namespace-mangled Chloro declaration with the same name is not a
distinct binding. It conflicts with the included declaration because both
occupy the same module namespace but name different linker symbols. This permits
a private C header to be included and selected declarations to be published
through `public extern "C"`, as `puts` is in the first example, without creating
an additional shadow namespace.

Every Chloro module uses the `.chl` extension. Library targets can request a C header for an ordinary module; the header exposes its public `extern "C"` definitions and named types, together with the types they require. The module remains available through normal imports. See [Libraries and C Interop](../getting-started/c-interop.md).

---
title: Modules and Visibility
---

# Modules and Visibility

CX uses explicit imports and visibility sections.

## Imports

`import` makes public declarations from another module available to the current
module. Imported declarations can be referenced through their module namespace.

```cx
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

```cx
import std::vector as v;

v::vector<int> values = v::vector::new<int>();
```

The special alias `_` places the imported module at the root namespace, allowing its public declarations to be referenced without the module prefix:

```cx
import std::vector as _;

vector<int> values = vector::new<int>();
```

Aliases may overlap, however any symbol name which refers to multiple definitions due to alias overlap cannot be used. However, if this occurs with `_`-aliasing and one definition refers to a symbol defined in the current module, the namespace symbol will deduce to that definition:

```cx
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

```cx
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
name mangling without moving the declarations out of their CX module.
`extern "C":` is private by default, while `public extern "C":` makes subsequent
declarations visible to importers. `private extern "C":` is an explicit spelling
of the default, and a later `public:` or `private:` header returns to normal CX
name mangling.

Additionally, declarations produced while processing a `#include` use unmangled C linker names
for backward compatibility reasons. This mode is scoped to the included file, 
including its nested includes; after the include ends, the previous CX naming and 
visibility configuration is For handwritten declarations, an `extern "C":` section disables namespace-based
name mangling without moving the declarations out of their CX module.

```cx title="lib/c_stdio.cx"
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

```cx title="lib/c_api.cx"
public:

#include "api.h"
```

Importers reference public included declarations through the owning module:

```cx
import c_api as c;

int main() {
    c::api_function();
    return 0;
}
```

Included declarations occupy the owning module's ordinary namespace, just like
handwritten `extern "C"` declarations. They are referenced without qualification
inside that module and through the module namespace by importers:

```cx
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

An ordinary namespace-mangled CX declaration with the same name is not a
distinct binding. It conflicts with the included declaration because both
occupy the same module namespace but name different linker symbols. This permits
a private C header to be included and selected declarations to be published
through `public extern "C"`, as `puts` is in the first example, without creating
an additional shadow namespace.

Files with the `.cxh` extension serve as library entry points and use the same syntax as `.cx` files. The [Libraries and C Interop](../getting-started/c-interop.md) guide explains how their public declarations become C-compatible library exports.

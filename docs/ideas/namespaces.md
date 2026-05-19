# Namespaces

> AI-generated contents. This proposal was drafted with AI assistance and should be reviewed before being treated as language design doctrine.

## Goal

CX namespaces should preserve the language's C-superset philosophy while giving non-C-facing CX code reliable name isolation and mangling. The model should keep the invariant that one symbol maps to one definition. Imports must remain explicit dependency declarations; symbols from external modules must not be usable without an import.

## Core Model

A namespace is a node in a namespace graph. Each node may contain both:

- symbol data: types, functions, globals, templates
- child namespaces

A module path is also a namespace path. A file such as `std/vector.cx` contributes symbols to the canonical namespace `std::vector`, unless overridden by future explicit syntax.

A namespace node can therefore be both a module and a parent namespace. For example, `N1::N2.cx` may define symbols in `N1::N2`, while `N1::N2::N3.cx` may define symbols in a child namespace.

Conceptually:

```rust
trait Namespace {
    fn namespace_data(&self) -> NamespaceData;
    fn resolve_inner_namespace(&self, name: &str) -> Option<NamespaceRef>;
}
```

## Imports

Imports create namespace reachability, not symbol copies.

```c
import std::io;
import collections::vector as vec;
```

`import std::io;` permits qualified access through `std::io`, for example:

```c
std::io::println("hello");
```

`import collections::vector as vec;` creates a local namespace alias:

```c
vec::Vector<i32> values;
```

Aliases point to the same canonical namespace node. They do not duplicate symbols.

External definitions may not be referenced unless their namespace has been imported.

## Namespace Opening

CX may support whole-namespace opening:

```c
import std::io;
using std::io;

println("hello");
```

`using` creates an unqualified lookup view over a namespace. It does not mutate the current namespace and does not re-export symbols.

Individual imports are intentionally excluded from the initial design.

## Symbol Identity

A successful name resolution must produce exactly one definition.

Namespaces expose symbols as a partial function:

```text
QualifiedSymbolName -> DefinitionId
```

Merging namespace views preserves only single-valued bindings:

```text
merge(T -> A, T -> A) = T -> A
merge(T -> A, T -> B) = T -> Conflict(A, B)
merge(T -> A, empty)  = T -> A
```

A conflict binding is not a symbol. It records that unqualified lookup cannot produce a unique definition.

## Lookup Rules

Qualified lookup follows namespace graph edges, then resolves a symbol in the target namespace:

```c
N1::N2::T
```

Unqualified lookup checks:

1. Local/current namespace symbols.
2. Opened namespace view from `using`.
3. Error if no symbol is found or if the opened view contains a conflict.

Local definitions are real definitions and are not killed by opened namespace conflicts.

Example:

```c
import N;
using N;

struct T {
    i32 value;
};

T local_value;      // resolves to local T
N::T remote_value; // resolves to N::T
```

If two opened namespaces expose different `T` definitions, unqualified `T` is ambiguous unless the current namespace defines its own `T`.

## Overlapping Definitions

Two different definitions may not occupy the same canonical qualified symbol.

This is a hard error:

```text
module A defines N::T
module B also defines N::T
```

This is allowed, but creates ambiguity in an opened view:

```c
import A;
import B;
using A;
using B;

T value; // error if A::T and B::T differ
```

Qualified access remains valid:

```c
A::T a;
B::T b;
```

## C Interop and Mangling

Source namespaces and linkage/mangling policy are related but distinct.

Normal CX modules use their canonical namespace as part of symbol identity and should mangle namespace-qualified functions where needed.

C-facing modules may opt into the global namespace and unmangled C ABI behavior at the module level. `.cxh` files should default to this behavior, but the compiler should eventually represent it explicitly rather than treating the extension as the only mechanism.

Suggested defaults:

- `.cx`: namespace inferred from module path; CX mangling enabled.
- `.cxh`: global namespace; standard C-style functions unmangled by default.

Standard non-member, non-templated functions in C-linkage/global modules remain C-compatible without per-function annotations.

Templated functions, member functions, and other compile-time CX constructs remain non-C-interoperable unless lowered through an explicit wrapper.

## Implementation Direction

The compiler should introduce a structural qualified name representation before changing storage internals:

```rust
struct QualifiedName {
    namespace: Vec<CXIdent>,
    name: CXIdent,
}
```

or similar.

Interfaces should store canonical qualified definitions. Imports should add namespace graph edges and aliases, not eagerly merge imported symbols into the current flat map.

`using` should be represented as an overlay view used during unqualified lookup. Overlay conflicts should be represented explicitly, not resolved by deletion or arbitrary priority.

This preserves the invariant:

```text
symbol resolution either fails, is ambiguous, or returns exactly one DefinitionId
```

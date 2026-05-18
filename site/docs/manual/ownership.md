---
title: Ownership Attributes
---

# Ownership Attributes

CX ownership is explicit. Types opt into stricter behavior with attributes, and
code moves or discharges resources visibly.

## `@nocopy`

`@nocopy` disables implicit copying.

```c
struct Data : @nocopy {
    int value;
};

void consume(Data data) {}

int good() {
    Data data = (Data) { .value = 1 };
    consume(move data);

    data = (Data) { .value = 2 };
    consume(move data);
    return 0;
}
```

## `@nodrop`

`@nodrop` implies `@nocopy` and additionally forbids silent scope exit.

```c
struct Resource : @nodrop {
    int value;
};
```

A reachable scope exit with an undisposed `@nodrop` binding is a type error.

## `@leak`

`@leak(x)` marks a local `@nodrop` binding as discharged without transferring
ownership.

Current restrictions:

- The operand must be a local identifier.
- The binding must be a stack local.
- The binding must have `@nodrop`.
- Conditional `@leak` does not satisfy ownership discharge.

## `@unpack`

`@unpack(x) { field: binding, ... }` consumes a local struct binding and
introduces fresh local bindings for selected fields.

All direct `@nodrop` fields must be explicitly bound. Unbound non-`@nodrop`
fields are discarded with ordinary C value semantics.

```c
struct Container : @nodrop {
    Resource resource;
    int count;
};

void Container::drop(this) safe {
    @unpack(this) {
        resource: resource,
    };

    resource.drop();
}
```

Current restrictions:

- The operand must be a local identifier.
- The binding must be a stack local.
- The binding must have struct type.
- Duplicate field or binding names are rejected.

## `@adopt`

`@adopt(place)` unsafely creates an owned value from an existing addressable
memory place without copying the bytes at that place.

```c
Resource value = @unsafe(@adopt(*ptr));
value.drop();
```

The adopted place becomes the storage for the owned value. This is intended for
low-level data structures that manually manage initialized regions, such as
vectors dropping elements stored behind a pointer.

Current restrictions:

- `@adopt` is unsafe-only in safe code.
- The operand must be an addressable memory place.
- The operand may not be const.
- The adopted type must currently be memory-resident.
- Adopting a local binding directly is rejected; use `move` for local bindings.

## Control-Flow Merge Rules

Move state is tracked across:

- `if`
- `match`
- `switch`
- loops
- `break`
- `continue`
- `return`

An inconsistent move state at a reachable join is a type error. Later uses of
such a binding are also rejected.

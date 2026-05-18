---
title: Aggregate Types
---

# Aggregate Types

CX supports C-style structs and first-class tagged unions. Aggregate layout stays
close to C: structs follow C padding and alignment rules.

## Structs

Ownership attributes are attached after the struct name with `:`.

```c
struct Data : @nocopy {
    int value;
};
```

Multiple attributes are comma-separated:

```c
struct Resource : @nocopy, @nodrop {
    int value;
};
```

Since `@nodrop` implies `@nocopy`, the idiomatic form is:

```c
struct Resource : @nodrop {
    int value;
};
```

## Tagged Unions

Tagged unions are declared with `enum union`.

```c
enum union Output {
    integer :: int,
    fp      :: double,
    string  :: const char*,
    error   :: void
};
```

Variant construction uses `::`:

```c
Output o1 = Output::integer(42);
Output o2 = Output::fp(3.14);
```

The payload type may be any valid type, including typedef names and inline type
definitions. `void` represents a zero-byte payload.

```c
enum union Json {
    object :: JsonObject*,
    number :: f64,
    string :: const char*,
    null   :: void
};
```

Tagged unions support the `@copy_traits(T)` attribute, which propagates
copyability based on a realized type parameter:

```c
enum union opt<T> : @copy_traits(T) {
    some :: T,
    none :: void
};
```

They also support ownership attributes:

```c
enum union Result : @nodrop {
    ok  :: int,
    err :: Resource
};
```

## Attribute Hierarchy

The aggregate attribute hierarchy is:

- trivially copyable, with C-style value semantics
- `@nocopy`
- `@nodrop`

Containment must respect that order.

- A copyable aggregate may not contain a `@nocopy` or `@nodrop` member.
- A `@nocopy` aggregate may not contain a `@nodrop` member.
- A `@nodrop` aggregate may contain either.

This rule applies to struct fields and tagged-union variants, and the check is
transitive over realized member types.

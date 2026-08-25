---
title: Linear Resources
---

# Linear Resources

Complex types often need to manage resources that must be cleaned up exactly once. The `@nodrop` restriction prevents a value from falling out of scope and implies `@nocopy`. A value of an `@nodrop` type must be moved elsewhere or explicitly discharged after its resources are handled.

```cx
struct string : @nodrop {
    char* data;
    usize length;
    usize capacity;
};
```

## Deferred Expressions

`defer expression;` registers a `void` expression to run when the current lexical scope exits:

```cx
file handle = open_file(path);
defer close_file(&handle);

process_file(&handle);
```

Deferred expressions run in last-in, first-out order. They run on ordinary fallthrough and when control leaves their scope through `return`, `break`, or `continue`. A return value is evaluated and preserved before the deferred expressions execute.

```cx
int example() {
    defer log("outer");
    {
        defer log("inner");
        return compute_result();
    }
}
```

Here, `compute_result()` is evaluated first, followed by `log("inner")` and `log("outer")`; the preserved result is then returned. A deferred expression is associated with the scope where the `defer` statement executes, so a defer inside a loop iteration runs when that iteration leaves its scope.

`defer` does not create a runtime closure. Conceptually its operand is staged code retained by the enclosing scope and emitted along each path that exits that scope. Bindings referenced by the deferred expression must therefore remain valid until cleanup executes.

## Consuming Values

A function taking an `@nodrop` value by value assumes responsibility for it. This makes consuming associated functions a natural convention for cleanup:

```cx
void string::drop(string this) {
    free(this.data);
    @leak(this);
}

void use_string(string s) {
    puts(s.data);
    move s |> string::drop();
}
```

Passing `string&` instead only borrows the string, so the caller retains responsibility for eventually consuming it.

## `@leak`

After a cleanup function releases the resources held by an `@nodrop` value, `@leak(value)` kills the local binding without moving the value elsewhere. It asserts that allowing the underlying region to disappear is intentional:

```cx
void string::drop(string this) {
    free(this.data);
    @leak(this);
}
```

The compiler cannot verify that cleanup occurred before `@leak`; incorrect use can therefore produce resource leaks or other undefined behavior. In a `safe` function it must be enclosed by [`@unsafe`](./safe-functions.md#unsafe).

## Unpacking Linear Structs

When an `@nodrop` struct owns `@nodrop` fields, `@unpack` can consume the outer value and create separate owned bindings for selected fields:

```cx
struct Resource : @nodrop {
    int data;
};

struct Container : @nodrop {
    Resource resource;
    int count;
};

void Container::drop(Container this) {
    @unpack(move this) {
        resource: resource,
    };

    move resource |> Resource::drop();
}
```

Every `@nodrop` field must be bound by the unpack operation so that no linear resource is discarded with the outer value. Copyable fields may be omitted when they do not need separate handling.

## Adopting a Value

An edge case arises when dropping values that do not represent unique allocations or memory regions. Consider a vector over some `@nodrop` type `Inner`:

```cx
struct InnerVector : @nodrop {
    Inner* data;
    usize length;
    usize capacity;
};

void Inner::drop(Inner this) { ... }

void InnerVector::drop(InnerVector this) {
    for (usize i = 0; i < this.length; i++) {
        // data[i] produces a reference, but Inner::drop consumes an owned value.
        this.data[i] |> Inner::drop();
    }

    @leak(this);
}
```

We are unable to directly move `this.data[i]` here, as `move` is only valid on local variable identifiers, and in practice because `data` is one contiguous heap allocation, there is no ordinary binding that owns each individual element. `@adopt` is an unsafe escape hatch that upgrades a reference to an owned value for cases such as this:

```cx
void InnerVector::drop(InnerVector this) {
    for (usize i = 0; i < this.length; i++) {
        Inner element = @adopt(this.data[i]);
        move element |> Inner::drop();
    }

    free(this.data);
    @leak(this);
}
```

Ownership itself does not directly imply any memory-management action, so adoption can be sound in narrow circumstances. Adopting a value that already has a live owning binding is likely to produce undefined behavior. Uses of `@leak` and `@adopt` are recommended to remain inside small abstractions whose ownership behavior is well understood and encapsulated.

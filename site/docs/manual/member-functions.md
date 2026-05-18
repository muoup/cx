---
title: Member Functions
---

# Member Functions

CX supports namespaced member syntax without dynamic dispatch.

## Borrowed Receiver

An instance method declares `*this` as its borrowed receiver.

```c
struct Counter {
    int value;
};

void Counter::print(*this) {
    printf("%d\n", this->value);
}
```

Call syntax:

```c
Counter c;
c.print();
```

## Consuming Receiver

A method may consume its receiver with `this`.

```c
void Counter::drop(this) {
    @unsafe {
        @leak(this);
    };
}
```

A consuming receiver moves the instance it is called on. The method body must
discharge ownership by transferring it to another binding or by marking it as
discharged with `@leak`.

Current behavior:

- `x.drop()` consumes `x` when `x` is a whole binding.
- Owned aggregate rvalues may call consuming receivers.
- Non-binding places such as `obj.field.drop()` are currently rejected.

## Static Member Functions

A member declaration with no receiver is a static member function.

```c
struct MyStruct {
    int value;
};

MyStruct MyStruct::create() {
    return { .value = 42 };
}
```

Call syntax:

```c
MyStruct s = MyStruct::create();
```

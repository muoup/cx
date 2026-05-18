---
title: Safe Functions
---

# Safe Functions

`safe` marks a function body for safe-subset checking.

```c
int fn() safe
where
    post(ret): (ret == 1)
{
    return 1;
}
```

A `safe` function must use a subset of C and CX semantics that is suitable for
formal verification. The feature is still being stabilized, but the current
rules include:

- Raw pointers are not allowed in safe contexts.
- Safe code may call only other `safe` functions.
- Contracts in safe functions must be pure.
- `@leak` is unsafe-only in safe code.
- Pointers to external data passed into a safe function may not outlive the
  function's lifetime without future explicit annotation.

## Safe Usage of References

References are unsafe in most cases. General-purpose reference-like data should
use safe abstractions such as `ref<T>` in the standard library. The exact design
of `ref<T>` is still being iterated; currently, it may not be copied and should
not escape its intended lifetime without explicit annotation.

Native `T&` references are safe in a narrow case: a `T&` may be returned from a
safe function and used with an rvalue lifetime in the caller's context.

```c
import std::io;

struct Data {
    int value;
};

int& get_value(Data data) safe {
    return @unsafe(data.value);
}

int extern_function(int x) safe;

int main() safe {
    Data data = { .value = 42 };
    get_value(data) = 100;
    extern_function(data.value);
}
```

## `@unsafe`

`@unsafe` is the explicit escape hatch inside safe code.

Supported forms:

- `@unsafe(expr)`
- `@unsafe { ... }`

The unsafe island suppresses safe-subset checks for its enclosed subtree only.

## FMIR Verification

When the compiler is invoked with `--analysis`, safe functions are lowered to
FMIR and analyzed before ordinary MIR-to-LMIR lowering continues.

```c
int fn() safe
where
    post(ret): (ret == 1)
{
    int x = 0;
    return x;
}
```

Under `--analysis`, this is a compile-time verification error.

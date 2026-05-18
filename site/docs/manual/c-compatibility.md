---
title: C Compatibility
---

# C Compatibility

CX aims to be compatible with C99, but the compiler is still growing toward that
goal. Code that compiles with `clang -std=c99` or `gcc -std=c99` is the long-term
compatibility target.

Current unsupported or incomplete areas include:

- full C preprocessor support
- `goto`
- defining variadic functions with `<stdarg.h>`
- ternary operators
- `volatile`, `restrict`, and `inline`
- static function variables
- anonymous unions

Calls to libc functions such as `printf` are supported.

## Compatibility Shims

Some system-header compatibility is currently implemented with shims:

- `long double` maps to `f64`
- `_Complex float` maps to `f64`
- `_Complex double` maps to `f64`
- `_Complex long double` maps to `f64`
- `__builtin_va_list` is represented as `opaque(24)`

These aliases are declaration and typechecking shims. They are not ABI-correct
for storage layout, calls, complex arithmetic, or target-specific varargs.

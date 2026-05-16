# C Compatibility Notes

This file records intentional compatibility shims used while the compiler grows
enough C support to consume system headers directly.

## Complex and Long Double Types

`_Complex` is tokenized as an intrinsic keyword so existing parsing for
consecutive intrinsic keywords can produce type names such as `_Complex float`.
Those names are registered in `cx-mir/src/intrinsic_types.rs`.

The current MIR float model only represents `f32` and `f64`, so the following C
types are temporary compatibility aliases:

- `long double` -> `f64`
- `_Complex float` -> `f64`
- `_Complex double` -> `f64`
- `_Complex long double` -> `f64`

These aliases are only intended to let declarations in system headers parse and
typecheck. They are not ABI-correct for storage layout, calls, or complex
arithmetic. Revisit them when MIR grows explicit `long double`, `f128`, or
complex-number representations.

## Varargs Builtins

`__builtin_va_list` is registered as `opaque(24)` so GCC system headers can
declare `__gnuc_va_list`, `va_list`, and functions taking `va_list` parameters.

This is a declaration/typechecking shim only. It does not implement `va_start`,
`va_arg`, `va_copy`, `va_end`, or ABI-correct varargs lowering. Revisit the size
and representation when target-specific C varargs semantics are implemented.

## Internal Builtin Header

The preprocessor reads `lib/libc/internal/__builtins.h` before user source so
C-spelled compatibility macros can live in a C header instead of Rust code.

Macros in that file are intentionally typechecking shims. For example,
`__builtin_bswap16`, `__builtin_bswap32`, and `__builtin_bswap64` currently
expand to identity expressions. Replace them with real compiler intrinsics or
ABI-aware implementations when those semantics are supported.

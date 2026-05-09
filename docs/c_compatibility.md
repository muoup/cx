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

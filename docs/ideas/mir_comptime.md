# AI-Generated Document: MIR Comptime Evaluation Plan

This note records an implementation direction for MIR constant evaluation. It is
not a language specification and does not commit the compiler to a particular
surface syntax.

## Goal

MIR should own evaluation of concrete compile-time values. A single evaluator
should accept a function-like MIR body and constant arguments, execute its
basic blocks, and return an `MIRConstant` when the result is valid in the
requesting context.

The first consumers are global initializers and runtime-callable constexpr
functions. This replaces frontend-specific expression recognizers with a
semantic evaluator that sees the same coercions, aggregates, control flow, and
calls as code generation.

## Scope Boundaries

The compiler has two separate compile-time mechanisms:

- Constant evaluation computes concrete runtime values, including aggregate
  values and symbolic pointers. It belongs in MIR.
- Staged `comptime` and `emit` construct typed language artifacts before MIR
  exists. They remain a frontend mechanism until MIR has a representation for
  emitted expressions, places, types, and declarations.

`constexpr` functions can be both runtime-callable and MIR-evaluable. A
comptime-only function is never lowered into a runtime call site, although it
may use normal call instructions inside an evaluator-only MIR body.

The MIR evaluator belongs to `cx-thir-lowering`. It needs the lowering context
that owns the in-progress MIR module, type registry, global declarations, and
materialization requests, and its work is complete before the finished MIR
unit is handed to later MIR consumers. `cx-mir` owns the representation and
IDs, not the evaluator or THIR-lowering context.

## MIR Representation

Function metadata should distinguish these modes:

- `Runtime`: code generation only.
- `Constexpr`: code generation plus evaluation when every argument is known.
- `ConstOnly`: evaluator only and rejected by runtime MIR validation.

Const-only and constexpr bodies use ordinary `MIRFunctionDefinition` blocks,
with `MIRFunctionMode` carried by the enclosing `MIRFunction`. Global
initializers use private `ConstOnly` functions represented by a lowering-owned
materialization request. THIR to MIR lowering first materializes every function
and global-initializer body, then walks the global-initializer requests in their
declared order to evaluate them. These requests are not retained in the
finished `MIRUnit`; the materialized functions remain ordinary MIR functions
and are skipped by MIR to LMIR lowering, so an initializer is never emitted as
an ordinary runtime function merely to evaluate it.

The evaluator's public boundary is intentionally small:

```text
evaluate(unit, body, arguments) -> Result<MIRConstant, MIRConstEvalError>
```

The body can be an ordinary MIR function definition, including a private
global-initializer function, or a future inline const expression lowered into a
synthetic entry block.

## Evaluation Values

`MIRConstant` is the result format, not the evaluator's complete internal
value model. Evaluation also needs temporary allocations and symbolic pointers:

- scalar integers, floats, booleans, and unit;
- aggregate allocations with typed fields and byte offsets;
- pointers to temporary allocations;
- relocatable pointers to globals and functions, each with a byte offset;
- undefined or uninitialized storage tracked explicitly for diagnostics.

At the boundary, temporary pointers may only escape where the destination
context permits them. Static initializers may contain null pointers, function
references, global references, global offsets, and aggregates containing those
values. A pointer to an evaluator-local allocation is rejected.

## Execution Model

The evaluator starts at an entry block with a register/place environment and
interprets the ordinary MIR instruction set in phases:

1. Implement literals, assignments, aggregate construction/projection,
   arithmetic, coercions, and return.
2. Add create/address/dereference and typed temporary allocation handling.
3. Add branches, jump-table dispatch, and block arguments.
4. Add calls to `Constexpr` and `ConstOnly` functions.

Runtime-only operations, external calls, variadics, `emit`, and observable
side effects fail with a source-ranged const-evaluation diagnostic. The
evaluator should report the active function/block/instruction stack, rather
than panicking on an unsupported instruction.

## Recursion and Initialization Order

Recursive constant functions are valid when they terminate. Cache completed
calls by function ID and canonical constant arguments, track active call keys,
and enforce an instruction budget. Re-entering the same active key is a cycle;
calling the same function with different arguments is ordinary recursion.

Global addresses are symbols, so they do not require eager evaluation of the
target global. This permits forward aliases and self-referential pointer
initializers. Reading a global's value during its own unfinished initialization
is an evaluation cycle and must fail unless a later language rule explicitly
defines that behavior.

Backend emission still declares every global before assigning initializers.
This is independent of evaluator order: a successful `MIRConstant::Global`
must remain a relocation even if its target is declared later in source order.

## Migration Plan

1. Keep the current symbolic global initializer lowering for C relocations and
   route its constants through a shared MIR constant representation.
2. Introduce evaluator types and diagnostics without changing existing staged
   comptime behavior.
3. Lower simple constexpr/global initializer bodies into private const bodies
   and evaluate scalar and aggregate results.
4. Add memory, control flow, calls, caching, and recursion handling.
5. Remove the THIR scalar comptime evaluator from global-initializer
   canonicalization once all supported initializer forms lower to MIR.

## Layout Ownership

Type layout is a pure MIR type-registry query, not an evaluator operation.
THIR lowering should eventually stop precomputing layouts and let MIR derive
them from its own type definitions. `sizeof` and `alignof` then lower to MIR
constants through that registry, while the evaluator only handles values and
control flow.

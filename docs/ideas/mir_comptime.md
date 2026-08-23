# AI-Generated Document: MIR Comptime Evaluation Plan

This note records an implementation direction for MIR comptime evaluation. It is
not a language specification and does not commit the compiler to a particular
surface syntax.

## Goal

MIR should own evaluation of concrete comptime values. A single evaluator
should accept a function-like MIR body and comptime arguments, execute its
basic blocks, and return an `MIRConstant` when the result is valid in the
requesting context.

The first consumers are global initializers and runtime-callable comptime
functions. This replaces frontend-specific expression recognizers with a
semantic evaluator that sees the same coercions, aggregates, control flow, and
calls as code generation.

## Scope Boundaries

The compiler has two separate compile-time mechanisms:

- Comptime evaluation computes concrete runtime values, including aggregate
  values and symbolic pointers. It belongs in MIR.
- Staged `comptime` and `emit` construct typed language artifacts before MIR
  exists. They remain a frontend mechanism until MIR has a representation for
  emitted expressions, places, types, and declarations.

`Comptime` functions can be both runtime-callable and MIR-evaluable. A
`ComptimeOnly` function is never lowered into a runtime call site, although it
may use normal call instructions inside a comptime MIR body.

The evaluator belongs in `cx-mir-comptime`. It depends on MIR, not THIR, so it
can be used by THIR lowering, MIR analysis, and MIR-to-LMIR lowering. The
passes that schedule evaluation own the surrounding compiler context; the
engine executes one MIR body and returns an `MIRConstant`.

## MIR Representation

Function metadata should distinguish these modes:

- `Runtime`: code generation only.
- `Comptime`: code generation plus comptime evaluation when every argument is
  known.
- `ComptimeOnly`: comptime evaluation only and rejected by runtime MIR
  lowering.

Comptime and comptime-only bodies use ordinary `MIRFunctionDefinition` blocks,
with `MIRFunctionMode` carried by the enclosing `MIRFunction`. Global
initializers use private `ComptimeOnly` functions. A global in
`MIRGlobalState::Initializer` holds the function ID until a comptime pass
replaces it with `MIRGlobalState::Initialized`; this is a value representation,
not a request or a lifecycle state machine.

The evaluator's public boundary is intentionally small:

```text
MIRComptimeEngine::evaluate(function, arguments)
    -> Result<MIRConstant, MIRComptimeError>
```

The body can be an ordinary MIR function definition, including a private
global-initializer function, or a future inline comptime expression lowered into a
synthetic entry block.

Calls carry an explicit `MIRCallKind`: runtime calls are lowered to runtime
code, while comptime calls must be consumed by a comptime pass before LMIR
lowering. A callee's function mode remains a validation constraint, not an
implicit rewrite of an ordinary call.

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
4. Add calls to `Comptime` and `ComptimeOnly` functions.

Runtime-only operations, external calls, variadics, `emit`, and observable
side effects fail with a source-ranged comptime diagnostic. The
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
3. Lower simple comptime/global initializer bodies into private comptime bodies
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

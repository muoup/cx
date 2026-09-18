# Temporary staged-expression design handoff

**AI-GENERATED conversation summary — 2026-09-17. TO BE REMOVED after this design is resumed or incorporated into maintained documentation.**

This note preserves a design discussion, not an implementation specification or a completed change. Compiler inspection during the conversation was read-only. The user subsequently authorized writing this temporary note. No implementation or test execution was authorized by that request.

## Motivation and existing behavior

The compiler is undergoing an internal rewrite. The discussion concerned staged expressions in `cx-thir-lowering`, especially `MIRFunctionBuilder`'s optional parent construction path and its `outer_return_type` / `outer_yield_type` fields.

The behavior to preserve is `std::opt::try`: a comptime function returns a staged expression that either yields the optional's value or returns `opt::none()` from the enclosing runtime function. Convenient template deduction currently depends on that runtime caller context.

The longer-term goal is to check generic function bodies once and specialize MIR per template input, rather than instantiate and typecheck each generic body again in THIR. Dependencies on ambient caller types should become explicit symbolic inputs where needed, rather than hidden context affecting otherwise identical instantiations.

The source audit found an existing guard against the feared cross-caller reuse bug:

- `complete_comptime_call` incorporates enclosing return and yield types into the comptime symbol name using `mangle_comptime_context`; realization deduplicates by that name. Different enclosing return types therefore already distinguish instantiations through this path. This also specializes functions for context they may not use.
- `lower_comptime_function` starts a fresh builder and obtains external return/yield types from `THIRComptimeFn.context`. Parent builder inheritance is used for staged captures and scratch evaluation functions, rather than directly borrowing a caller builder for every comptime function.
- `THIRComptimeValueType` describes the ordinary result type and staged parameters, but external return/yield context is stored separately in `THIRStagingContext`.

These were source findings, not a reproduced soundness failure. The checkout contained ongoing compiler edits, so verify these details before relying on them during implementation.

## Chosen metadata syntax and meaning

The user selected:

```cx
expr<return = R, yield = Y> T
```

`T` is the ordinary value produced by the expression. `R` and `Y` provide external return/yield type context for constructing staged code. They do not assert that an exit will execute, and they are not intended to be an exhaustive effect summary of all code that can be composed into the expression.

In particular, absence of `return = R` must not imply that the expression cannot return externally: an unannotated staged parameter can contain an already checked return that its recipient merely forwards.

A possible spelling of `opt::try` under the chosen syntax is:

```cx
comptime expr<return = opt<U>> T opt::try<T, U>(expr opt<T> self) {
    return emit match (self) {
        opt::some<T>(value) => yield move value;
        opt::none<T>() => return opt::none<U>();
    };
}
```

This was an illustrative proposal, not implemented syntax. `T` is deduced from the input optional; `U` is deduced by matching the enclosing runtime function's return type against `opt<U>`. Extracting an `int` inside a function returning `opt<string>` should remain possible. A bare, unconstrained `return = R` would not itself establish that `opt::none()` can produce `R` when checking a generic body once.

Yields handled inside the emitted `match` produce its ordinary result; they are distinct from yields escaping the fragment to an external yielding scope.

## Break/continue and parameter metadata

The user does not want mandatory break/continue metadata. Materialization can reject an escaping break or continue when its destination is invalid. Future opt-in `nobreak` / `nocontinue` restrictions remain compatible with the chosen syntax.

The relevant check concerns the generated code, not proving whether an exit will execute at runtime. Comptime construction may eliminate a branch before validation; an ordinary runtime condition generally does not excuse an invalid remaining exit. Exactly predicting runtime behavior is not required.

The assistant initially claimed staged parameters needed effect propagation in their types. That claim was corrected: under the user's intended model, mandatory parameter metadata is unnecessary merely to accept, forward, or insert staged code.

```cx
comptime expr T identity<T>(expr T value) {
    return value;
}
```

`identity` needs the ordinary result type `T`, but does not need the external return type of a return statement already contained in `value`. The statement was checked during construction, and composition is validated at materialization. By contrast, a helper that constructs a new `return opt::none()` or invokes a helper requiring an external return type needs enough context to typecheck or deduce that new operation.

The intended model resembles duck typing at composition boundaries: comptime code assembles fragments with limited type awareness, and the final code must work in the runtime caller context. Parameter metadata could later provide opt-in API restrictions or document invariants, but is not required as a complete behavioral contract.

## Crucial evaluation-context clarification

Do not conflate lexical placement inside a runtime function with evaluation in runtime context.

The user clarified that arguments to a comptime function are evaluated in a **comptime context**. A staged expression rvalue passed as an argument therefore does not qualify for yield template deduction from the lexically surrounding runtime scope. It is being constructed or forwarded, not immediately materialized there.

The outer comptime call's result, when consumed in runtime context, is eligible for yield deduction from the destination's available yield type. This is the intended boundary:

- Construction or forwarding in comptime context leaves the eventual destination open.
- Immediate materialization in runtime context can supply the destination's expected yield type.

The assistant's objection involving a staged argument passed to a helper was withdrawn after this clarification; it had conflated lexical location with evaluation context.

Currently, staged expressions in runtime functions are rvalues materialized on the spot; the user reports that runtime functions do not yet support storing them in comptime variables for later materialization. If that is added, evaluating the right-hand side of a comptime variable assignment would occur in comptime context and would not receive the immediate-runtime yield deduction described above.

When a destination's yield type is itself unknown, contextual deduction still needs an annotation, constraints, or another source of type information. An independently typed yielded value can instead be checked or adapted at materialization.

## Proposed lowering redesign

The user's preferred direction is immutable staged-expression accumulation as a DAG. Conceptually, a staged value references a `THIRExpression` plus captured bindings to other expressions or evaluated comptime values such as `MIRConstant`s. Materialization recursively lowers the retained THIR directly into the destination function.

This would replace premature lowering into staged MIR generation buffers followed by MIR instruction translation/remapping. The current `MIRStagedValue` already shares templates and bindings using `Arc`; the substantive change is retaining THIR until final placement, not merely introducing shared graph storage.

Lightweight checking during THIR-to-MIR lowering would validate placement-dependent requirements, including return/yield compatibility and valid exit destinations. This deliberately allows a checked fragment to retain unresolved composition requirements. It should not require repeating ordinary name resolution or overload selection for already checked code.

The user proposed saving the old scope stack, creating the expression's own context, recursively lowering nested staged expressions, and restoring the previous context. The assistant identified an implementation distinction to preserve: captured lexical bindings belong to the fragment, but control-flow destinations and cleanup must remain connected to the materialization context. Literal replacement of the complete scope stack could hide caller defers and exit targets.

Specific implementation considerations suggested during the discussion, not yet implemented or fully settled:

1. Use stable binding identities such as `THIRLocalID` or dedicated capture IDs, rather than variable names, to preserve shadowing and hygienic capture. Each insertion needs fresh mappings for locals declared inside the fragment.
2. Distinguish a nested code fragment from an already evaluated value and from a runtime place. Replacing a captured value/place with its original source expression could repeat evaluation or recreate storage. DAG sharing must not implicitly memoize runtime execution.
3. Preserve captured runtime place identity and lifetime. The existing capture path distinguishes runtime places from value captures; a redesign must retain those semantics. Captures must not become dangling references to expired comptime evaluation frames.
4. Use a fresh lexical binding environment with destination-linked control scopes, or an equivalent explicit representation. An escaping return must clean up both fragment-local scopes and the relevant enclosing caller scopes.
5. Keep the fragment immutable. Adapting an escaping `i8` yield to an `i16` destination should add a permitted conversion at the exit or create an adaptation node. It should not retype interior arithmetic and change its semantics. Converting a block's ordinary result is a separate operation from adapting an external yield.
6. Preserve source locations and materialization provenance. A useful diagnostic would identify the original yield and explain: "Attempted to materialize staged expression with a yield statement in a non-yielding context", with the insertion site and, where useful, intermediate composition sites.

The permissive model motivates deferred validation; it was not established that all more restrictive alternatives are undecidable. Conservative earlier checks are possible but would accept fewer compositions or require stronger contracts.

## Deduction and remaining design boundaries

Signature-based deduction is the initial straightforward route: declared external types become symbolic generic inputs, matched against the available runtime destination during eligible calls. This avoids checking a generic body under each concrete caller's ambient context.

Inferring metadata from a generic body once is also conceptually possible: collect symbolic constraints and generalize them into a stored signature. Context-dependent overload resolution or ambiguous template deduction may still require annotations. This was discussed as a compatible future option, not selected as an immediate implementation requirement.

Deferring THIR lowering alone cannot solve expressions that lacked enough type information to construct THIR. For example, `yield opt::none()` may require an expected type for template deduction, whereas `yield already_typed_value` can retain its own type until placement. Supporting the former without annotations would require explicit deferred typing obligations.

Other details still needing concrete decisions include the representation of destination-linked materialization frames, the exact binding rules for nested external yields, the coercions permitted at exit boundaries, and where staged DAG storage lives so that reusable MIR does not unnecessarily depend on THIR ownership or lifetimes.

## Source pointers from the audit

- `compiler/cx-typechecker/src/type_checking/op/binop/calls.rs`: comptime call completion, context specialization, staged argument handling.
- `compiler/cx-typechecker/src/environment.rs`: `staging_context()` and expected yield context.
- `compiler/cx-typechecker/src/type_checking/staged_expr.rs`: staged body checking and expected types.
- `compiler/cx-thir/src/thir/data.rs`: comptime prototype and staged value type representation.
- `compiler/cx-thir/src/thir/comptime.rs`: `THIRStagingContext` and staged expressions.
- `compiler/cx-thir/src/thir/name_mangling.rs`: context-dependent symbol mangling.
- `compiler/cx-thir-lowering/src/builder.rs`: capture construction and runtime place/value distinctions.
- `compiler/cx-thir-lowering/src/builder/function.rs`: parent inheritance and scope state.
- `compiler/cx-thir-lowering/src/lowering.rs`: comptime function setup and return/yield lowering.
- `compiler/cx-thir-lowering/src/lowering/control_flow.rs`: cleanup and exit destination handling.
- `compiler/cx-thir-lowering/src/lowering/staged.rs`: current staged MIR instantiation.
- `compiler/cx-mir-comptime/src/value.rs`: shared staged templates and captured bindings.
- `lib/std/optional.cx`: `opt::try`, `try_or`, and related staged helpers.

**End of temporary AI-generated handoff. Remove this file after use.**

# Reference lifetime implementation plan

This document records the proposed v0 semantics and the implementation sequence.
It is a design plan, not a statement that lifetime checking is implemented.

## Scope

V0 adds ephemeral, static, and parameter-relative references; inferred local
reference bindings; explicit safety contexts; and conservative reference
dependency analysis. Ordinary stack access follows fixed language rules.
Libraries continue to establish richer borrowing arrangements through explicit
APIs, unsafe implementations, staged expressions, and linear borrowing objects.
There are no implicit library hooks on ordinary local declarations.

Lifetime information does not change runtime layout or ABI and does not create
new template specializations. Function signatures describe their lifetime
relationships explicitly. Local analysis checks those relationships without
revisiting ordinary type inference.

## Safety contexts

An unannotated function is non-safe. A safe function starts in a safe context.
Both expression and block forms of `@nonsafe` and `@unsafe` are supported. The
innermost annotation determines permission, and leaving it restores the prior
context. In particular, `@nonsafe` inside `@unsafe` removes unsafe permission.

| Operation requirement | Safe context | Non-safe context | Unsafe context |
| --- | --- | --- | --- |
| Safe | Allowed | Allowed | Allowed |
| Non-safe | Rejected | Allowed | Allowed |
| Explicitly unsafe | Rejected | Rejected | Allowed |

Preserve the distinction between a function's public safety contract and the
lexical permission at a particular operation. A safe function containing an
unsafe implementation remains a safe callable. Permission is not a lifetime
guarantee and must not erase known reference dependencies.

Instruction source metadata should carry the effective safety context. Keep
this on the instruction rather than adding unrelated flags to each instruction
variant. Captured, instantiated, and deferred code must preserve the context of
the operation, with local annotations applied explicitly; a call site's unsafe
context must not silently bless another function's implementation.

`@leak` requires non-safe permission. Invalidating storage with live dependent
references requires unsafe permission, including when invalidation is caused by
a leak. Existing non-safe-operation diagnostics and explicitly-unsafe-operation
diagnostics must remain distinguishable.

## Reference categories and conversions

The source spelling is `T '_&`, `T 'static&`, or `T 'a&`; an unannotated `T&`
remains the free-reference spelling in signatures. Lifetimes in signatures have
their own namespace and are not template arguments. Named output bounds must
be supplied by input lifetime contracts rather than invented by an output type.

Identifier expressions initially provide ephemeral access to their places.
Reading a copyable value performs a copy; copying a value with embedded
references must retain those references' dependencies. Pointer dereferencing
and indexing remain outside the initial safe access subset.

An ephemeral view can become a const bounded reference when its storage and
access dependencies support the requested bound. The cast creates a new view,
not a new storage lifetime. An expected return type cannot make a local variable
outlive its function. A parameter's reference targets the caller's referent, not
the callee's parameter slot.

An identifier-produced temporary view of an owned local may be retained by
creating a dependency on that local's storage. An access grant that a library
limits to an invocation or expression cannot be extended merely because the
underlying allocation lives longer. Retain both storage and access dependencies.

Converting to a free reference requires non-safe permission. It must not remove
constness. Widening a promised bound or manufacturing a relation that cannot be
established is not an ordinary implicit safe conversion.

Local reference declarations are included in v0. Their reference lifetime is
inferred from the initializer, and explicit lifetime annotations in local
declarations are rejected. The declaration's mutability still applies: an
ephemeral source cannot become a mutable bounded reference through inference.
Existing bounded references supplied by a library retain their bounds and
mutability when bound locally. The non-safe free-reference route remains
available for unchecked code.

Concrete local region identifiers must not enter template cache keys or symbol
mangling. Semantic callable compatibility must nevertheless compare lifetime
contracts, with bound names compared modulo renaming. Lifetime-bearing type
arguments and aggregates require lifetime slots separate from runtime type
identity; unsupported combinations must be diagnosed until their propagation
is implemented.

## Assignment and evaluation

The assignment target is a place. Assignment does not route its left-hand side
through general implicit reference conversion. An ephemeral writable access is
consumed by the store. The assignment result must not export that writable
access: in the safe subset it can expose a const ephemeral result for reading
and chained assignment.

Evaluation order is callee first, then arguments fully evaluated left to right.
Assignment evaluates its target, fully evaluates its source, checks replacement,
and stores. Full evaluation includes materializing copies when subsequent
evaluation could change their source. Merely accumulating `Copy(place)` operands
until a later instruction does not implement this rule. Ownership transfer from
a move must likewise occur at the move expression's evaluation point, rather
than being deferred past later argument side effects. Materializing a moved
value transfers its obligation to the result; it must not duplicate ownership.

A reference argument captures a reference, not a snapshot of its referent. A
value argument captures its owned value before the next argument executes.
Self-assignment and reads of the assignment target on the right-hand side remain
supported. The target must remain valid across evaluation of the source.

## Ownership and invalidation

Use one semantic invalidation operation with explicit reasons. An initial MIR
implementation may retain inline move operands and dispatch them to that same
operation, while replacement receives an explicit invalidation instruction.
Separate normalization into atomic instructions can follow after evaluation
order and ownership transfer are specified; it must not create a read from an
already-invalidated source or silently discard an owned value.

| Reason | Ownership effect | Reference effect |
| --- | --- | --- |
| Move | Transfers ownership; valid for `@nodrop` | Invalidates dependencies on the old region |
| Scope end | Discards a remaining value; rejects live `@nodrop` | Ends storage after scoped views finish |
| Replacement | Cannot discard a live `@nodrop` value implicitly | Checks the views affected by replacement |
| Leak | Explicitly discharges the ownership obligation in a non-safe context | Still checks live dependent references |

`@nodrop` constrains disposal, not every end of a storage lifetime. A move is a
valid transfer. An unsafe context does not silently substitute for explicit
discharge of a linear value.

Ordinary same-type writes can preserve references to a whole object and its
stable fields. A view into a tagged union's active payload also depends on that
payload remaining valid. V0 conservatively treats union replacement as
invalidating active payload views, without invalidating references to the whole
union. The assignment's own destination access does not conflict with itself.

Borrowed pattern bindings are introduced only on the matching control-flow
edge. Their dependency includes the selected payload, and initially remains
active for the arm's lexical scope. Owned matches transfer payload ownership
instead. No tuple feature or general output-parameter inference is needed for
native patterns.

## Analysis model

The current MIR pass is forward ownership analysis, not backwards liveness.
Reuse its worklist and conservative joins, while retaining distinct state for
ownership and reference dependencies.

Track storage identity, reference bounds, possible origins, and any access or
payload dependencies. A reborrow preserves its source's dependencies; a
relative result receives the dependencies required by its signature. A local
binding does not create an independent owner for its referent.

An edge from a view to its sustaining region means that the view cannot promise
validity beyond that region, and that invalidating the region while the view is
active requires unsafe permission. A view can have multiple sustaining regions.
Keep the region of a local's current owned value distinct from a later value
initialized in the same local slot; reinitialization cannot repair old views.
Acknowledging an invalidation suppresses that operation's dependency conflict,
but does not undo the ownership move, lengthen an annotation, or erase origins.

At joins, retain all possible dependencies and require availability on every
incoming path. Loops use a fixed point. Initial bounds are lexical, expression,
static, or signature-relative; v0 does not infer last-use shortening. Scope exit
ends the corresponding views before checking disposal of their owners.

Nested references need separate treatment for the lifetime of the reference
slot and the lifetime carried by the stored reference. Loading a stored
reference must preserve the latter. A shared lifetime name does not establish
that two arguments alias; dependency summaries may conservatively retain more
than one possible origin.

## Sequential implementation slices

One Luna agent implements one slice at a time. The main agent specifies each
slice, reviews its diff, and owns semantic decisions and broader validation.

1. Finish the existing syntax groundwork: lex lifetime modifiers without
   breaking character literals, parse all reference declarator routes, preserve
   HIR annotations, repair exhaustive matches, and update grammar coverage.
2. Introduce the three permission levels and `@nonsafe` in parsing and
   typechecking. Replace unsafe nesting depth with context restoration, keep
   callable safety separate, and distinguish non-safe and unsafe diagnostics.
   Check nested blocks, expression annotations, and restoration after errors.
3. Preserve effective permission in MIR instruction metadata, including
   captured, instantiated, and deferred code. Check the metadata directly;
   later invalidation checks must not reconstruct permission from a function's
   public contract or the scope where staged code happens to be instantiated.
4. Establish full evaluation and snapshot semantics for value operands before
   introducing invalidation events. Cover scalar and aggregate arguments,
   assignments, and staged execution.
5. Represent semantic reference categories and signature lifetime schemas.
   Preserve annotations through completion, formatting, callable compatibility,
   and template handling without lifetime-driven specialization. Reject named
   output bounds without input binders and unsupported lifetime-bearing types.
6. Centralize ownership invalidation and add replacement events. Preserve move
   transfer semantics, check `@nodrop` overwrite, and carry source permission
   into all events without yet duplicating reference rules across instructions.
7. Add storage regions, view dependencies, and conservative CFG joins to the
   ownership worklist. Establish full-expression and lexical end events, and
   check move conflicts with direct stack views before adding escape routes.
8. Implement ephemeral and static reference conversions. Preserve constness
   and access-grant bounds, distinguish implicit stack views from library
   grants, and prevent promotion of locals to static storage.
9. Add inferred local reference bindings. Derive their dependencies from the
   initializer, reject explicit local lifetime inputs, and retain dependencies
   through copies, rebinding, branches, and lexical scope exit.
10. Instantiate signature-relative bounds at calls and check returned views
    against the declared contract. Cover forwarding, multiple possible origins,
    local return rejection, and bounded-to-free conversions.
11. Add projection dependencies and tagged-union payload views. Bind pattern
    views only on successful edges and distinguish payload replacement from a
    stable write to an ordinary object or a whole-union reference.
12. Propagate dependencies through reference-containing values and nested
    references supported by v0. Reject unsupported wrappers explicitly so they
    cannot erase bounds by storing or copying a reference.
13. Integrate reference dependencies with staged capture, instantiation,
    deferred code, and nonlocal exits. Until then, diagnose lifetime-bearing
    staged paths that cannot preserve those dependencies.
14. Update the manual and examples, and complete default-backend, LLVM, native
   grammar, grammar WASM, and extension WASM validation. State any baseline
   failures separately.

Each slice needs focused positive and negative behavior checks before the next
slice starts. Final regression coverage must include local return rejection,
relative forwarding, bound shortening, free-reference conversions, constness,
inferred locals, scalar mutation during const access, move conflicts, payload
replacement, `@nodrop` transfer/disposal, nested safety contexts, branches,
loops, and staged exits. Syntax-only acceptance is not lifetime safety evidence.

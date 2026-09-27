# AI-generated handoff: reference lifetimes, safety tiers, and syntax migration

**AI-GENERATED PLANNING NOTE — NOT AN IMPLEMENTED LANGUAGE SPECIFICATION.**

This file records decisions from the September 2026 design discussion for later implementation sessions. It is separate from the human-written, unfinished `docs/ideas/reference_lifetimes.md`; do not edit that document to reconcile differences. `docs/TEMP_AI_GENERATED_staged_expression_design.md` records the broader future effect and staged-expression design. The decisions below supersede conflicting older AI notes about free and static references. Check the active compiler state before implementation.

## Scope and guiding rule

This branch should migrate the surface syntax, add a small three-tier safety model, and implement a conservative lifetime subset. It should preserve C's ability to make unchecked claims in nonsafe code. The compiler checks lifetime relationships that an API declares; libraries define richer access, aliasing, and mutation protocols. There is no universal Rust borrow checker and no implicit exclusive-access promise on `T&`. A general effect algebra, fully checked safe templates, and templated permissions remain outside v0.

The central distinction is between **permission to use a reference** and **evidence that producing it was sound**. A free reference has no compiler-enforced lifetime restriction on its use. A safe producer of a free reference must establish that the reference can remain usable wherever that unrestricted contract permits it. A nonsafe producer can make the same claim without verification; responsibility for a false claim remains at that nonsafe boundary. Forwarding an existing free reference does not make a new claim and is safe in itself. An `@safe` consumer can rely on its input contract; a nonsafe caller can violate that contract, just as it can violate other safety preconditions.

Do not reintroduce a separate `'static` lifetime *semantics* merely to express an unrestricted lifetime. Under this model, a free reference produced safely carries the intended program-long usability contract. Free references produced nonsafely have the same source type and permitted uses, but their producer may not have justified the claim. Whether the old `'static` spelling is rejected or accepted as a transitional alias remains undecided.

## Syntax for the first implementation

```cx
comptime expr<T> pass<T>(expr<T> value) {
    return value;
}

struct vec<T> [@nodrop] { ... };
struct rcell<T> [@nodrop, @unsafe_move] { ... };

int f() @safe {
    with [@nonsafe] { ... }
    with [@unsafe] { ... }
    return with [@unsafe](some_expression);
}
```

The examples above show intended syntax, not complete function bodies.

- Replace `expr T` with `expr<T>` in comptime function signatures. V0 does not add `expr<T>` as a general type in comptime local declarations. The exact revised spelling of parameterized staged signatures still needs a separate decision.
- Replace aggregate `: @nodrop, @nocopy, @unsafe_move` attributes with bracketed attributes. Add `@nonsafe_move`. In v0 these map to concrete existing-style checks, not a generic effect algebra.
- Replace trailing `safe` with trailing `@safe`. `@safe` is a function contract marker, not an effect-list member.
- Replace `@unsafe { ... }` and `@unsafe(expr)` with `with [@unsafe] { ... }` and `with [@unsafe](expr)`. Support the corresponding `@nonsafe` forms. The inner `with` scope sets its exact tier: `with [@nonsafe]` nested in `with [@unsafe]` removes explicit unsafe permission until it ends.
- Parse and retain a function effect-list slot before `@safe`, but reject nonempty lists with a clear unsupported-feature diagnostic in v0. Do not silently accept `std::fileops` or other unimplemented contracts. The syntax for explicitly unsafe function declarations while general effect lists are unavailable remains open.
- Migrate repository examples and fixtures and reject superseded spellings in the same branch. Preserve standard C syntax where applicable; the CX-specific syntax cutover must not break C parsing.

The future effect design uses closed empty defaults for omitted lists on safe functions and open defaults on nonsafe functions, and includes library-declared effects and intrinsic `@return`/`@yield`. These are design context, not checks to implement in this v0 syntax migration. Future `with` authority may be restricted to trusted code; v0 treats it as the replacement for the current explicit unsafe island.

## Reference lifetime contracts

| Source form | V0 meaning | Where the obligation lies |
| --- | --- | --- |
| `T '_&` | Ephemeral: valid within the current expression or parameter-call boundary, without escaping that boundary. | The producer establishes this short bound; the compiler checks attempts to extend it. |
| `T 'a&` | Relative: usable within a bound supplied by the signature and call. | The producer and caller establish the relationship; MIR analysis checks it. |
| `T&` | Free: no compiler-enforced lifetime restriction at use sites. | A safe producer must justify unrestricted use; a nonsafe producer may assert it unchecked. |

For `fn(T 'a&, T 'a&) -> T 'a&`, the returned reference may be used only during a lifetime supported by **both input referents and their access grants** in the caller. The return cannot outlive either input's relevant validity period. The parameter variables themselves need not remain alive in the caller. A named output bound must have an input or other declared source; the output type cannot invent one. Lifetime names describe relations and must not become concrete runtime-region IDs or template-specialization keys.

An ephemeral reference is the starting view for ordinary place access. A parameter annotated `'_` cannot be safely returned as though it had an input-relative or free lifetime; an explicitly permitted unchecked conversion can still assert a free contract. A binding conversion may shorten a longer established bound safely; it cannot lengthen the source's validity. V0 policy discussed for unchecked conversion to free is nonsafe permission from an ephemeral source and explicit unsafe permission from a named bounded source. Independently proving a producer's unrestricted contract is a separate, safe path. Converting an already-free reference to free or forwarding it adds no permission requirement.

`const` limits mutation through a reference; it does not freeze the referent against all other aliases. Lifetime validity, access authority, aliasing, and value stability are separate facts. In particular, a global object's storage lasts for the program, but that fact alone does **not** justify safely exposing a mutable free reference: arbitrary mutation may break the object's protocol. A library can expose one through a safe API only if its contract makes the permitted uses sound. V0 should conservatively support the straightforward const case and leave rich mutable access protocols to libraries, not invent compiler exclusivity rules.

A const reference to an entire tagged-union object can remain valid while that object's storage remains valid. A reference to the active payload is different: generated pattern/variant-access operations should expose a bounded reference through a pseudo-signature or eventual real generated signature. Replacing the active variant can end that payload's validity even when the containing storage remains. A raw union does not receive an automatic safe variant guarantee. Likewise, an element inside a reallocatable buffer is not made free merely because the buffer owner is global.

## Three safety tiers

```text
Safe < Nonsafe < Unsafe

required Safe      => allowed everywhere
required Nonsafe   => allowed in Nonsafe or Unsafe
required Unsafe    => allowed only in Unsafe
```

An unmarked function begins in the nonsafe tier; an `@safe` function begins in the safe tier. `with` changes the lexical permission for its body and restores the surrounding tier afterward. Unsafe permission includes nonsafe permission. The function's callable contract remains distinct from the permission at one operation: an `@safe` function may contain an explicit island, but its author remains responsible for the public contract. In v0 the island is not restricted to trusted packages, so it remains an explicit assertion boundary rather than a proof.

Ordinary operations currently rejected only because a function is safe, such as raw pointer dereference and raw indexing, become nonsafe requirements. Calls to functions explicitly marked unsafe require unsafe permission even in an unmarked function. `@nonsafe_move` makes movement require nonsafe permission; `@unsafe_move` makes it require unsafe permission. The intended future decomposition is `@unsafe_move = [__internal_unsafe_move, @nonsafe_move]`: stronger movement restrictions contain the weaker one. An aggregate marked only `@nonsafe_move` cannot mask an `@unsafe_move` field. Similarly, `@nodrop` contains the `@nocopy` restriction. Copying a `@nocopy` value or silently dropping a `@nodrop` value remains an error that no safety tier can authorize.

Invalidating storage with a live bounded dependent reference requires explicit unsafe permission at the invalidation operation. Permission acknowledges the risk; it does not erase the reference's origin or make subsequent use of a stale view automatically valid. A move's ownership kill comes from `Invalidate`, not from reading the moved value, preserving the atomized MIR model.

## Conceptual types and transformations

These are planning shapes, not a request to add parallel wrapper hierarchies if current IR types can express them cleanly.

```text
ReferenceContract = Free | Ephemeral(boundary) | Relative(lifetime_name)
PermissionTier    = Safe | Nonsafe | Unsafe
ReferenceFact     = (referent_region, access_grants, validity_bound, possible_origins)

HIR reference spelling
    -> THIR reference contract and signature relationships
    -> MIR place/view identity + explicit BindLifetime and Invalidate
    -> MIR CFG analysis of ownership and reference dependencies
    -> LMIR with lifetime-only metadata erased after validation
```

```text
typecheck_reference_conversion(source, target, permission):
    if target merely preserves or shortens source's established contract:
        accept
    else if target is Free and source is independently proven unrestricted:
        accept safe production
    else if target is Free and source is Ephemeral:
        require Nonsafe; record unchecked producer assertion
    else if target is Free and source is Relative:
        require Unsafe; record unchecked producer assertion
    else:
        reject an unsupported extension of validity

lower_bounded_view(source, sustaining_regions):
    emit the value/reference operation
    emit BindLifetime for each necessary storage or access dependency

analyze_invalidation(region, instruction_permission):
    if a live bounded view depends on region and permission is below Unsafe:
        diagnose at the invalidation source range
    apply ownership invalidation
    retain origin information needed to diagnose later stale uses
```

These rules should be implemented as general conversions and analysis facts rather than special cases in pointer, call, or return lowering. Distinguish a reference's own storage from the referent's region. Relative results inherit dependencies from the relevant caller arguments. CFG joins retain every possible dependency; loops need a fixed point. Scope ends, replacements, moves, leaks, tagged-payload switches, and reference-bearing aggregate copies must preserve or invalidate the corresponding facts. Until a case can be represented soundly, reject it in the safe subset rather than silently dropping its bound. Staged captures and materialization must preserve region identity and the effective permission of the originating operation.

## Suggested implementation slices

1. **Syntax migration.** Update lexer/parser, HIR formatting, grammar, library sources, examples, and fixtures for `expr<T>`, bracketed type attributes, `@safe`, and both forms of `with`. Add the reserved function-effect-list slot with a nonempty-list diagnostic. Keep unrelated staged-expression implementation changes out of this slice.
2. **Permission semantics.** Represent the three tiers in typechecking; make `with` restore the previous tier on every exit path. Distinguish a function's public contract from an operation's effective permission. Preserve that permission in MIR instruction metadata, including comptime-generated and staged instructions.
3. **Move restrictions.** Add `@nonsafe_move`, keep `@unsafe_move` stronger, check aggregate-field propagation, and classify existing unsafe operations. Do not build effect-row union/subtyping machinery just to implement these concrete checks.
4. **Lifetime-bearing types.** Preserve parsed annotations through THIR, signatures, conversion, comparison, formatting, and calls. Remove or diagnose unsupported `'static` spelling according to the remaining syntax decision. Do not confuse ordinary place access with producing a reference contract.
5. **MIR reference facts.** Lower bounded conversions to explicit binding operations. Extend MIR analysis with referent regions, access dependencies, invalidation checks, conservative joins, and caller-relative result instantiation. Ensure operand evaluation order and ownership transfer are already represented at the correct MIR point.
6. **Producer boundaries.** Check safe free-reference returns against known origins where the compiler can do so; reserve explicit permission islands or trusted library implementations for guarantees the compiler cannot derive. Initially reject unsupported safe mutable-free production, reference-containing aggregate paths, and staged paths rather than inferring unsound guarantees.
7. **Validation and maintained docs.** Add focused positive and negative cases for relative returns, local escape, free forwarding, bad producers, permission nesting, move restrictions, invalidation, tagged payloads, mutable-global exposure, and both backends. Update maintained language documentation only after the behavior is established.

## Current code landmarks and open decisions

At the time of this note, `compiler/cx-parsing/src/parse/types.rs` accepts lifetime spellings in `HIRTypeKind::MemoryReference`, but `compiler/cx-typechecker/src/symbol/completion.rs` discards them. THIR defines `THIRReferenceLifetime` without carrying it through completed reference types. `THIRCoercion::ReferenceBounding` remains a `todo!()` in `compiler/cx-thir-lowering/src/lowering/operators.rs`. MIR defines `BindLifetime`, while `compiler/cx-mir-analysis/src/passes/ownership.rs` currently tracks ownership availability rather than reference dependencies. Verify all these landmarks against the checkout before editing.

The following details were not settled and should be asked about before their implementation slice:

- The v0 spelling for a function whose call itself requires explicit unsafe permission, while nonempty general function-effect lists are diagnosed.
- Whether old `'static` source syntax is rejected outright or accepted briefly as an alias for free; no separate `'static` semantic category is intended.
- Exact rules for local reference declarations in `@safe` bodies, especially explicit local lifetime annotations. Forwarding an already-free reference is settled as safe in itself, but safe local mutable references still need a producer contract.
- The precise allowed set of safe mutable-free producers, and how a library declares or verifies richer aliasing and mutation protocols. Do not replace this with a compiler-wide borrow checker.
- The v0 treatment of parameterized staged-expression signature syntax and lifetime-bearing staged captures beyond conservative rejection.

This handoff grants no authorization to implement the plan. Recheck the user's latest instructions, preserve unrelated changes, and keep implementation slices narrow enough to review individually.

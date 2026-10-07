# Temporary agent handoff: staged expressions, effects, semantic lowering

AI-generated conversation state, rewritten 2026-09-19; supersedes the 2026-09-17 dump. DESIGN, NOT IMPLEMENTED SPECIFICATION. Repository-relative paths for portability. Retire after incorporation into maintained docs; reading this file does not authorize deletion or implementation.

## Intent / scope

User is manually redesigning MIR to remove accumulated agent-written technical debt. Future agent implementation should be narrow, explicitly requested sprints filling gaps in user's skeleton. This session authorized investigation/discussion and this rewrite, not compiler changes.

Near-term goal: Rust-style checking of generic bodies once under bounds, then MIR specialization. Preserve permissive/non-safe code compatible with base C. Unconstrained native operators in non-safe templates can still require per-instantiation semantic checking. Historical intent: site/docs/manual/templates.md; docs/pipeline_design.md. docs/ideas/permissions.md is OLD/DRAFT: motivation useful, details not endorsed wholesale.

## Guiding syntax (proposed)

~~~cx
expr<T> [E]
expr<T> [@return<R>, @yield<Y>]
void load_configuration(std::cstring_view path) [std::fileops] @safe { ... }

comptime expr<T> [P]
unwrap_or<P: permission, T: type>(
    expr<opt<T>> [P] operand,
    expr<unreachable> [P | @yield<T>] or_else
) @safe {
    return emit match (operand) {
        opt::some<T>(value) => yield move value;
        opt::none<T>() => or_else;
    };
}
~~~

- Type BEFORE bracket metadata; visibly retain expr. Permissions/effects before trailing annotations idiomatically; @safe AFTER prototype. Uniform intrinsic @ entries alongside library-defined entries.
- Dense metadata may use trailing where alias: result expr<T> [M], where M = E | @return<opt<U>>. M is a defined alias, not an independent deduction parameter. Exact placement relative to @safe/body TBD.
- Generic kind names/case (permission, Permissions, Effects, Type/type) remain provisional after conceptual shift to effects. Singleton/row merge spelling likewise illustrative.
- Merge may fail: concrete incompatibility => type error; symbolic merge => retained validity obligation. No explicit where compatible(...) needed if validity is implicit.
- Superseded spellings: expr<return = R, yield = Y> T; expr[E]<T>; E<T> hiding staging; prefix safe. Higher-kinded type constructors are not an implementation commitment.
- No overload system requested: identifiers resolve to a single definition, shadowing aside.

## Effects / contexts

- Effects bound what execution MAY do, not what must execute. Function annotation bounds invocation; expr annotation bounds latent materialized execution. Constructing/forwarding fragments does not exercise latent effects.
- Closed contract: actual composed fragment effects <= declared E; materialization/call: E accepted by destination/function context. Include effects inherited from inserted fragments, not only newly constructed operations.
- Return/yield acceptance includes valid destination, type compatibility, cleanup. Internal match yield produces ordinary result; only external yields escape. Handling a local yield must distinguish its destination, not erase all yields with the same value type. Binding details TBD.
- unwrap_or above does not need P as generator invocation effects. Generated match handles fallback's additional yield; remaining generated effects bounded by P.
- Shared generic E in compound(expr<T> [E] a, expr<T> [E] b) requires bound inference effects(a) <= E and effects(b) <= E, not exact equality. Prefer least compatible upper bound. Comptime selection of one input need not automatically narrow declared output.
- Join is checked combination, not map overwrite. Conflicting return/yield type requirements need compatibility rules, not invented union-valued destinations. Closed upper bounds are not disjoint cases: empty effects satisfy larger bounds too.
- No mandatory break/continue metadata requested; retain materialization placement checks. Completeness concerns tracked dimensions, not every possible behavior.
- Check retained operations conservatively; runtime reachability proofs unnecessary. Comptime construction may eliminate branches before validation. No undecidability claim forces permissiveness.

## Two tiers

- Safe: full declared bounds; no unexpected escaping return/yield. Omitted entries of closed safe contract forbid corresponding escapes. Explicit contract-template deduction wanted; inference of undeclared safe public contracts from bodies not selected.
- Non-safe: duck-typed composition/contextual instantiation may remain, with optional partial bounds. Unknown/open != empty/verified. Explicit [..] openness was assistant suggestion, not settled syntax. Defaults across modes/aliases need definition.
- Forwarding through a safe function cannot certify an unknown fragment: validate the resulting contract or use an explicitly trusted boundary. Generator execution and generated execution have distinct effects/safety obligations.
- Function scopes are targets of effect allowance. with/similar scope syntax explicitly DEFERRED; present needs are return/yield, library effects future motivation.

## Latest type-effect correction: DO NOT REINTRODUCE REJECTED MODEL

User rejected stored per-operation categories/tables (move_effects<T>, effects(T, move)) as the language model. Types carry effects; THIR -> MIR nodes interpret relevant intrinsic effects and propagate operation effects:

    lower move(value):
        if type(value) carries @unsafe_move:
            operation carries @unsafe_move
        else:
            operation carries no effect

- Merely using/holding a type marked @unsafe_move does not require unsafe authority; moving it propagates the effect. Two illustrative move signatures explained this rule, NOT actual overloads.
- User explicitly wants broad @unsafe with implication @unsafe -> @unsafe_move, NOT reverse. Narrow effect may be granted independently. Context allowance may use implication closure.
- Copy/discard on nocopy/nodrop types should carry distinguished UNPERMITTABLE effect (identifier TBD). No context, including unsafe, can permit it. Can implement as failed static obligation/compile-error/static_assert(false); need not emit runtime assertion. Merely carrying such a type is not an error.
- Cover implicit operations: copies from coercions/arguments, discards from scope exit/replacement/owned fields. nodrop forbids silently discarding; move transfers obligation; explicit consumption/discharge distinct. Effects do not replace ownership/control-flow analysis.
- Unsafe/nonsafe contexts can be effect allowances; delegating unsafe authority into a checked safe function is desired. Allowance accepts responsibility for operation conditions, not proof of runtime invariants. Distinct nonsafe_move policy mentioned, not finalized.
- Preserve base-C defaults: normally declared ordinary types copyable, droppable, movable, allocatable/sized. Restrictive declarations alter defaults. Permission cannot conjure missing runtime layout.
- Composite effects/permissions wanted. Considered @droppable = {@__internal_droppable, @copyable}; !@droppable removes both. Actual discard checks primitive atom, otherwise removing copy alone also forbids discard. Negative revocation vs positive nocopy/nodrop spelling NOT final. Do not object that composite removal cannot express nodrop -> nocopy.
- Earlier example foo<T: Type, P: Permissions>(T [P] value) must not allow foo<UndroppableType, {@droppable}> to manufacture intrinsic droppability. Exact type-use well-formedness after effects reformulation TBD. Assistant's universal "supports(T,P) evidence" interpretation is NOT final effect semantics: expression effects are upper bounds, permitted by destination.
- Prototype type-effect bounds should mean "only effects within E", not open contains-E matching. No need explicit not-unsafe_move on ordinary types with closed defaults. Unconstrained generic T does not itself prove absence; generic defaults/bounds need explicit rules.
- LATEST AGREED FUTURE: generic bounds can express "function carries generic effects E present on T"; need bounding/filtering E to specifically relevant effects such as unsafe_move. Implement alongside future trait-like type bounds for single generic-checking path. No conditional-propagation syntax chosen; acceptable to defer. For now concrete permissive instantiation resolves effects; checked generic needs sufficient bounds or conservative declared effects.
- EXPLICIT TBD: reducing a type's exposed effects. Do not settle revocation/erasure/wrapper safety now.

## Architecture: consistency formulation -> authoritative semantic lowering

User proposal supersedes assistant's initial insistence on fully elaborated semantic THIR:

    HIR -> THIR:
      resolve bindings to IDs, establish symbolic type identity/relationships;
      formulate known C arithmetic/promotions/coercions;
      retain unresolved dependent operations/context requirements.
    THIR -> MIR:
      authoritative legality/effect/ownership/context checking;
      comptime MIR evaluation, type resolution, specialization/materialization;
      executable MIR only after relevant obligations established.

- Avoid duplicate implementations of "can yield here?" / unsafe movement etc. THIR node is a request, not approval. Preserve intrinsic numeric type awareness for C semantics; not all semantics reducible to effects.
- Hypothetical: using A = @instantiate(type_gen(args...)); A x = factory<A>(...); x = assume_integer_add<A>(x, 2); use(move x);
- THIR may introduce opaque alpha linked to generating computation. Shared identity supports consistent use without representation/permission knowledge. Permissions are in generated Type VALUE, not necessarily in static Type type. instantiate is a type-forming intrinsic, not ordinary static function prototype.
- Equal identities establish consistency, not support for comparisons/arithmetic/copy. Limited early expressiveness acceptable; helpers can expose alpha-preserving signatures and reject unsupported concrete types later. Unknown result must not silently equal alpha.
- Lowering resolves dependencies, validates/executes comptime computation, obtains descriptor, then checks moves/storage/etc. Alpha resolution per specialization, not mutable shared THIR. Distinct pending identities need not imply distinct final nominal types.
- Runtime representation/stage checks remain necessary; instantiate does not magically make every comptime-only type runtime-compatible. Dependency scheduling/cycle diagnostics/unresolved obligations explicit. Comptime-executed MIR also validated; quotations may retain placement requirements.
- Preserve fixed operation meanings/bindings; unresolved native operations remain explicit. No silent reinterpretation of already-fixed arithmetic.
- Comptime-only structs with staged fields desired future feature. Constructing/storing fields does not execute fragments. Parent latent summaries/field contracts remain open; don't assume construction exercises all child effects.

## Generic checking agreement

- Body checking, concrete-bound checking, and code specialization are distinct. Checking once allows per-type/per-effect generated code.
- Safe generic: need not support native operators on unconstrained generic arguments. Future library/type-class interfaces supply operation meaning/signature/effects. Check body under declared assumptions; instantiation substitutes types/interfaces without discovering new source-operation meaning.
- Non-safe template: a+b on unconstrained generic operands needs concrete numeric promotions/pointer arithmetic/etc.; per-instantiation semantic checking is natural main path. Share resolved symbolic THIR/independent work, not necessarily reparsing or repeated lexical resolution.
- Complete contract = enough assumptions to SELECT AND VALIDATE dependent operations, not merely written return types/effect rows. Non-safe functions with complete interfaces could also check once; mechanism not selected. Inferred contracts/deferred regions were alternatives discussed, not commitments.
- Bare Type-returning generator does not universally guarantee safe movement. Checked generic needs bounds/output guarantee/evidence/refinement; otherwise concrete-instantiation checking.
- No unimplemented trait/filter syntax should be presented as settled.

## Earlier staging invariants still applicable

- Preserve opt::try: input opt<T> supplies T; eligible runtime return destination opt<U> supplies U independently. Extract int inside function returning opt<string>. Proposed output expr<T> [@return<opt<U>>]. Bare unconstrained return R insufficient to establish opt::none can produce R in a checked generic.
- Comptime call arguments evaluated in COMPTIME context: lexical nesting in runtime function does not supply immediate runtime yield deduction. Outer call result materialized in runtime context may deduce from destination. Future comptime-variable RHS likewise not immediate runtime materialization. Unknown destination needs other constraints.
- Instantiation and materialization contexts may differ. Permissive mapping: (instantiation context, prototype template) -> (prototype, generation_request). Generation-relevant context belongs in specialization identity; final placement checked separately without retyping fixed interiors.
- Immutable staged DAG retaining THIR + bindings to fragments/evaluated constants/runtime places; materialize by recursive lowering into destination. Replaces premature staged MIR emission/remapping. Existing Arc sharing alone is not this redesign.
- Stable binding/capture IDs, fresh insertion locals, hygienic bindings. Sharing must not memoize runtime evaluation. Distinguish fragment/value/place captures; do not replay source for evaluated values. Preserve place identity/lifetimes; avoid expired comptime frames.
- Fragment lexical bindings separate from destination control/cleanup context. Replacing whole scope stack can hide caller exits/defers. External exits clean up both fragment and caller scopes.
- Allowed exit adaptation at boundaries (e.g. i8 -> i16 yield), not interior retyping; ordinary result conversion separate. Preserve original/composition/materialization source provenance.

## Breadcrumbs / audit snapshots (verify new checkout)

- lib/std/optional.cx: opt::try/try_or/unwrap_or_else, currently old syntax.
- site/docs/manual/{templates.md,move-semantics.md,linear-resources.md,safe-functions.md}: existing intent/contracts, not implementation of this proposal.
- compiler/cx-typechecker/src/type_checking/value/moves.rs: observed unsafe_move checked during THIR construction. .../control_flow/yield.rs and compiler/cx-thir-lowering/src/lowering.rs: observed yield-context checks in both stages.
- compiler/cx-typechecker/src/type_checking/op/binop/calls.rs; .../environment.rs; .../type_checking/staged_expr.rs: context/call completion/staged arguments.
- Original 2026-09-17 audit, not all reverified: complete_comptime_call uses mangle_comptime_context (external return/yield types) in symbol identity; existing path already guards cross-caller reuse, can specialize unused context. No reproduced reuse soundness failure. lower_comptime_function takes THIRComptimeFn.context; parent builders also serve captures/scratch evaluation.
- compiler/cx-thir/src/thir/{type.rs,data.rs,comptime.rs,name_mangling.rs}; compiler/cx-thir/src/intrinsic_types.rs: representations/context/primitive mappings.
- compiler/cx-thir-lowering/src/{builder.rs,builder/function.rs,lowering.rs,lowering/calls.rs,lowering/comptime.rs,lowering/control_flow.rs,lowering/staged.rs} and lowering/staged/: capture/evaluation/materialization/exit paths.
- compiler/cx-mir-comptime/src/value.rs: shared staged storage.
- No compiler implementation, runtime testing, or soundness proof completed in this discussion. Open details also include target binding, coercions, checked generic representation, staged storage ownership; preserve explicit TBDs above.

# Temporary agent handoff: HIR -> HMIR -> MIR staged pipeline

AI-generated conversation state, written 2026-09-30 on branch feat/2ltt. DESIGN, NOT IMPLEMENTED SPECIFICATION. Paths are repository-relative. Retire this file once its content is incorporated into maintained docs. Reading it does not authorize deletion or implementation.

Companion to docs/TEMP_AI_GENERATED_c_compatible_type_parsing.md, which covers parsing. This doc covers everything after parsing.

## Intent / scope

The pipeline replaces language-facing template instantiation with comptime semantics, and defers instantiation to staging.
- Templates become ordinary comptime parameters, Zig-style. Type constructors are functions returning `@type`.
- Instantiation is not a separate frontend mechanism. It is what staging does when it evaluates a static application.
- C operators desugar to lib/std/intrinsic/native_ops.cx, as described in the parsing doc.

Background audits: ~/workspace/Papers/2209.09729-2ltt/cx-staging-pipeline-audit.md and cx-parallels-audit.md.

## User decisions

- **THIR and HMIR merge.** Elaboration outputs HMIR directly, and there is no separate checked tree. Elaboration and staging stay separate passes.
- **Coercion is explicit.** A stuck conversion is not a deferred constraint. Elaboration emits a call that staging runs:

  ~~~cx
  comptime T add(@type T, T a, T b) => a + b;
  // elaborates as
  comptime T add(@type T, T a, T b) => op::add(a, b) |> op::coerce(T);
  ~~~

- **Comptime evaluation may fail during staging.** Following Zig, generic code that is never instantiated is only partly checked. The accepted cost is a deviation from the theory. The requirement is clean error traces, never C++-style expansion dumps.
- **The rewrite runs as a parallel pipeline.** HIR -> HMIR -> MIR sits next to HIR -> THIR -> MIR. The new path produces no comptime functions in MIR. Once THIR is removed, the comptime parts of MIR are removed with it.

## Pipeline

~~~text
HIR    parsed; operators are neutral nodes; names unresolved
  |  elaborate: demand-driven queries, bidirectional checking,
  |  metavariables + unification, NbE for closed comptime terms
HMIR   structured tree in arenas; IDs; stage-annotated;
       type terms on binders; explicit op::coerce / staged ops;
       each generic definition stored once
  |  stage: demand-driven from roots; evaluate U1, residualize U0;
  |  memoized type constructors; specialization cache
MIR    concrete, flat, signless; no static work remaining
  |  cx-mir-analysis -> LMIR -> backend   (unchanged)
~~~

Why merge THIR into HMIR:
- The checker needs the evaluator. Checking `opt(int) x; x.value` requires evaluating `opt(int)`, so elaboration must already produce executable terms.
- Name resolution to IDs happens during elaboration anyway (binders, shadowing, the scoped binding table from the parsing doc). A separate flattening step would do nothing else.

Why keep elaboration and staging separate:
- The LSP can stop after elaboration and still get types, errors and go-to-definition.
- Every diagnostic has one owner: the definition site or the instantiation site.

## HMIR shape

- **Structured, not flat three-address code.** Runtime regions keep their scope, cleanup, `defer` and exit structure until MIR lowering. The static side runs as a tree-walking evaluator over the same terms.
- **Terms are typed, and types are terms.** Binders, temporaries and operations whose MIR form needs a type carry a type term. That term may be neutral, e.g. `add_result(T, T)`. Other nodes are not annotated.
- **Types are typed so that three things stay possible:**
  - residualization can produce concrete MIR types;
  - the "checked" claim for closed code stays honest;
  - a core linter can re-check HMIR (like GHC's `-dcore-lint`). This is cheap once terms are typed.
- **Static and runtime parameters are separated in calls:** `Call(def, static_args, runtime_args)`. After elaboration, static arguments are always explicit. They may be neutral, but they are never holes.
- **`expr` parameters are quoted code:** `Quote`/`Splice` nodes. Place parameters carry a place, not a value.
- **The static value model** has four kinds of value: interned type descriptors, ordinary static values, static closures, and code values (with captures and exit requirements). A residual reference to a runtime local is a different thing from that local's contents.

## Elaboration

- **It is demand-driven, per declaration.** There are separate queries for `signature(def)`, `body(def)` and `eval(closed_term)`.
  - Runtime recursion only needs the signature.
  - A static cycle, such as a type that needs its own value, is an error.
  - cx-typechecker/src/requests.rs is the starting point, split into a signature phase and a body phase.
- **`_` is a metavariable,** solved by unifying type terms. Every metavariable must be solved by the end of its enclosing declaration. Staging never deduces anything.
- **PROPOSED deduction order:**
  1. Unify argument types first.
  2. Use the expected result type only for metavariables that are still unsolved.

  This matches C++ and keeps results predictable:
  - `long y = sum(_, x, 10)` with `int x` gives `T = int`, then the result is coerced to `long`.
  - `opt(int) n = opt::none(_)` still works, because no argument constrains `T`.
  - Letting the expected type win first would silently change arithmetic width, which is why arguments go first.
- **Unification can decompose memoized type constructors.** Their descriptors record (constructor, args), so `opt(?0) ≟ opt(int)` solves `?0 := int`. A general type-level computation is not injective: `add_result(?0, ?0) ≟ int` cannot solve `?0` and reports "cannot deduce `_`".
- **NbE: closed terms evaluate, open terms may get stuck.**
  - A closed term (no free comptime parameters) is evaluated during elaboration: `opt(int)`, `add_result(int, long)`, `sizeof(int)`.
  - An open term evaluates as far as it can. `pair(T)` returning `struct { T first; T second; }` fully evaluates with a neutral `T`, so `p.first` resolves early.
  - A constructor that branches on `is_int(T)` gets stuck.

## Explicit coercion

Every checking site inserts a coercion: initialization, assignment, argument, return, initializer-list field and condition. At each site:
1. **Unify.** If it succeeds, emit no node.
2. **Unification is stuck (open terms):** emit `op::coerce(From, To, x)`, and staging runs it.
3. **Unification fails on closed types:** run `op::coerce` during elaboration. It either converts legally (`int` -> `long`, giving an `int_cast`) or reports the error at the definition site.

As a result, closed code (including all C) gets early errors and never pays for identity coercions at staging.

- **Pass the source type.** `From` is what the expression synthesized. Without it, `coerce` cannot dispatch when both types are stuck.
- **Handle value category in elaboration, before `coerce`.** Reads, copies, moves and temporaries happen there, so `coerce` is value -> value. Binding a reference is not a coercion.
- **Four operations with separate rule tables:**
  - `op::coerce`: C's implicit assignment conversions.
  - `op::convert`: `as`, the stricter table.
  - `op::c_cast`: C-style `(T)x`, the loosest table, kept for compatibility.
  - `op::truthy`: conditions. In C a condition compares a scalar with 0; it does not convert to `bool`.
- **Other stuck lookups get the same treatment.** Member access, indexing and calls through a stuck callee type become explicit staged HMIR operations, resolved at staging and reported through the same error path.

## Staging

- **Roots:**
  - non-generic functions
  - `main`
  - exported and `extern`-visible symbols
  - functions whose address is taken

  Staging generates the instances they transitively request.
- **The specialization cache** is keyed by (definition, interned static arguments, context parameters). Reusing the result of an effectful generator needs a stronger policy than this key.
- **Type-returning comptime functions are pure and memoized.** `opt(int)` is the same type everywhere. A struct type produced by such a function gets its identity from (defining site, normalized arguments), and descriptors are interned so type equality is ID equality.
- **Most of native_ops are `@inline` mixed-stage functions, not `expr` macros.**
  - `add(@type A, @type B, A x, B y)` gets one instance per `(A, B)`, and inlining is required.
  - This avoids duplicating side effects through `expr` parameters.
  - Only `&&`, `||`, `?:`, assignment, compound assignment, `++`/`--` and `&x` need `expr` or place parameters.
- **Cross-module instances are emitted by the requesting module.**
  - For testing, duplicates with internal linkage are enough.
  - Later, `linkonce_odr`/COMDAT with mangling derived from the interned arguments.
- **Staging may fail only for these reasons:**
  1. comptime evaluation failures (asserts, `@compile_error`, running out of fuel, intrinsic side conditions)
  2. staged `op::coerce` or stuck lookups that fail once they can be evaluated
  3. layout or representation errors for types that were stuck until now
  4. specialization limits (depth, instance count)

  Flow and resource analysis stays on MIR (cx-mir-analysis), as it is today.

## Error traces

1. **Hide prelude frames,** similar to Rust's `#[track_caller]`. A failing `op::coerce` or `op::add` points at the user's `a + b` or `return`, never into native_ops.cx.
2. **A frame is a call site plus its static arguments,** e.g. `in sum(T = struct Point), called from main.cx:12`. Never show expanded code.
3. **Report each failing instance once,** with the first chain of calls that required it. Cap the depth and elide the middle frames.
4. **Add a `@compile_error(fmt, args...)` builtin.** That requires a type printer usable at comptime. Running out of fuel reports through the same path.

## Parallel pipeline plan

The current code already accommodates a second path:
- compiler/cx-pipeline/src/scheduler.rs:520 does `generate_mir(thir)?.into_static_runtime_only()`.
- compiler/cx-pipeline-data/src/db.rs:31 stores `MIRUnit<'static>`.
- `into_static_runtime_only` (compiler/cx-mir/src/unit.rs:59) drops `comptime_functions` and `staged_expr_pool`.

Analysis, LMIR and both backends only ever see runtime-only MIR. If the new path produces `MIRUnit<'static>` directly, nothing downstream changes. After THIR is deleted, the `'thir` lifetime on `MIRUnit` and the comptime parts of MIR go too:
- cx-mir/src/expr/comptime.rs
- cx-mir/src/staged.rs
- the `MIRComptime*` types in cx-mir/src/value.rs

Crates:
- `cx-hmir`: data only.
- `cx-hmir-elaborate`: HIR -> HMIR. The LSP will eventually depend only on this crate.
- `cx-hmir-stage`: HMIR -> MIR.

Items to handle:
1. **Template bridge.** lib/std uses `<T>` templates: optional, vector, box, span, cell/rcell, functional. The elaborator should treat HIR templates as comptime parameters:
   - `T f<T>(T x)` becomes `f(@type T, T x)`.
   - `f<int>(x)` becomes `f(int, x)`.
   - `struct pair<T>` becomes `comptime @type pair(@type T)`.

   This gives the new path the whole test suite on day one, and allows differential testing: run tests/integration through both pipelines and compare program output. Removing `<T>` later becomes a parser-only change.
2. **Shared intrinsic semantics.** Move cx-mir-comptime/src/execution/{arithmetic,scalar}.rs and intrinsics/ into a crate both evaluators use, so the two evaluators cannot drift apart during the overlap.
3. **Module database.** Add `module_db.hmir` and new scheduler steps `Elaborate` and `Stage`. Staging a module depends on its dependencies' HMIR, not their MIR, because instances are built in the requesting module.
4. **Prelude auto-import.** `INTRINSIC_IMPORTS` (compiler/cx-thir/src/intrinsic_types.rs:14) is already a prelude auto-import. Move it somewhere both pipelines can reach, and add native_ops.cx to it for the new path only.
5. **New syntax on the old path.** The old typechecker should reject new HIR forms (`@type`, `_`, comptime parameters) with a clean "requires the HMIR pipeline" error, not a panic.
6. **LSP.** `typecheck_only_lsp` (compiler/cx-pipeline/src/lib.rs:411) stays on THIR until the switch.
7. **Pipeline selection.** A config flag selects the pipeline, and the integration test runner is parameterized over both.

## Migration order (each step leaves the compiler working)

1. Elaborate non-generic code into HMIR, with a staging step that translates directly to MIR. This is equivalent to cx-thir-lowering and proves the IR.
2. Add type values, memoized type constructors and NbE, starting with type-returning comptime functions (`opt(T)`).
3. Add mixed-stage functions, the specialization cache and the template bridge. Differential-test against THIR.
4. Add the native_ops desugaring behind a flag. Make arithmetic `@inline` functions, and do integers first so the usual arithmetic conversions are validated early.
5. Delete THIR, cx-typechecker template machinery (`THIRTemplateInput`, symbol/template.rs), the comptime parts of MIR, and the parser's `temporary_type_names` path.

## Open questions

- Deduction order (proposed above): arguments first, then expected type.
- Policy for static side effects under runtime branches. A tractable first rule: they must be pure or isolated.
- Hygiene of `expr` parameters that are used more than once: let-insertion vs documented macro semantics.
- Whether checked bounds (`@type T : Int`) come later to give definition-site guarantees for generic arithmetic.
- The mangling scheme for interned static arguments. It must be stable across modules.

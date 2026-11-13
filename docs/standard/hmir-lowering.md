# HMIR lowering

HMIR unit to MIR unit. Evaluation interprets comptime code, elaborates types, and emits runtime code in one pass over HMIR; definitions are generated on demand.

## Terms

*verified uncommitted*

- **context**: runtime (emits MIR into a function) or comptime (interprets). Runtime emission is *enabled*, *unevaluated* (dry run: types only, nothing kept), or *suppressed* (after a `yield`, until its target).
- **evaluation**: ready value, or *incomplete*: suspended until the type expected of it is known; may carry a partial value, which may stand as fallback.
- **complete expecting T**: resumes an incomplete evaluation with T; a ready one is unchanged; no T leaves it incomplete.
- **fail if incomplete**: incomplete evaluation becomes T0052.
- **then**: continuation runs on the ready value; an incomplete evaluation stays incomplete and carries the continuation, so expectations reach it later.
- **stop**: runtime *divergence*, or a comptime `return`, `yield`, `break`, `continue`, carried outward until caught.
- **freeze e**: evaluate e in a comptime child seeing only *known* values of visible locals: constants, runtime slots with a known value, live comptime storage. Constants holding a local address are not known. A comptime stop escaping it: M0039. Result exported.
- **export**: result must become a constant holding no local address, else T0017; a reference to comptime storage: T0017; a place is copied out.
- **requires impure**: unless unevaluated, expectations must allow impure operations, else T0070. Contract conditions are pure.
- **requires nonsafe**: unless unevaluated, permission at least nonsafe, else T0028.
- **place**: a value of type `T&`, designating storage.
- **collapse**: `T& ... &` read until a single `T&`.
- **value type** of T: referent of T, unqualified.
- **integer promotion**: int narrower than 32 bits, `bool` included, to `i32`.
- **comptime-only type**: `type`, staged `^T`, dependent `_`, or a function that is comptime-stage, has a comptime parameter, or a comptime-only parameter or return type.
- **declaration point**: position of a declaration in its unit. A context *at* a point sees declarations of its own unit up to it, and all of other units.
- **def**: one symbol; holds every redeclaration of it, each a *declaration* (role declaration, tentative, or definition) with optional content and tag.

## Unit

*verified 425c5ff1*

HMIR unit and imported HMIR to one MIR unit. Every def of the unit validated, in id order; then the generation queue drained in request order. Yields the MIR module.

Settings: explicit return required unless the module file is `.c` (configurable); assertions on unless disabled by flag.

### Validation

*verified 425c5ff1*

Per def: declaration selected (see [Selection](#selection)), linkage reconciled.
- `rule/safe-variadic-definition` defined variadic function with safe contract: T0017.
- `rule/generation-roots` requested for generation when not static and: runtime-stage, defined function not [needing specialization](#function-lowering); or global with initializer or not extern.
- `rule/eager-comptime-globals` untagged comptime global definitions bound eagerly, so their errors surface unused. Def with several declarations whose selected is a global: bound eagerly.

### Generation

*verified 425c5ff1*

Queue of definitions, specializations, constructors; first in, first out. A request made while lowering one function is generated after it.
- `inv/generation-once` each def requested once; each specialization once per def and comptime arguments; each constructor once per type and variant.
- def: selected declaration's content. Function: lowered only if runtime-stage, defined, not needing specialization. Global: [Global definition](#global-definition). Comptime global: nothing.
- specialization: [Function lowering](#function-lowering) with its signature and bindings.
- constructor: [Variant constructor](#variant-constructor).

## Symbols

*verified 425c5ff1*

### Lookup

*verified 425c5ff1*

`a::b::n` from a source unit. Searched: source unit, then its imports. Yields best-ranked defs, sorted, deduplicated.

Candidate names in order: as written; unqualified also in source namespace; for each namespace prefix with aliases, every alias target joined with the rest. Per candidate name: ordinary class, then tag class; first class with any match taken. Tagged lookup (`struct S`) searches tags only.

- own unit always visible; other units need a public declaration, or package visibility with the name's namespace under the source's.
- rank: own unit in the source namespace, then own unit elsewhere, then imports; then having a declaration with the looked-up tag (none, for untagged lookups). Only the best rank kept.

See [Name](#name) for filtering and ambiguity.

### Selection

*verified 425c5ff1*

Choosing the governing declaration of a def among those visible at a point. Yields its index.
- `rule/no-visible-declaration` none visible: T0001.
- `rule/mixed-tags` declarations with differing tags: T0067. Differing kinds (function, global, comptime global): T0006.
- `rule/duplicate-definition` two definitions: T0007, unless both untagged type definitions.
- selected: has content, highest role (definition, tentative, declaration), earliest among equals.
- `rule/redeclaration-agrees` functions must agree with selected in stage, variadic, param count, comptime params, link name; globals in link name; comptime globals in being a type: else T0006.

### Linkage

*verified 425c5ff1*

Reconciled over visible function and global declarations. None: standard.
- first `static`: static. Otherwise the last non-`extern` linkage, else `extern`.
- `rule/static-after-external` `static` after a non-static first: T0006.
- `rule/global-standard-after-static` global without specifier after a `static` first: T0006; functions inherit.

### Binding

*verified uncommitted*

Value of a def at a point, memoized (see [Memo](#memo)).

- tagged declaration (`struct S`): forward type of that tag created if unrealized; yields type value.
- function: signature of every visible declaration evaluated; all alike, else T0006. Signature of the selected declaration kept. Yields function constant.
- global: declared type of every visible declaration evaluated; incomplete one completed from initializer value. Combined by [composite](#sameness), else T0006. Yields `T&` address of the global.
  - `rule/tentative-array` unit end, `T[]`, not extern, no initializer: `T[1]`.
  - `rule/incomplete-global` incomplete object type, not extern: T0009. Exempt: while initializing, tentative `T[]`.
- comptime global: declared type evaluated. `type`: alias published before its target is evaluated, so the target may refer to it; yields target type. Otherwise initializer value expecting declared type.
  - `rule/comptime-redeclaration` other visible declarations must evaluate to alike types, else T0006; a non-type value with others: T0006.

Initializer value: memoized in a comptime child at the body's point, then exported.

## Functions

*verified 425c5ff1*

### Function lowering

*verified uncommitted*

Runtime function, or specialization with bound comptime parameters. Yields MIR function definition.

*Needs specialization*: runtime-stage, and a comptime param, or a param type written as `^T` or comptime-stage function type.
- `rule/quirk-specialization-by-syntax` decided by the param's type expression form; aliases to such types not seen.

Runtime context at the body's point, return type from signature, permission per contract safety. Address-taken labels registered. Root scope targets returns.
- params: specialization-bound value, else fresh named place, initialized, bound as `T&`.
- [Contracts](#contracts) entered, then body evaluated, then [fallthrough](#fallthrough). Divergence caught.

Prototype: link name, reconciled linkage, lowered signature.
- `rule/comptime-prototype` comptime-only function type: runtime-stage T0042, else M0031.

### Variant constructor

*verified 425c5ff1*

Variant `S::V` used as runtime function: static `V.__cx_variant.<n>`; payload param moved into the variant, returned.

## Globals

*verified 425c5ff1*

### Global declaration

*verified 425c5ff1*

Link name, reconciled linkage, lowered type; mutable unless const. Name already declared in module: same type and mutability, else M0004.

### Global definition

*verified 425c5ff1*

- extern without initializer: declaration only.
- `rule/global-redefined` already defined in module: M0007.
- initializer: [initial value](#initial-value), lowered constant.
- otherwise zero-initialized.

#### Initial value

Comptime child at the body's point. Initializer value expecting declared type; declared type completed by it; converted implicitly. Without initializer: zero of declared type, `T[]` as `T[1]`.
- `rule/extern-initial-value` extern without initializer: M0029.
- incomplete type, or no zero value: T0009.
- zero: ints 0, floats 0.0, pointers null, `void` unit, aggregates zero per member, unions and tagged unions first member.

## Types

*verified 425c5ff1*

### Memo

*verified 425c5ff1*

Defs and expressions evaluated once per key: def or expression, in a context of owner unit, point, visible known comptime bindings (canonicalized), expected type, expectations, initializer flag.

Evaluated in a comptime child at the context's point, bindings rebound. Def: only the expectations' permission kept.
- `inv/type-identity` type id is the memo entry of its type expression; same expression in a different context, a different type. Generic instances arise this way.
- `rule/dependency-cycle` entry re-entered while evaluating, nothing published: M0028, naming the path.
- incomplete or failed: entry reset, re-evaluated on next use.
- expression: type node constructed. Otherwise evaluated, completed expecting expected, converted implicitly to it; with no expected, initializer non-types rvalue-decayed.

### Forcing

*verified 425c5ff1*

Type id realized: aliases followed; pending forward-declared tag evaluated once visible.
- `rule/expected-type-value` evaluates to non-type: T0053.
- a type defined as itself: M0028.

Preparing (before layout or lowering): forced through members, each validated ([Type expression](#type-expression) `rule/field-traits`, `rule/unreachable-pointee`). Forward-declared tag stops, left unprepared.

Visible: not declared after the current point in the same unit. Complete: not undefined, not `unreachable`; arrays need complete element.

### Sameness

*verified uncommitted*

- *same*: equal move semantics; underlying types structurally equal, qualifiers ignored at any depth. Functions: variadic, params, return; stage, safety and decaying ignored.
- *alike*: equal semantics, qualifiers included at depth. Functions: stage, safety, variadic, comptime params, params and return alike ignoring top qualifiers. `rule/quirk-decaying-not-alike` a `fn` and a C function type of one signature are same but not alike, as decaying is part of the id.
- *composite* of two: alike, the first; `T[]` and `T[n]` with alike elements, the bounded; else none.
- `inv/canonical-types` settled types fingerprinted; alike types share one id. Named aggregates, tags, builtins, staged by underlying identity plus added semantics. Constants canonicalized with them; ints wrapped to width.

### Type lowering

*verified 425c5ff1*

HMIR type to MIR type, cached per underlying type.
- `rule/comptime-only-lowered` comptime-only type: T0065.
- const and volatile become metadata. Qualified arrays qualify elements instead.
- struct, union, tagged union: distinct per type, never shape-interned; undefined while fields lower, so self-reference works. Bitfields keep width. Forward-declared tag: undefined of that tag.
- `void`, `unreachable`: void. Reference: memory reference. `T[]`: undefined array.
- comptime-only types laid out pointer-sized in comptime memory.

## Expression evaluation

*verified 425c5ff1*

Each node evaluated by kind. Comptime context seeking a selected `case` routes every node to the seek of [Switch](#switch).

- `inv/suppressed-diverges` after any node, runtime context with suppressed emission: diverges.
- `todo/error-node` error node.

### Literal

*verified 425c5ff1*

Int and float literals: constant of their suffix type. String literal: `str` constant. Unit: `void` constant.

### Local

*verified uncommitted*

Binding of the local, collapsed.
- `rule/runtime-staged-local-splices` runtime context, staged (`^T`) value: [spliced](#splice) with no arguments.

- `rule/comptime-unavailable-local` no binding, comptime context: M0029.
- `todo/runtime-unbound-local` no binding, runtime context.

### Let

*verified uncommitted*

`T x;`, `T x = e;`, `_ x = e;`, `auto& x = e;`. Yields unit. Each evaluation binds a fresh instance.

Declared type evaluated; for `auto&` the hole inside is evaluated instead, as referent. `@adopt` initializer goes to [Adoption](#adopt).

Declared type known, complete, visible: storage allocated up front, and at runtime default-initialized unless nocopy.

No initializer: declared type completed without expectation; storage declared; default-initialized unless nocopy or already initialized.

With initializer: e evaluated, completed expecting declared type, then:
- converted implicitly to declared type, when known.
- `_` or `auto&`: collapsed, kept as-is. Inferred type: `_` the value's own type, a reference when e is a place; `auto&` its referent.
- otherwise inferred type is the value type.
- declared type completed expecting inferred, then `auto&` wraps it as reference, value converted implicitly to it.
- storage declared, then written if default-initialized earlier, else initialized.

Rules:
- `rule/runtime-staged-local` runtime, declared type is code: initializer required (T0017); bound as [staged argument](#staged-argument), no storage.
- `rule/comptime-function-local` runtime, comptime-only function type: value bound directly, no storage.
- `rule/local-object-type` function type: T0011, noting to take its address; except a comptime-only function type in comptime storage, held there as a value. Incomplete or invisible type: T0009.
- `rule/quirk-let-double-initialize` runtime local of complete, copyable declared type default-initialized before its initializer, then overwritten.
- `todo/comptime-let-at-runtime` comptime-marked local in runtime context.

### Block

*verified 425c5ff1*

Statements in order.

Unscoped block: last statement's result yielded unchanged, places kept.

Scoped block: own scope. Non-yielding: yields unit. Yielding (`.{ }`): last statement's value rvalue-decayed; plus every `yield` targeting it. At runtime: no yields, the last value; otherwise yields and fallthrough merged by [merge](#merge). At comptime: a yield stop is caught as the value.

- `inv/dead-code-checked` statement after a divergence still evaluated, in an unreachable block that only labels or cases inside may enter.
- `rule/diverged-block` all paths diverged and current block unreachable: block diverges.
- `rule/nodrop-discarded` non-final statement yields an owned nodrop value: T0071.
- `rule/yield-suspends-block` non-final statement incomplete for a yielded type: remaining statements resume once completed. Any other incomplete non-final statement: fail if incomplete.
- `rule/mixed-yields` some paths yield void, others values: T0036.
- `rule/quirk-unscoped-no-discard-check` unscoped block's final statement skips the nodrop check and decay.

### If

*verified 425c5ff1*

Own scope. Condition is a truthy coercion, inserted by HIR.

Comptime: condition evaluated, fail if incomplete, must be an int constant. Selected branch evaluated in its own scope; with `else` yields its value, without yields unit.

Runtime: branch on condition. Then-branch in own block and scope, kept as place when an `else` exists. Then else-branch, likewise, completed expecting what the then-branch was expected as. Arms merged by [merge](#merge); stays a place only if neither arm declared places. Without `else`: yields unit.

- `todo/comptime-condition-shape` comptime condition not an int constant: panic, not a diagnostic.

### While and For

*verified 425c5ff1*

`while`, `do while` (condition after first body), `for (init; cond; inc)`. Yields unit.

`for`: own scope; init evaluated, fail if incomplete; then as `while` with increment.

Comptime: loop until condition false; body in own scope; `break` ends, `continue` proceeds to increment.

Runtime: condition and body share a scope. `break` targets exit, `continue` targets increment, else condition.

- `rule/diverging-condition` runtime condition diverges: loop diverges, body and exit unreachable.
- `rule/quirk-unbounded-comptime-loop` no comptime iteration limit; M0036 never raised.

### Switch

*verified 425c5ff1*

Condition evaluated, then rvalue-decayed, must be int (T0012), integer-promoted.

Case labels collected through body structure: blocks, `if`, loops, labels; not into nested switches. Case value frozen, int constant (M0001), normalized to promoted type.

- `rule/duplicate-case` duplicate value or second `default`: T0007.

Runtime: case branch to each label's block, default to `default` or exit. Body in own scope, `break` targets exit. Yields unit.

Comptime: matching label, else `default`, else yields unit without evaluating body. Body then evaluated *seeking* the label: nodes skipped until it; blocks walk their statements; `if` seeks into then, then else, conditions unevaluated; a loop seeks into its body, then continues as a normal loop. Reaching the label resumes normal evaluation. `break` caught.

#### Case

Comptime while seeking or switching: evaluates its statement. Runtime: jumps to its block, then evaluates its statement; yields unit.

- `rule/case-outside-switch` no enclosing switch: T0018.
- `rule/case-past-defer` case entering past a deferred expression's scope: T0017.

### Match

*verified 425c5ff1*

`match (s) { pattern => e, ... }`. Subject evaluated, fail if incomplete, collapsed.

- `rule/pointer-pattern` pointer subject: T0059.
- `rule/match-subject` not int or tagged union: T0011.

Own scope. Owned subject moved into fresh storage bound to the match local; borrowed subject bound as-is. Dispatch: tag for unions, then integer-promoted.

Arms checked in order:
- binding or `_`: catchall.
- variant pattern on unions; int literal or value pattern on ints. Value pattern frozen expecting dispatch type, int constant, else T0060.
- `rule/floating-case` float pattern: T0061. Any other pattern: T0035.
- `rule/unreachable-arm` arm after catchall, after all variants, or duplicate value: T0032.
- `rule/nonexhaustive-match` no catchall and not all variants: T0033, naming missing variants.

Runtime: each arm in own block and scope, arm pattern bound, arm value kept as place only if subject borrowed and arm declared no places, else rvalue-decayed. Owned subject dropped after each arm. Each arm completed expecting the previous arm's expectation. Without catchall, default block unreachable.
- `rule/mixed-arms` void and valued arms: T0012.
- arms merged by [merge](#merge).

Comptime: dispatch must be constant; selected arm bound and evaluated.
- `rule/quirk-comptime-match-no-drop` owned subject not dropped after a comptime arm.

#### Variant pattern

`V`, `V(p)`, `S::V(p)`.
- `rule/variant-qualifier` `S` not the subject's underlying type: T0012.
- unknown variant: T0049. Non-void variant without inner pattern: T0034.
- `rule/void-variant-payload` void variant with inner pattern other than binding or `_`: T0019.
- `todo/nested-payload-patterns` other inner pattern: T0042.

#### Arm binding

Binding pattern binds whole subject; variant with binding binds its payload, qualifiers of subject added. Binding declared `T&` or `const T&` binds a reference; otherwise a by-value copy, unqualified.
- `rule/binding-behind-reference` borrowed subject, by-value binding of nodrop or nocopy: T0062.
- owned subject consumed, invalidated as moved: by-value binding; or variant without binding whose payload is not nodrop.

### Labels and goto

*verified uncommitted*

`label: s`: runtime, emitting: jump into label block and continue there; then s evaluated.

`goto label`: jump; diverges. `goto *e`: e converted implicitly to `const void*`; jumps to any address-taken label; diverges. `&&label`: `void*` constant of label in named function, or current runtime function.

- `rule/comptime-goto` goto in comptime: M0021.
- `rule/defer-jump` goto inside deferred expression: T0058.
- `rule/quirk-sealed-goto` goto or `goto *` inside a [sealed](#materialize) quote: T0076, even when the label lies inside the quote; labels are function-wide names.
- `rule/label-address-context` `&&label` in comptime without named function: T0018.

### Return

*verified uncommitted*

`return e;`, `return;`. Unevaluated: diverges. Return type: current one, else signature's evaluated return type; no signature: M0039.

- `rule/defer-return` inside deferred expression: T0058.
- `rule/sealed-return` runtime, inside a [sealed](#materialize) quote: T0076.
- `rule/noreturn-return` return type `unreachable`: T0057.
- `rule/void-return-value` void return type and explicit value, even a void one: T0056.
- `rule/missing-return-value` non-void return type and no value: T0020.
- value completed expecting return type, converted implicitly.

Comptime: return stop carries value; none is unit typed as return type. Runtime: postcondition checked (see [Contracts](#contracts)), every scope's defers expanded and scopes dropped, return emitted; diverges.

#### Fallthrough

End of a function body, runtime or after the comptime checks of [Dependent call](#dependent-call): `void` returns; int `main` returns 0; otherwise, with explicit returns required and block reachable: M0002; else unreachable emitted.

### Yield

*verified 425c5ff1*

`yield e;`, `yield;` (unit). Requires a yielding target: enclosing yielding block in this body.

Value evaluated; incomplete marked as yielding; then rvalue-decayed. Comptime: yield stop. Runtime: scopes inside target left (defers expanded), value recorded for target, emission suppressed, diverges.

- `rule/untargeted-exit` no target: unevaluated diverges; inside deferred expression T0058; else T0018.

### Break and continue

*verified uncommitted*

Comptime: stop. Runtime: nearest target (loop or switch for `break`, loop for `continue`), scopes to it left, defers expanded, jump; diverges. No target: [Yield](#yield) `rule/untargeted-exit`.
- `rule/sealed-loop-exit` evaluated, a [sealed](#materialize) quote between the exit and its target: T0076. A target inside the quote is not crossed; the nearest target is taken across non-root quotes, so a loop of library code can catch a caller's `break`.

### Defer

*verified 425c5ff1*

`defer e;`. Yields unit.

- `rule/comptime-defer` comptime: M0021.
- `rule/nested-defer` inside deferred expression: T0017.

Operand dry-run once in a detached runtime child, emission enabled, deferred. Recorded on innermost scope with current expectations, source, visibility.

- `rule/defer-fallthrough` dry run stops (diverges, jumps): T0038. Incomplete: T0052.
- `inv/defer-expansion` defers expanded in reverse order on normal scope close, and on every exit leaving the scope. Each expansion is a fresh evaluation in a deferred scope; stops ignored.
- `rule/quirk-defer-evaluated-twice` operand evaluated at declaration and again at each expansion.

### Unsafe

*verified 425c5ff1*

`@unsafe e`: e evaluated with permission unsafe; an incomplete result keeps the permission when resumed.

### Escape

*verified uncommitted*

`e!`: e evaluated with escaping allowed; an incomplete result keeps it when resumed. Every root quote [materialized](#materialize) while it holds is unsealed. Covering no splice, or none that escapes, is allowed.

- `todo/unreachable-op` `unreachable` control op.

### Binary operator

*verified 425c5ff1*

`&&`, `||`: see [Short circuit](#short-circuit). Otherwise both operands evaluated, fail if incomplete, as arithmetic operands: rvalue-decayed, integer-promoted.

- `rule/pointer-offset` `p + i`, `i + p`, `p - i`: offset by i converted to `i64`, signedness kept.
- `rule/pointer-difference` `p - q`: `i64` difference.
- `rule/pointer-comparison` comparison with a pointer side: both pointers, both to [common pointer](#common-type); one pointer, other converted implicitly to its type. Yields `bool`.
- otherwise both converted implicitly to the [common type](#common-type) of the operands; comparisons yield `bool`, others the common type.
- `rule/quirk-shift-lhs-type` shifts: common type of lhs with itself, so rhs converted to lhs's promoted type.
- `rule/invalid-binary-operands` no common type: T0014.
- `rule/integral-only` `%`, `&`, `|`, `^`, `<<`, `>>` with float common type: T0046.

### Short circuit

*verified 425c5ff1*

lhs evaluated, condition-coerced. Comptime: lhs decides (`||` true, `&&` false), yields lhs; else yields rhs condition-coerced. Runtime: branch; yields `bool` merged from both paths; rhs diverging leaves only short path.

### Unary operator

*verified 425c5ff1*

- `!e`: condition-coerced, negated.
- `-e`, `~e`: arithmetic operand. `-` on int or float, `~` on int. `~` float: T0046. Otherwise T0045.
- `++x`, `--x`, `x++`, `x--`: requires impure; x collapsed.
  - `rule/increment-addressable` not a reference: T0055. Const referent: T0005.
  - int or float: plus or minus one; pointer: offset by one; else T0045.
  - written back; prefix yields x as place, postfix yields old value.

### Coerce

*verified 425c5ff1*

- truthy (from conditions): value evaluated, fail if incomplete, condition-coerced.
- implicit, cast, convert: target type evaluated, fail if incomplete; value completed expecting target. Implicit mode: [implicit conversion](#implicit-conversion). Cast mode: [explicit conversion](#explicit-conversion).
- `rule/quirk-convert-as-cast` convert mode treated as cast.

### Assignment

*verified 425c5ff1*

`t = v`, `t op= v`. Requires impure. t evaluated, fail if incomplete, collapsed to `T&`.

- `rule/assign-addressable` t not a reference: T0055.
- `rule/no-const-reassignment` T const: T0005.
- `t = v`: v evaluated, completed expecting T. `t op= v`: t as arithmetic operand, v as arithmetic operand, combined as [Binary operator](#binary-operator).
- result converted implicitly to T; old t dropped; t initialized with result. Yields t as `T&`.
- `rule/quirk-compound-assign-no-expectation` `op=` gives v no expectation, `=` expects T.

### Address-of

*verified 425c5ff1*

`&e`: e evaluated, fail if incomplete, collapsed.
- function value: function pointer. `rule/comptime-function-address` comptime-only function: T0065.
- `T&`: `T*`. `rule/bitfield-address` bitfield reference: T0043.
- otherwise T0055.

### Dereference

*verified 425c5ff1*

`*p`: p evaluated, fail if incomplete, rvalue-decayed, must be pointer (T0002). Requires nonsafe. Yields `T&`.

### Copy

*verified 425c5ff1*

`@copy e`: e evaluated, fail if incomplete; value type taken. Read if a reference, converted implicitly to value type.
- `rule/uncopyable` nocopy or nodrop: T0073.

### Move

*verified 425c5ff1*

`@move x`. Requires impure.
- `rule/move-local-only` x not syntactically a local: T0027.
- `rule/quirk-move-binding-shape` binding of x not a place, or not `T&`: T0019.
- `rule/no-const-move` T const: T0005. `@unsafe_move` T: requires nonsafe.

x's place lifted, x invalidated as moved; yields lifted value.

### Leak

*verified 425c5ff1*

`@leak x`. Requires nonsafe, impure.
- `rule/leak-local-only` x not syntactically a local: T0027.
- x evaluated, fail if incomplete, must be a reference, else T0012.
- nodrop T: x invalidated as leaked, yields unit.
- `rule/quirk-leak-droppable-passthrough` droppable T: no-op, yields x itself as place.

### Adopt

*verified 425c5ff1*

`T x = @adopt e;`, optionally `@unsafe @adopt e` (permission unsafe). Requires nonsafe, impure. Yields unit.
- `rule/adopt-declaration-only` `@adopt` anywhere else: T0030.
- `rule/comptime-adopt` comptime: M0021.
- e evaluated, fail if incomplete, must be a reference (T0012). Declared type completed expecting adopted referent.
- `rule/adopt-exact-type` qualifiers only gained, and unqualified types identical, else T0012.
- x bound to e's storage, taking over its obligations; no copy.

### Type expression

*verified uncommitted*

Type node: declared type forced, yields its [type result](#type-result). `T[_]`: undefined array of T as partial; incomplete, no fallback.

Runtime context evaluates types frozen. A type expression is memoized; a comptime stop escaping: M0039. Evaluated type described: rvalue-decayed, must be a type value (T0053).
- `todo/runtime-local-in-type` a runtime local read in a type expression evaluated at runtime has no binding in the frozen context: internal error, as `T[sizeof(x)]` or `@type.equal(@type.type_of(x), T)`.

Construction:
- struct, union, tagged union declared forward first, so members may refer to it.
- member and element types must be complete and visible: T0009.
- array bound frozen: `void` is unbounded `T[]`; int, non-negative, within `u64` (T0011); otherwise M0001. `_` bound while constructing: T0052.
- function type parameters adjusted as [Parameter type](#parameter-type).
- `rule/staged-squash` staged over a staged type is that type: `^^T` and `^T` with `T = ^U` are `^U`; staged shapes formed by quotes squash likewise.
- `rule/field-traits` owned field nodrop, nocopy or `@unsafe_move` without the container declaring it: T0029.
- `rule/unreachable-pointee` pointer to `unreachable`: T0011.
- copy-traits declaration takes move semantics of the named type.

### Type operator

*verified uncommitted*

`sizeof`, `alignof`, `offsetof`: subject [deduced](#subject-type); then yields constant of unsigned pointer-width int.
- `sizeof`, `alignof`: complete object required, T0009.
- `offsetof(T, a.b[i])`: complete object required; fields followed through anonymous members (T0003), bitfield T0019; index requires array (T0012), frozen int (M0001), adds element size times index.
- `@type.size_of(T)`, `@type.align_of(T)`: as `sizeof`, `alignof`.
- `rule/type-query-operand` other type operators evaluate their type operands as [type expressions](#type-expression), nothing deduced.
- `is_int`, `is_float`, `is_pointer`, `is_reference`: kind of the type; `is_signed`: a signed int. `rule/is-const-top-level` `is_const`: the type's own const only, so `const T&` is not const. Never fail; yield `bool`.
- `equal(A, B)`: alike.
- `pointer_inner`, `reference_inner`: pointee, referent; other type: T0002.
- `decay(T)`: referent of a reference, qualifiers dropped; then array to pointer to element, function to function pointer.
- `rule/type-of-as-is` `type_of(e)`: e evaluated as for [subject type](#subject-type), yielding its type as-is: a place is `T&`.
- `todo/type-ops` member, aggregate: `todo!`.

### Member

*verified 425c5ff1*

`b.m`: b evaluated, fail if incomplete, collapsed.
- `rule/quirk-member-auto-dereference` pointer or array subject dereferenced first (requires nonsafe); an array yields its first element's member.
- non-reference b held in a temporary.
- member path through anonymous members; unknown: T0003.
- each step adds owner's const and volatile. Yields `F&`.

### Index

*verified 425c5ff1*

`a[i]`: both arithmetic operands; added as pointer offset (either order, so `i[a]`); dereferenced, requires nonsafe; yields `T&`.

### Initializer

*verified 425c5ff1*

`T{...}`, `{...}` against expected type. Target type evaluated, known partial used as guide; then by kind. Each value completed expecting its slot type, converted implicitly.

- struct, union: named entries by name (T0003); positional entries continue after the previous entry. Past the last field: T0016.
- array: more entries than bound: T0016; named entry: T0003. `T[]` bound becomes entry count.
- `rule/quirk-braced-string-array` char array with a single `str` entry: the whole array string-converted.
- tagged union: exactly one named entry (else T0054), a variant (T0003), payload.
- scalar: one positional entry; named: T0003; else T0054.

### Is

*verified 425c5ff1*

`e is V(b)`: yields `bool`, tag equals variant.
- `todo/is-non-variant` non-variant pattern: T0042.
- e collapsed; pointer: T0059; not tagged union: T0002; variant as [Variant pattern](#variant-pattern).
- owned subject held in a temporary.

### Unpack

*verified 425c5ff1*

`@unpack {a, b} = e`. Requires impure. Yields unit.
- `rule/unpack-owned` e not an owned struct (a place is a reference): T0012.
- unknown field T0003; field twice T0007; binding name twice T0006.
- `rule/unpack-nodrop` unbound nodrop field: T0040.
- constant aggregate materialized. Each bound field lifted (bitfields copied) into fresh named storage. Comptime constant aggregate: fields taken directly.

### Definition

*verified 425c5ff1*

Def reference: its binding at the current visibility point.
- type constant: [type result](#type-result).
- non-comptime function, runtime emitting: prototype declared, def requested for generation.
- global address: requested; runtime emitting: global declared.
- `inv/generation-request` request only for defs of this unit or static linkage.

### Name

*verified 425c5ff1*

`a::b::n`: resolved, then as [Definition](#definition). Unresolved: qualified variant constructor `S::V`; then intrinsic type by root name, as type result; else T0001.

Resolution: candidates looked up, filtered to visible.
- `rule/incompatible-tag` tagged lookup, any declaration with another tag: T0067.
- `rule/ambiguous-symbol` multiple candidates, more than one defining, or any pair not equivalent: T0008. Equivalent: same link name, neither static, alike function signatures or alike reference types.
- first candidate wins.

### Scope access

*verified 425c5ff1*

`T::V`: T evaluated as type, then must be tagged union (T0002); yields variant constructor, else T0003.

### Piped

*verified 425c5ff1*

Inner expression evaluated unchanged.

### Call

*verified uncommitted*

`f(args)`, `f(.p = a)`. Callee evaluated, then call evaluated, then result completed expecting the call's expectation.

Callee prepared:
- staged reference read; a code-typed callee is its known value (a comptime local holding a quote yields the quote). Quote or opaque callee: named arguments T0017; else [Apply](#apply).
- `fn&` kept; function value made a `fn&` constant; otherwise rvalue-decayed, function pointer dereferenced to `fn&`.
- curried value unwrapped, its arguments applied first.

Routed to [Dependent call](#dependent-call):
- comptime context, not a variant constructor.
- comptime-only callee type.

Otherwise:
- `rule/runtime-named-arguments` named arguments: T0072.
- `rule/not-callable` non-function: T0002.
- `rule/argument-count` applied plus given fewer than params, or more when not variadic: T0015.
- `rule/call-requirements` non-constructor: non-safe contract requires nonsafe; requires impure.
- each argument completed expecting its param, converted implicitly. Variadic extras: [default argument](#default-argument).
- unevaluated: placeholder of return type. Comptime constructor: variant built. Else call emitted.
- `rule/noreturn-call` return type `unreachable`: unreachable emitted, diverges.
- runtime emitting def call: [Contracts](#contracts) around it.

### Dependent call

*verified uncommitted*

Call needing comptime evaluation: comptime parameters, comptime-stage functions, or comptime context.

- `rule/dependent-callee` callee not a def function: M0029. Variadic: M0021.
- *spliced*: runtime context, comptime-stage function. *runtime*: runtime context, not spliced; produces a specialization.

Arguments bound: curried values first; named arguments to comptime params by name (any param of a comptime-stage function); positional fill remaining non-defaulted params in order.
- `rule/unknown-parameter` named argument matching no such param: T0072. Twice: T0007.
- `rule/dependent-argument-count` too many positional: T0015.
- `rule/partial-application` fewer positional allowed only if something supplied and no runtime param given an expression; else T0015.
- not partial: unfilled defaulted params become holes; [Call](#call) `rule/call-requirements` apply.
- `rule/no-body` no definition: runtime T0042, comptime M0033.

Per param in order, in a callee frame whose comptime state persists across params:
- param type evaluated, adjusted as [Parameter type](#parameter-type).
- `rule/specialized-code-param` runtime, code-typed param: M0021. Comptime, reference param: M0021.
- param type naming a deferred param: [deduction](#deduction) from this param on; still deferred: T0023.
- staged param: [staged argument](#staged-argument). Runtime comptime param or spliced: argument frozen expecting param. Otherwise evaluated, completed expecting param, then converted implicitly; comptime call: exported. Default: evaluated apart, in a comptime child of the declaring unit.
- `type` param whose argument or default stays incomplete (`_`, `T[_]`): deferred; other incomplete: T0052.
- comptime param bound in frame, later param types see it; canonicalized.

After binding:
- deferred param left: T0023, unless the function generates a type (all params comptime, returns `type`) and the call is not partial: yields incomplete, completed from the expected type's memo origin.
- partial, and comptime-stage or an unbound comptime param: yields curried value.
- signature formed; every other visible declaration re-evaluated against bound values, must agree in params, comptime-ness, defaults and signature, else T0006.
- partial otherwise: specialization declared, yields it curried.
- runtime: specialization applied. One per def and comptime arguments, static, named `<link>.__cx_specialize.<n>`; called with runtime arguments; [Contracts](#contracts) around it.
- comptime: executed in frame. Precondition must hold (M0026). Body: root other than a non-yielding block returns its value; otherwise fallthrough: `void` returns unit, `unreachable` M0027, else M0002. Return value exported. Live nodrop comptime storage unconsumed: A0003. Postcondition must hold.
- `rule/quirk-comptime-main-fallthrough` comptime fallthrough of an int function named `main` exempt from M0002.
- spliced: result exported; staged quote result materialized.
- `rule/call-depth` comptime call depth limit: M0025.

#### Deduction

Runs once a param's type names a deferred param, over the arguments from that param on. No fixpoint: what a pass leaves deferred stays so.

1. Arguments other than closure literals (through pipes) typed as [subject type](#subject-type). Param type expression unified against it: through pointers, references, arrays, nominal qualifiers, staged, function signatures, and generator calls (arguments recovered from the memo origin of the actual type, the generator itself possibly an earlier param). Value types used unless the param is declared as reference.
2. Each deferred param inferred so far: its pending argument or default completed from the inferred type, bound in the frame.
3. Closure literals, in order, then step 2 again after each. Param type must be a function type of the closure's arity, else skipped.
   - declared closure param types unified against the param's, then step 2.
   - `auto` closure params: the param's parameter type, evaluated in the frame; not yet known: closure skipped.
   - closure body [synthesized](#quote) with those types; result lifted to `^R`, unified against the param's return type.

- `rule/conflicting-deductions` two inferences not alike: T0024.
- `rule/closure-deduction-errors` errors from synthesizing a closure body are reported as-is.

### Contracts

*verified 425c5ff1*

Runtime call to a def with pre or postcondition: params bound to const observed copies of arguments; precondition evaluated and asserted before call ("precondition failed"); result bound, postcondition asserted after. Conditions evaluated in the caller's runtime, pure expectations, permission per contract safety.
- `rule/void-postcondition-binding` result binding on a void result: T0066.
- `rule/contract-condition-value` condition stops: T0011.
- `inv/contract-both-sides` function entry and returns also assert its own contract, so each holds checked on both sides.

### Hole

*verified 425c5ff1*

`_`: incomplete; completes to the expected type as a type value.

### Comptime

*verified 425c5ff1*

`comptime e`: e frozen with no expectation, fail if incomplete. Type-valued: described type, partial fallback accepted. Otherwise completed expecting expectation, converted implicitly to it, else rvalue-decayed; exported.

### Quote

*verified uncommitted*

Captures: free locals of the quoted expression, quote params and inner declarations excluded. Runtime: bindings as-is. Comptime: rvalue-decayed, exported. Missing binding: M0029.

`rule/emit-squash` in a runtime context, a quote without parameters is its quoted expression evaluated in place: emitting code being emitted changes nothing. A [staged argument](#staged-argument) or a spliced quote literal stays a quote.

Otherwise incomplete, completing without expectation too. Expected staged shape (`^T`, or comptime function to a staged result, any param types): param count must match (T0015); typed quote params checked identical (T0012), evaluated in a comptime child with captures; yields quote of the shape.
- `rule/quote-materializes` non-staged expectation: quote materialized in place.
- `rule/quote-synthesis` nothing expected: typed by itself. Declared param types evaluated as above; an `auto` param: T0052. Body evaluated with a placeholder per param (the carried type for `^T`), captures shadowed, in an unevaluated detached child as [subject type](#subject-type); result R. Type: `^R` without params, else a non-decaying comptime function from the declared types to `^R`.

### Splice

*verified uncommitted*

Quote literal operand, optionally under `comptime`: incomplete; on expectation, arguments typed as [subject type](#subject-type), lifted, the quote completed against that signature, then applied. Otherwise code evaluated, fail if incomplete, converged, then [applied](#apply).

A staged value spliced without arguments (a [Local](#local) read at runtime): converged; a `^T` quote materialized; anything else yields itself.

### Intrinsic

*verified uncommitted*

`va_start(l, last)`, `va_end(l)`, `va_arg(l, T)`. l must be a reference alike `__builtin_va_list` (T0012), passed by pointer. `last` rvalue-decayed. Yields result, else unit.

Path intrinsics: operands, then result type checked, then emitted.
- `rule/strict-intrinsic-operands` value operands as evaluated: never read, decayed or converted. Reference-typed: T0002, so a place is passed as `@copy(x)`. Type operands as [type expressions](#type-expression).
- *alike operands*: alike ignoring top-level const, else T0014.
- int: operands ints (T0002); binary operands alike. Comparisons, `l_and`, `l_or`: `bool`; other unary, binary: the operand type. `to_float(v, F, signed)` F a float, `cast(v, I, sign_extend)` I an int (T0002): the target. `to_ptr(v, sign_extend)`: `void*`.
- float: operands floats (T0002); binary operands alike. Comparisons: `bool`; `neg`, other binary: the operand type. `to_int(v, I, signed)` I an int, `cast(v, F)` F a float: the target.
- pointer: `to_int(p, I)` I an int: I. `rule/byte-pointer-offset` `add`, `sub`: pointer, offset an int of pointer width (T0002), in bytes; the pointer type. `diff(a, b, T)`: alike pointers, `i64`. Comparisons: alike pointers, `bool`.
- `rule/bitcast-kinds` `internal.bitcast(v, T)`: pointer to pointer, or int to int of one width; else T0002.
- `internal.assert(c [, message])`, `internal.assume(c)`: c an `i1` (T0002); unit.

---

### Helpers

#### Rvalue decay

Repeated until nothing applies:
- staged value at runtime: materialized; diverging, an `unreachable` unit.
- array or `T[]`: pointer to first element; non-place held in a temporary. Element const if array const.
- `str`: `char*`, qualifiers kept.
- non-comptime function: function pointer.
- `rule/comptime-function-read` reference to a comptime-only function: a function name yields the function itself; storage is [read](#read).
- `T&`: read.

#### Read

Load from `T&`.
- `rule/nocopy-read` nocopy T: T0013.
- volatile T: requires impure.
- `rule/quirk-external-read-impure` value is a global place or constant address, T mutable, runtime: requires impure.

#### Implicit conversion

Value to target T, else T0013.

Directly retyped:
- `T[n]` to `T[]`, same element, same move semantics: retyped `T[n]`.
- source `unreachable`, or staged `unreachable` to staged.
- source compatible with T and qualified for it.

Otherwise one step, then retried:
- referent `str` or non-comptime function, T pointer: decay step.
- `E[n]&` to `E[n]` or `E[]`: same element, length if given, not nocopy, same move semantics: copied, retyped.
- reference to reference whose source is a reference to a reference: decay step.
- `rule/bitfield-bind` bitfield reference to compatible non-bitfield reference: T0043. Other reference to reference: fails.
- reference source, or array to pointer: decay step.
- staged to non-staged: decay step.
- `rule/reference-binds-temporary` non-reference to `U&`, compatible with U: address taken, of a temporary unless `str` or function designator.
- `rule/string-to-char-array` `str` constant to `char[n]` or `char[]`: NUL-padded char array constant; `[]` bound is length plus one; longer than bound: T0016.
- move semantics differ: fails.
- `rule/quirk-null-by-value` int constant 0 to pointer: null. Decided by the value, so any folded 0.
- int, float, pointer to `bool`: truth.
- int to same-width int: retyped, no cast.
- int or float to int or float: cast.
- pointer to int: requires nonsafe, cast.
- pointer to pointer, one side `void`, qualified: retyped.
- `rule/discarding-function-pointer` function pointer to one returning `void`, otherwise alike: retyped; nodrop result: T0048.
- `rule/quirk-explicit-discard-stricter` same, under explicit conversion: requires nonsafe; implicit does not.

Compatible: same type; or same move semantics, and pairwise staged, pointers (either `void`, or compatible pointees), references (same bitfield, compatible), arrays (same length, compatible), unbounded arrays (compatible). Qualified: pointee or referent const and volatile only gained.

#### Explicit conversion

`void` target: unit retyped. Otherwise [implicit conversion](#implicit-conversion) in explicit mode, then:
- pointer and array sources rvalue-decayed; move semantics differ: fails.
- pointer to pointer: requires nonsafe.
- `char*` to `str&`: requires nonsafe.
- int to pointer: requires nonsafe unless constant 0.
- otherwise T0013.

#### Condition coercion

Rvalue-decayed. `bool` unchanged; int, float, pointer compared unequal to zero or null, folded for constants; else T0047.

#### Common type

Both integer-promoted. Floats: wider. Float and int: the float. Ints: wider width's signedness; equal width signed only if both are. Otherwise none.

Common pointer: pointee of the `void` side if any, else left; adds the other side's const and volatile.

#### Default argument

Rvalue-decayed; float narrower than `f64` to `f64`; else integer promotion.

#### Merge

Values reaching a join, each with its block. None: diverges. Merged type: any void arm, no value (yields unit); alike types keep the first; otherwise pointers to common pointer, else common type.
- `rule/quirk-merge-first-arm-fallback` no common type: the first arm's type, others then fail conversion.
- `rule/place-merge` stays a place only if allowed and all arms are references alike to the merged type; else every arm rvalue-decayed and type re-merged.
- each arm converted implicitly into one result.

#### Type result

Type as a value; in runtime emitting context: T0017. Unsettled type forced. `T[]`: incomplete with partial fallback; completes from an expected `T[n]` of the same element (else T0011; non-array T0052).

#### Subject type

Operand evaluated in an unevaluated detached runtime child seeing frozen known values (quotes with their captures shadowed), placeholders for the rest. A placeholder of a comptime-only type is opaque: no storage, its contents unknown. Diverging: `unreachable`. `type` value: that type. `rule/quirk-sizeof-string-literal` `str` constant: `char[len + 1]`. Otherwise referent type.

#### Parameter type

Forced. `type` in a parameter that is neither marked `comptime` nor of a comptime-stage signature: T0017. Array or `T[]`: element pointer. Non-comptime function: decaying, its function pointer; `rule/explicit-fn-indirection` non-decaying (`fn`): T0011.

#### Staged argument

Piped marks stripped. Quote literal: used as-is. Target a function to code (not `^T`): operand evaluated. Local already holding code: collapsed. `rule/explicit-staged-argument` otherwise, a piped operand is captured as a parameterless quote; any other: T0075, so `f(emit e)`, `e |> f()` and `f() <| e`. Completed expecting target, converted implicitly.

#### Apply

Code applied to arguments; needs staged shape (T0002), argument count (T0015). `rule/closure-params` code params (`^T`, or a function to code) take [staged arguments](#staged-argument), so `^T` code is spliced at each use; others are frozen expecting the param, evaluated once. Runtime: quote materialized with them. Comptime: new quote with params bound as captures.

#### Materialize

Quote inserted at runtime. Opaque code: an opaque value of its result, unit for `void`. Otherwise quote constant required (M0029); count (T0015); comptime: M0021. Quoted expression evaluated in a fresh quoted scope with captures, at the quote's source and visibility; `rule/sealed-root-quote` the scope is sealed when no quoted scope encloses it (a root quote, inserted into hand-written runtime code) and escaping is not allowed ([Escape](#escape)); quotes materialized inside another are never sealed; params bound to the arguments as-is; result completed expecting result type, converted; `void` result yields unit.

## Comptime execution

*verified 425c5ff1*

Comptime storage and operations, as used by evaluation in comptime context.

### Memory

*verified 425c5ff1*

Storage is regions of bytes, per-byte initialized flags, and *cells*: non-byte constants (addresses, functions, quotes, types) held at an offset and size. Regions numbered globally, never reused; shared by a context and its children.

- allocation: uninitialized, sized by layout. Incomplete type: M0021.
- `rule/comptime-invalid-access` M0040 when: region gone; access outside its size; uninitialized bytes read; part of a cell read; cell read as bytes; bytes read as an address, unless all zero (null); tag byte names no variant.
- write over part of a cell: rest of that cell becomes uninitialized.
- tagged union: payload at offset 0, one tag byte after the largest payload rounded to the largest alignment.
- `rule/quirk-union-decode-largest-first` union read: members tried largest first; first that decodes is the active member.
- string literal: one region per text, NUL-terminated; write: M0021.
- runtime global: read through a region holding its [initial value](#initial-value); write: M0021.
- other addresses (functions, labels) as storage: M0021.

### Slots

*verified 425c5ff1*

Each allocation has a slot: uninitialized, live, or invalidated. Only whole-slot values change state; writes through fields do not.
- read uninitialized: M0034. Read or write invalidated: A0004.
- write or initialize: live. Drop: uninitialized. Move, leak: invalidated.
- `inv/comptime-no-flow` state per slot only; no flow analysis, a dropped nodrop value not reported. Unconsumed nodrop at a comptime call's end: see [Dependent call](#dependent-call).

### Scalar operations

*verified 425c5ff1*

- ints: two's complement wrap to width, signed included. `bool` unsigned.
- `rule/comptime-zero-divisor` `/`, `%` by zero: M0023.
- `rule/comptime-undefined-arithmetic` M0037 when: minimum `/` or `%` by -1, signed; shift count negative or not below width; float to int out of range or NaN.
- floats: `f32` rounded after each step.
- pointer comparison: same base by offset; different bases, or null against address or function, only `==`, `!=` (unequal); functions by identity. Otherwise M0021.
- pointer offset: null plus 0 stays null; null plus other: M0037; storage address shifted, overflow M0022; function or label address: M0021.
- pointer difference: same storage base, byte distance over element size; different bases M0037; not a multiple, or zero-sized element: M0037.
- casts: int, float among each other; int 0 to null; null to 0; pointer to pointer keeps address. Otherwise M0021.
- `rule/comptime-variadic` variadic intrinsic: M0021.

## MIR emission

*verified 425c5ff1*

Runtime context writing a MIR function.

### Instructions

*verified 425c5ff1*

- `inv/after-terminator` instruction after a terminator in its block dropped; a terminator suppresses emission until the next block switch.
- `inv/unevaluated-forgets` unevaluated: nothing emitted. Store to a place forgets known values of slots bound to it; store through a pointer or to a global forgets every known value.
- reachability: from entry, constant branches followed on the taken side only. Unterminated unreachable blocks end in `unreachable`.
- operation results live in registers; registers have no address.

### Storage

*verified 425c5ff1*

- declared storage: place in the current MIR scope, flagged nodrop and adopted as applicable. Parameter: place, initialized at entry.
- scratch storage (temporaries, merges): one scope per function that is never closed, so never dropped.
- `inv/scope-drop` closing a scope emits a drop invalidation for each of its places, newest first. Drop semantics belong to MIR analysis.
- dereference: with assertions, pointer checked unequal to null ("dereferenced a null pointer"); then reinterpreted as reference.
- contract assertions omitted without assertions.

### Constants

*verified 425c5ff1*

- aggregate: zero members omitted. Union, tagged union: active member only.
- function def: prototype declared; requested for generation if of this unit or static. Constructor: [Variant constructor](#variant-constructor). Curried: must be a generated specialization, else M0021.
- address of a global: global with offset. Of a runtime place: local with offset.
- `rule/comptime-value-at-runtime` comptime region, or register address, reaching runtime: M0032.
- `todo/constant-lowering` any other constant (type, quote, ...): `todo!`.

### Operations

*verified 425c5ff1*

- int operations signed or unsigned by the operand type; `>>` arithmetic when signed.
- int cast sign-extends when the source is signed and not `bool`.
- pointer offset: constant index pre-scaled by element size; otherwise multiplied, signed by the index type.
- tag of a tagged union constant folded; otherwise read as sum index.

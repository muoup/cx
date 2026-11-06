# HMIR lowering

HMIR unit to MIR unit. Evaluation interprets comptime code, elaborates types, and emits runtime code in one pass over HMIR.

## Terms

*verified 425c5ff1*

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
- **comptime-only type**: `@type`, staged `^T`, dependent `_`, or a function that is comptime-stage, has a comptime parameter, or a comptime-only parameter or return type.

## Expression evaluation

*verified 425c5ff1*

Each node evaluated by kind. Comptime context seeking a selected `case` routes every node to the seek of [Switch](#switch).

- `inv/suppressed-diverges` after any node, runtime context with suppressed emission: diverges.
- `todo/error-node` error node.

### Literal

*verified 425c5ff1*

Int and float literals: constant of their suffix type. String literal: `str` constant. Unit: `void` constant.

### Local

*verified 425c5ff1*

Binding of the local, collapsed.

- `rule/comptime-unavailable-local` no binding, comptime context: M0029.
- `todo/runtime-unbound-local` no binding, runtime context.

### Let

*verified 425c5ff1*

`T x;`, `T x = e;`, `_ x = e;`, `auto& x = e;`. Yields unit. Each evaluation binds a fresh instance.

Declared type evaluated; for `auto&` the hole inside is evaluated instead, as referent. `@adopt` initializer goes to [Adoption](#adopt).

Declared type known, complete, visible: storage allocated up front, and at runtime default-initialized unless nocopy.

No initializer: declared type completed without expectation; storage declared; default-initialized unless nocopy or already initialized.

With initializer: e evaluated, completed expecting declared type, then:
- converted implicitly to declared type, when known.
- `_` or `auto&`: collapsed, kept as-is. Inferred type: `_` the value's own type, a reference when e is a place; `auto&` its referent.
- otherwise inferred type is the value type.
- inferred non-comptime function type becomes its function pointer.
- declared type completed expecting inferred, then `auto&` wraps it as reference, value converted implicitly to it.
- storage declared, then written if default-initialized earlier, else initialized.

Rules:
- `rule/runtime-staged-local` runtime, declared type is code: initializer required (T0017); bound as [staged argument](#staged-argument), no storage.
- `rule/comptime-function-local` runtime, comptime-only function type: value bound directly, no storage.
- `rule/local-object-type` function type: T0011. Incomplete or invisible type: T0009.
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

*verified 425c5ff1*

`label: s`: runtime, emitting: jump into label block and continue there; then s evaluated.

`goto label`: jump; diverges. `goto *e`: e converted implicitly to `const void*`; jumps to any address-taken label; diverges. `&&label`: `void*` constant of label in named function, or current runtime function.

- `rule/comptime-goto` goto in comptime: M0021.
- `rule/defer-jump` goto inside deferred expression: T0058.
- `rule/label-address-context` `&&label` in comptime without named function: T0018.

### Return

*verified 425c5ff1*

`return e;`, `return;`. Unevaluated: diverges. Return type: current one, else signature's evaluated return type; no signature: M0039.

- `rule/defer-return` inside deferred expression: T0058.
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

*verified 425c5ff1*

Comptime: stop. Runtime: nearest target (loop or switch for `break`, loop for `continue`), scopes to it left, defers expanded, jump; diverges. No target: [Yield](#yield) `rule/untargeted-exit`.

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

*verified 425c5ff1*

Type node: declared type forced, yields its [type result](#type-result). `T[_]`: undefined array of T as partial; incomplete, no fallback.

Runtime context evaluates types frozen. A type expression is memoized; a comptime stop escaping: M0039. Evaluated type described: rvalue-decayed, must be a type value (T0053).

Construction:
- struct, union, tagged union declared forward first, so members may refer to it.
- member and element types must be complete and visible: T0009.
- array bound frozen: `void` is unbounded `T[]`; int, non-negative, within `u64` (T0011); otherwise M0001. `_` bound while constructing: T0052.
- function type parameters adjusted as [Parameter type](#parameter-type).
- `rule/field-traits` owned field nodrop, nocopy or `@unsafe_move` without the container declaring it: T0029.
- `rule/unreachable-pointee` pointer to `unreachable`: T0011.
- copy-traits declaration takes move semantics of the named type.

### Type operator

*verified 425c5ff1*

`sizeof`, `alignof`, `offsetof`: subject [deduced](#subject-type); then yields constant of unsigned pointer-width int.
- `sizeof`, `alignof`: complete object required, T0009.
- `offsetof(T, a.b[i])`: complete object required; fields followed through anonymous members (T0003), bitfield T0019; index requires array (T0012), frozen int (M0001), adds element size times index.
- `todo/type-ops` pointer inner, reference inner, member, `typeof`, decay, aggregate, is-int, is-float, is-pointer, is-signed, equal: not produced by HIR, `todo!` here.

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

*verified 425c5ff1*

`f(args)`, `f(.p = a)`. Callee evaluated, then call evaluated, then result completed expecting the call's expectation.

Callee prepared:
- staged reference read. Quote callee: named arguments T0017; else [Apply](#apply).
- `fn&` kept; function value made a `fn&` constant; otherwise rvalue-decayed, function pointer dereferenced to `fn&`.
- curried value unwrapped, its arguments applied first.

Routed to [Dependent call](#dependent-call):
- unevaluated context, comptime-stage function: in a frozen comptime child.
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

*verified 425c5ff1*

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
- staged param: [staged argument](#staged-argument). Runtime comptime param or spliced: argument frozen expecting param. Otherwise evaluated, completed expecting param, then converted implicitly; comptime call: exported. Default: hole.
- comptime param bound in frame, later param types see it; canonicalized.
- incomplete `@type` argument deferred to [deduction](#deduction); other incomplete: T0052.

After binding:
- partial, and comptime-stage or an unbound comptime param: yields curried value.
- signature formed; every other visible declaration re-evaluated against bound values, must agree in params, comptime-ness, defaults and signature, else T0006.
- partial otherwise: specialization declared, yields it curried.
- runtime: specialization applied. One per def and comptime arguments, static, named `<link>.__cx_specialize.<n>`; called with runtime arguments; [Contracts](#contracts) around it.
- comptime: executed in frame. Precondition must hold (M0026). Body: root other than a non-yielding block returns its value; otherwise fallthrough: `void` returns unit, `unreachable` M0027, else M0002. Return value exported. Live nodrop comptime storage unconsumed: A0003. Postcondition must hold.
- `rule/quirk-comptime-main-fallthrough` comptime fallthrough of an int function named `main` exempt from M0002.
- spliced: result exported; staged quote result materialized.
- `rule/call-depth` comptime call depth limit: M0025.

#### Deduction

Deferred `@type` arguments inferred from later arguments' actual types, typed by dry run. Param type expression unified against actual: through pointers, references, arrays, nominal qualifiers, staged, function signatures, and generator calls (arguments recovered from the memo origin of the actual type). Value types used unless the param is declared as reference.
- `rule/conflicting-deductions` two inferences not alike: T0024.
- `rule/undeduced` left undeduced: T0023, unless the function generates a type (all params comptime, returns `@type`) and the call is not partial: yields incomplete, completed from the expected type's memo origin.

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

*verified 425c5ff1*

Captures: free locals of the quoted expression, quote params and inner declarations excluded. Runtime: bindings as-is. Comptime: rvalue-decayed, exported. Missing binding: M0029.

Incomplete until expected. Expected staged shape (`^T`, or comptime function from staged params to staged result): param count must match (T0015); typed quote params checked identical (T0012), evaluated in a comptime child with captures; yields quote of the shape.
- `rule/quote-materializes` non-staged expectation: quote materialized in place.

### Splice

*verified 425c5ff1*

Quote literal operand, optionally under `comptime`: incomplete; on expectation, arguments evaluated for their value types, the quote completed against that signature, then applied. Otherwise code evaluated, fail if incomplete, then [applied](#apply).

### Intrinsic

*verified 425c5ff1*

`va_start(l, last)`, `va_end(l)`, `va_arg(l, T)`. l must be a reference alike `__builtin_va_list` (T0012), passed by pointer. `last` rvalue-decayed. Yields result, else unit.
- `inv/va-only` only variadic intrinsics reach HMIR lowering.

---

### Helpers

#### Rvalue decay

Repeated until nothing applies:
- staged value at runtime: materialized; diverging, an `unreachable` unit.
- array or `T[]`: pointer to first element; non-place held in a temporary. Element const if array const.
- `str`: `char*`, qualifiers kept.
- non-comptime function: function pointer.
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

Operand evaluated in an unevaluated detached runtime child seeing frozen known values, placeholders for the rest. Diverging: `unreachable`. `@type` value: that type. `rule/quirk-sizeof-string-literal` `str` constant: `char[len + 1]`. Otherwise referent type.

#### Parameter type

Forced. `@type` without `comptime`: T0017. Array or `T[]`: element pointer. Non-comptime function: function pointer.

#### Staged argument

Quote literal, or local already holding code: used as-is. Otherwise operand captured as a parameterless quote. Completed expecting target, converted implicitly.

#### Apply

Code applied to arguments; needs staged shape (T0002), argument count (T0015). Runtime: arguments evaluated, converted, quote materialized. Comptime: arguments taken as staged arguments, new quote with params bound as captures.

#### Materialize

Quote inserted at runtime. Quote constant required (M0029); count (T0015); comptime: M0021. Quoted expression evaluated in a fresh quoted scope with captures, at the quote's source and visibility; non-reference params get storage; result completed expecting result type, converted; `void` result yields unit.

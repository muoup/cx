# Temporary agent handoff: C-compatible parsing for computed types and comptime parameters

AI-generated conversation state, written 2026-09-30 on branch feat/2ltt. DESIGN, NOT IMPLEMENTED SPECIFICATION. Paths are repository-relative. Retire this file once its content is incorporated into maintained docs. Reading it does not authorize deletion or implementation.

## Intent / scope

The overhaul moves CX toward 2LTT:
- MIR intrinsics (compiler/cx-mir/src/expr/intrinsic.rs) are the semantic atoms, and types are opaque during typechecking.
- C operators desugar to an auto-imported prelude, lib/std/intrinsic/native_ops.cx. For example, `a + b` becomes `native_ops::add(_, _, a, b)`.
- Zig-style comptime parameters replace `<T>` templates.
- A proposed HMIR mixes U0 and U1. HMIR -> MIR lowering is the staging step, and MIR stays runtime-only and analyzable.

Background audits: ~/workspace/Papers/2209.09729-2ltt/cx-parallels-audit.md and cx-staging-pipeline-audit.md.

This doc covers only the parser problem. Once types are computed comptime values, the parser can no longer tell whether a term is a type.

## Hard constraints (user decisions)

- CX stays backward compatible with C. REJECTED: `let x: T = e`, name-first/Rust/Zig declarations, `fn` items, dropping C declarator syntax, removing C casts.
- REJECTED: `#` as a force-type prefix. compiler/cx-lexer/src/lexer/scanner.rs:48 treats every `#` as the start of a directive. Also, a declaration usually starts a line (which would make it a directive in C), and `#x` stringifies inside `#define` bodies.
- ACCEPTED compatibility break: new keywords and `_` as a deduction hole may collide with C identifiers. These are the only accepted breaks. A future `cx migrate` could rename colliding C identifiers to reserved `__name` spellings.
- The preferred style is clean idioms over escape hatches:
  - `auto x = e;` (C23 inference)
  - `e as T` for right-hand-side typing
  - an explicit escape, used only in rare cases

## Governing rule

New syntax may only use token sequences that are syntax errors in C. Valid C, including `.c` files and `#include` regions (IncludeBegin/IncludeEnd), parses exactly as it does today. Extensions fire only where C would have rejected the input. Check every future syntax proposal against this rule.

## Current type-classification sites (to be replaced or narrowed)

The parser currently classifies types with a cross-module C lexer hack: preparse builds GlobalPreparseRegistry from typedef, struct, union and enum names (compiler/cx-parsing/src/preparse.rs, compiler/cx-preparse-data). The call sites are:

- compiler/cx-parsing/src/parse/statement.rs:101: declaration versus expression at statement start
- compiler/cx-parsing/src/parse/identifier.rs:80 and templates.rs (`note/unnote_templated_types`, `temporary_type_names`): `f<` as template arguments versus less-than
- compiler/cx-parsing/src/parse/operators.rs:92: `(T)x` cast versus a parenthesized expression
- compiler/cx-parsing/src/parse/expressions.rs:595: `sizeof(T)` versus `sizeof(e)`
- compiler/cx-parsing/src/parse/types.rs:51: an ad-hoc lookahead where an identifier followed by `=` or `.` is not a type

Template arguments currently use `parse_initializer` with the declared name required to be absent (templates.rs:89). This is the "decompose an initialization into a standalone type" approach, and it has caused problems before. The declarator parser competes with the expression parser for `(`, `[` and `*`. For example, in `f(T (x))` the `(x)` could be a declarator or a call. Do not reuse that approach for comptime arguments. See "Type-names without a declared name" below.

## Escape: `@type(e)`

- `@type` names the universe. Declaring `@type T` binds a type value, and applying it as `@type(e)` turns the type value `e` into a type (the 2LTT splice/instantiate).
- The two forms are told apart by whether `(` immediately follows `@type`. `@` is already the builtin prefix (`@unsafe`, `@leak`, `@nocopy`).
- Example: `@type(add_result(A, B)) (*fp)(int);`
- The escape should be rare. The rules below cover common code without it.

## Declaration contexts

| Context | Rule | Registry needed? |
|---|---|---|
| File scope, parameter lists, struct bodies | Always a declaration. A registry- or keyword-led head takes today's C path. Otherwise parse a **computed head** (a postfix expression: identifier, `::` path, call, member access), then a normal C declarator. | Only to decide whether `(` after a leading name is a declarator (C `size_t (*fp)(int)`) or a call (`opt(T) x`). |
| Block statement start, registry-led | Today's C path. | Yes |
| Block statement start, `head IDENT ...` | Declaration. In C, a call or postfix expression followed by an identifier is a syntax error. Example: `add_result(A, B) result = add(a, b);` | No |
| Block statement start, `head *p = ...` / `head &r = ...` | Declaration. In C grammar, the left side of `=` must be a unary expression, so this is a syntax error in C. | No |
| Block statement start, `head *p;` | Stays an expression (valid C: a discarded multiplication). The elaborator gives a targeted error when the left operand is a type value: "multiplying a type; use `auto`, `as`, or `@type(...)`". Never silently reinterpret it. | — |
| `(T)x` casts | Unchanged, registry only. Computed types must use `as`. | Yes |

- Generalize the types.rs:51 lookahead: a known type name at statement start is a declaration only when it is followed by a declarator start (identifier, `*`, `&`, `(`, qualifier). Once types are values, statements such as `A == B;` and `T::is_int(...)` become possible.
- Return types may name parameters bound later in the same prototype: `T opt::unwrap(comptime @type T, ...)`. The name is resolved during elaboration, and the parser does not care.

## Type-names without a declared name

These appear in call arguments (comptime type arguments), `as` operands, `sizeof(...)` and `@type(...)`. There are two parse paths that never compete:

1. **Registry- or keyword-led** (`int`, `const char*`, `struct foo`, known typedefs, `@type`-declared names): parse a proper C type-name (specifier-qualifier list plus abstract declarator), including `int (*)(int)`. In argument position this is a C syntax error, so it's free to use.
2. **Anything else:** parse an ordinary expression, then apply the **trailing-suffix rule**. A maximal run of `*`, `&` or `const` that ends at `,`, `)`, `]`, `;` or the end of an `as` operand is a type suffix. In expressions, `*` directly before those tokens is always a syntax error, so the rule is unambiguous. Examples: `f(opt(int)*, x)`, `x as opt(int)*`.

Parenthesized abstract declarators exist only on path 1. Computed function-pointer types use `@type(...)` or an alias.

## `auto` and `as`

- `auto x = e;` is C23 type inference, and the token already exists (IntrinsicType::Auto).
- `as` is already a keyword (KeywordType::As, used for import aliases).
- `as` binds at C-cast (unary) precedence: `a as long + b` equals `(long)a + b`.
- Inside an `as` operand, a `*`/`&` run followed by a token that can start an operand is binary. So `x as int * y` means `(x as int) * y`.
- `e as T` CHECKS `e` against `T` (bidirectional). It does not convert whatever `e` synthesizes. This lets `auto x = e as T` replace `let x: T = e`:
  - `auto p = { .x = 1 } as Point;` (`{` at expression start is already an initializer list: parse_structured_initialization)
  - `auto n = opt::none(_) as opt(int);`
  - `auto b = 0 as u8;`
- `as` is stricter than a C cast. Proposed (not final):
  - **Allowed:** numeric conversions, `void*` <-> `T*`, adding qualifiers, enum <-> integer, and ascription.
  - **Not allowed:** removing const, integer <-> pointer, unrelated `T*` <-> `U*`, and function-pointer casts. These need `@bitcast`-style builtins or a C-style `(T)x`, which stays for compatibility.
- Intended to desugar through a native_ops conversion function, so the table of allowed conversions is library code.

## Comptime parameters

- Comptime arguments are ordinary positional parameters, which removes template parsing entirely. Declaration: `T opt::unwrap(comptime @type T, opt(T) self)`.
- Call: `opt::unwrap(_, x)`. `_` is a deduction hole, meaningful only in CX-mode argument positions whose parameter is comptime. It is consistent with `_` as the existing pattern wildcard (`this is opt::some(_)`). gettext's `_("...")` is a macro that is expanded before parsing.
- Deduction uses argument types AND the expected result type: `opt(int) n = opt::none(_);`. Operator desugaring uses the same machinery: `native_ops::add(_, _, a, b)`.
- Any binder whose declared type is literally `@type` (a comptime parameter, or a local `@type U = add_result(T, int);`) is registered as a type name for its lexical scope. This is purely syntactic, so it's allowed.
- Replace `temporary_type_names` (a counting map) with a lexical scope stack of type and value bindings. It also correctly lets a variable shadow a typedef, which C requires.
- `auto T = make_type();` is invisible to the parser as a type. The elaborator should diagnose later misuse: "`T` holds a type; declare it `@type T`".
- Optional, not decided: `comptime @type T = _` marks a parameter as omittable at call sites, only as part of a leading run of such parameters, omitted all or nothing. Example: `opt::unwrap(maybe)`.

Example: the native_ops signatures need no escapes.

~~~cx
comptime @type add_result(@type A, @type B) { ... }

comptime expr add_result(A, B) add(@type A, @type B, expr A x, expr B y) {
    @type R = add_result(A, B);
    ...
}
~~~

## Parser work implied (all additive; C mode and include regions never reach it)

1. Computed-head declarations at file scope, in parameter lists and in struct bodies.
2. The block-statement rules `head IDENT` and `head *p =`.
3. Registry-/keyword-led C type-names plus the trailing-suffix rule, for call arguments, `as`, `sizeof` and `@type(...)`. These replace `parse_template_args` and `note/unnote_templated_types`.
4. A scoped type/value binding table instead of `temporary_type_names`.
5. `as` at unary precedence, and `@type(...)` parsing.
6. The C grammar and the registry remain authoritative for `.c` files and `#include` regions. The C path should emit the same neutral HIR (a typedef becomes a type-valued binding) so that header code also gets native_ops semantics.

## Adjacent decisions from the same discussion (not parsing; not final)

- Desugaring to native_ops happens during elaboration, not parsing. Two reasons: operators must be dispatched on the operand's kind (type value, generic function or runtime value), and diagnostics need the operator's source site.
  - `*`, `&`, `[]`, `==` and `sizeof` are neutral parse nodes.
  - The kind always follows from declared binders, even in generic bodies, so dispatch never waits for instantiation.
- Operators on comptime-only kinds (`@type`, static booleans) are elaborator built-ins. Operators on runtime values desugar to native_ops.
- Sugar is disabled inside the prelude itself (otherwise it would be circular). There, runtime arithmetic calls intrinsics explicitly.
- `&&`, `||` and `?:` need `expr` (lazy) parameters. `=`, `+=`, `++` and `&x` need place (reference) parameters. So native_ops entries are comptime macros, not runtime functions.
- The `add_result` sketch does not implement C's usual arithmetic conversions:
  - `i8 + i8` must give int.
  - `u32 + i32` must give u32; the sketch gives i32 when sizes are equal.
  
  Encode integer ranks. native_ops chooses signed or unsigned intrinsics, because MIR stays signless.
- Cache native_ops expansions keyed by operator and static arguments. This requires the prelude's comptime functions to be pure.
- An unbounded native_ops operator is a deferred body. Checked generics need bounds (for example traits whose primitive implementations come from native_ops). See the audits' checked-versus-deferred distinction.
- Split intrinsic.rs into two groups:
  - **Exposed intrinsics (the trusted core, with surface signatures and side conditions):** Int, Float, Ptr, Aggregate, VA, Assert/Assume, Bitcast.
  - **Compiler-internal, emitted by elaboration only:** AdoptPlace, PlaceAddress, GlobalAddress, ReferenceAddress, ArrayAddress, StringAddress, GetFnPtr.

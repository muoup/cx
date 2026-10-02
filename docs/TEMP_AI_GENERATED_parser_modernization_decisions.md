# Parser modernization: syntax decisions (2026-10-02)

Sections 1 to 6 and 8 are implemented (see "Implementation status" at the end); `@reify`
(section 7) is implemented for explicit signatures only. Builds on
`TEMP_AI_GENERATED_c_compatible_type_parsing.md` (computed-head declarations, `@type`
binders, `_` holes) and `TEMP_AI_GENERATED_staged_pipeline_design.md`.

Status of each item is one of **decided**, **tentative** or **open**.

## 1. Comptime parameters and generators (decided)

Template parameters become comptime parameters; type generators become comptime functions.
Both body forms parse; `=>` is preferred.

```c
// before
enum union opt<T> { none :: void, some :: T };
opt<U> opt::map<T, U>(opt<T> this, U (*func)(T));

// after
comptime @type opt(@type T) => enum union : @copy_traits(T) { none :: void, some :: T };
opt(U) opt::map(@type T, @type U, opt(T) this, @fn func(T) -> U);
```

## 2. `_` and `auto` (decided, open to tweaks)

The two are separated by position.

| Spelling | Meaning | Where it appears |
|---|---|---|
| `_` | a comptime argument the compiler fills in | argument / expression positions |
| `auto` | the whole type of a name being declared | declaration specifier, always followed by a declarator |

- `opt(_)` is always the curried generator. When a curried generator lands where a type is
  demanded (a scope-res lhs, or `opt(_) x = f();`), the missing arguments come from
  unification with the expected type.
- `_::member` is the limit case: the lhs is the expected type.
- `auto::none()` and bare `_ x` are rejected.
- The pattern wildcard `_` is the one separate use.

## 3. Patterns (decided)

A pattern is the expression that would build the value, with `auto x` where a value is
captured and `_` where it is ignored.

```c
// before
match (o) {
    opt::some<T>(value) => use(value);
    opt::none<T>()      => ...;
}

// after
match (o) {
    some(auto value) => use(value);
    none()           => ...;
}
if (this is some(auto& v)) { ... }
match (c) { RED => ...; auto other => ...; }
```

- A bare identifier is a reference that must resolve; a typo is an error, never a catch-all.
- Payload-less variants keep their parens, mirroring construction.
- The head resolves in the scrutinee type's namespace first, so no qualifier is needed.
- `auto x` owns, `auto& x` / `const auto& x` borrows. Today a binding aliases an lvalue
  subject and owns an rvalue one; that implicit rule goes away.

Restrictions (confirmed):

- An owning binding on a subject that stays in use copies the matched value. If the value
  cannot be copied it is an error: borrow with `auto&` or match on a moved subject.
- Only `auto`, `auto&` and `const auto&` bindings; typed bindings (`some(int x)`) are
  ambiguous with computed heads (`ok(some(int) x)`).
- Value patterns are comptime constants; `some(value)` with a runtime local gets a "bind the
  value with `auto name`" diagnostic, which also covers migration.

## 4. Scope resolution on types (decided)

A type points to a (possibly empty) namespace. `std::opt(int)::some` reads as
`(std::opt(int))::some`.

```
before: opt::some<int>(5)       Identifier { opt::some, template_input }
after:  std::opt(int)::some(5)  Call( ScopeAccess( Call(std::opt, int), some ), 5 )
```

- Internal constructors are the canonical form; std does not add `opt::some` / `opt::none`
  forwarding functions.
- A path prefix that resolves to a type (`T::none()`, a typedef, a non-generic
  `enum union shape`) looks in the type's constructors, then in the namespace of the same
  name. Identifier chains therefore cannot be resolved purely lexically.
- When the lhs has holes (`opt(_)::x`, `_::x`) the member must be a constructor and the lhs
  is taken from the expected type.
- Deduction runs from the expected type only. A generator is an arbitrary comptime function
  and cannot be inverted, so `auto x = opt(_)::some(5);` is an error and is written
  `opt(int)::some(5)`. Accepted.
- Alias generators (`comptime @type maybe(@type T) => opt(T);`) do not unify, since the
  type's recorded origin is `opt`.
- No fallback from `opt(int)::is_some` to the `opt::` associated-function namespace.

Staged expressions are typed where they are spliced, so `opt::try` deduces against the
caller:

```c
comptime expr(T) opt::try(@type T, expr(opt(T)) self) =>
    emit match (self) {
        some(auto value) => yield move value;
        none()           => return _::none();
    };
```

## 5. Function types (decided, keyword spelling `@fn`)

`fn T(args)` is ruled out: with type application, `fn U(T) func` parses as `U` applied to
`T`. `fn` is also a common C identifier, hence `@fn`.

```c
// named form: C's inside-name declarator
@fn cb(int) -> int = add_one;
@fn table[4](int) -> int;                 // C: int (*table[4])(int)
@fn *pp(int) -> int;
struct vtable { @fn drop(void*) -> void; };

// anonymous form: type arguments, casts, return types, unnamed parameters
vector(@fn(int) -> int) handlers;
@fn(int) -> void get_handler(int which);
```

- Same type as C's `R (*)(A)`, which stays valid.
- `@fn(int) -> int*` returns `int*`.
- `@fn foo(int) -> int;` at file scope is a function-pointer variable, not a prototype;
  `@fn foo(int x) -> int { ... }` is an error. Both want a specific diagnostic.
- Dependent signatures (`@fn(@type T, expr(T)) -> expr(T)`) are deferred; a curried
  `tap(int)` is already monomorphic.

## 6. Staged expressions and closures (decided)

`expr(T)` is the only staged type former. The parameterized form is a comptime function
value; there is no `expr(args) -> T` sugar and `expr[T](args)` is dropped.

```c
// before
comptime expr T tap<T>(expr T value, expr(T&) void func);
with_trace(source) <| |value| .{ value += 1; };

// after
comptime expr(T) tap(@type T, expr(T) value, @fn func(expr(T&)) -> expr(void));
with_trace(source) <| |value| emit .{ value += 1; };
```

A closure is an anonymous comptime function and nothing else.

```c
|value| emit .{ value += 1; }      // comptime body producing code
|value| then                       // sugar: emit <rest of the enclosing block>
|n| .{ comptime int k = n * 2; yield emit .{ use(k); }; }   // comptime scratch space
```

- `return` returns in whatever context it executes in. Inside `emit` it is part of the code
  value and runs where that code is spliced; outside `emit` it is a comptime return from the
  closure. This is the rule named comptime functions already follow.
- A plain `emit ...` value is the zero-argument case, so `||` is never written and prefix
  `|` cannot be confused with binary or.
- Parameters are untyped by default and checked against the expected
  `@fn(expr(A)...) -> expr(B)`; typed parameters use declaration form
  (`|expr(int&) v, int n|`).
- Runtime locals in the comptime part are an error; inside `emit` they are free variables
  of the code value.
- Arguments to `expr(T)` parameters are **no longer auto-quoted**; the caller writes `emit`.
  ```c
  x |> opt::unwrap_or_else(return 5);              // before
  x |>(1) opt::unwrap_or_else(_, emit return 5);   // after
  ```
  One exception as implemented, **to be confirmed**: the subject of a pipe is quoted when it
  lands on an `expr` parameter, so `x |>(1) opt::try(_)` needs no `(emit x)`.
- Calls inside `emit` to a comptime function splice the result; arguments are passed as
  code or evaluated at comptime according to each parameter's type.

## 7. Runtime anonymous functions (decided, explicit signatures implemented)

A native operator, `@reify`, turns a comptime function on code into a real function.

```
@reify(@type F, f)
    f : @fn(expr(A)...) -> expr(B)   or a plain expr(B)
    => B anon(A a...) { return <splice f(a...)>; }     of type F = @fn(A...) -> B
```

```c
auto hello = @reify(_, emit printf("hi\n"));                           // @fn() -> int
auto cmp   = @reify(_, |expr(int) a, expr(int) b| emit a - b);         // @fn(int, int) -> int
qsort(xs, n, sizeof(int), @reify(_, |a, b| emit *(int*)a - *(int*)b)); // from qsort's parameter
auto h     = @reify(@fn() -> int, emit printf("hi\n"));                // explicit bound
```

- The signature is deduced from the expression when the parameter types are known (no
  parameters, or typed closure parameters) and the body does not itself need an expected
  type. Otherwise it comes from the expected type or the explicit argument.
- The explicit signature is a bound, with one allowance shared by every function pointer
  conversion: a function may stand in for one returning `void`, unless its result is an
  owned `@nodrop` value. Nodrop and nocopy act as pseudo-effects until there is an effect
  algebra.
- An emitted `return` lands in the anonymous function, so it returns from the lambda.
- Capturing is rejected by a general check: emitted code that uses a local of one function
  cannot be spliced into another. Comptime values are baked in and allowed.
- Parameters are real locals, so arguments evaluate once.
- Not covered: recursion (the function has no name). The generated function should be
  memoized per site and comptime arguments.

## 8. Pipes (decided)

`x |> f(y)` targets argument index 0. `|>(n)` with an integer literal picks the index,
counting comptime parameters: `x |>(1) f(_, y)`.

## Not in this pass

`as`, `@type(e)`, native_ops, methods, user-attached type namespaces, associated constants
and types, dependent function types.

## Parser and HIR work implied

- HIR: comptime flag on parameters, `@type`, type-name call arguments; remove
  `HIRTemplatePrototype` / `HIRTemplateInput`; `ScopeAccess` expression; `HIRPattern` gains
  binding mode and loses `template_input`; closure node carries optional parameter types;
  `@fn` type node.
- Parser: computed-head declarations, trailing `*` / `&` suffix rule, `@type` binder scope
  table, `=>` and block generator bodies, `expr(T)`, `@fn` declarator and anonymous forms,
  postfix `::`, `auto` bindings, `|>(n)`; delete `templates.rs` and the `f<` lookahead.
- Lowering: origin (generator + arguments) recorded on generated types for unification;
  expected-type resolution of holes at splice; escaping-local check; `@reify`.
- Migration: `lib/std`, about 60 fixtures, examples. Every closure call site gains `emit`,
  every `expr` argument gains `emit`, every variant pattern changes.

## Implementation status (2026-10-02)

Implemented: comptime parameters and generators with both body forms, `_` holes, `auto`
bindings and binding modes, name-resolved variant patterns, value patterns, scope resolution
on types including constructors as values (`opt(int)::some`), `@fn` in declarator and
anonymous form, `expr(T)`, explicit-`emit` closures with optional typed parameters,
`|>(n)`, the void-result function pointer rule, `@reify` with an explicit `@fn(...) -> T`
signature. `lib/std`, the fixtures and the examples are migrated.

`@reify` is lowered in HIR lowering to a static function whose body returns the splice of
the value applied to its parameters. It is lowered outside the enclosing function, so a
captured local is reported as an unresolved symbol.

One rule was added that section 6 does not state and still needs a verdict: a piped subject
that lands on an `expr` parameter is quoted implicitly (`x |>(1) opt::try(_)`). Every other
`expr` argument needs `emit`.

Not implemented:

- `@reify` signature deduction (`_`, or from the expected type), and `@reify` inside a
  generic function when the signature mentions its comptime parameters.
- `const` is not enforced on `const auto&` bindings.
- A closure body must be `emit ...` or `then`; the comptime-scratch form is rejected.
- The escaping-local check for spliced code.
- A payload-free variant of a generated type named as a value (`opt(int)::none` without a
  call); the call form works.
- Type-directed lookup of `T::member` beyond constructors.
- Diagnostics from HIR lowering are mostly the generic "erroneous expression".
- `site/docs`, `site/stdlib/*.json`, the README and the vendored copy under
  `compiler/cx-zed-extension/grammars/cx` still show the old syntax.

Suite: 313 of 367 pass. The 54 failures are `type-errors` fixtures whose checks have not
been ported to HMIR lowering; the same 54 fail on HEAD once its `optional.cx` sketch is
removed.

Found along the way, present before this pass:

- `std::printf` is unresolved when `std::io` and `std::string` or `std::span` are both
  imported `as std`: the lookup is ambiguous and is treated as not found.
- Several `stdlib.h` functions are unresolved (`qsort`, `bsearch`, `getenv`, `labs`,
  `atexit`, `mblen`).
- `examples/lisp-interpreter` does not build (the ambiguity above, and two pattern subjects
  that are pointers, `interpreter.cx` lines 70 and 91) and segfaults when patched.

# Parsing

Token stream to HIR, then HIR unit to HMIR unit. The parser consults the [preparse](preparsing.md) registry to tell type names from values; desugaring plans one def per symbol and lowers every declaration into it. No name is resolved here except locals and function-level statics.

## Terms

*verified uncommitted*

- **mode**: file extension decides. `.c` is *C mode*; `.cxc` defaults to unmangled naming; anything else is CX with namespaced naming.
- **include region**: tokens between include markers the lexer inserts for `#include`d text.
- **ordered**: a statement made in C mode or an include region; its def declaration carries its point, so it is seen only after it.
- **comma mode**: whether `,` is an operator at the current nesting; a stack, outermost true.
- **type start**: tokens at which a declaration or type is parsed instead of a value; see [Type start](#type-start).
- **generator**: registry symbol of generator kind (a comptime function returning a type); names a type only once applied, `opt(int)`.
- **temporary type name**: name bound by a `type` parameter, treated as a type for the declaration that binds it.
- **class**: ordinary, or tag (`struct`, `union`, `enum` names).
- **stage** (desugaring): runtime or comptime body being lowered.

## Unit

*verified 425c5ff1*

Global statements until tokens end. Yields the HIR: definition statements in order, each with its module namespace and ordered flag, plus imports and namespace aliases from preparse.

State carried between statements: visibility (starts *package*), symbol naming, include stack, temporary type names, C function linkages.

```ebnf
global   = include-begin | include-end | import-stmt | typedef | ";"
         | ["public" | "private"] "extern" "\"C\"" ":"
         | ("public" | "private") [":"]
         | ["comptime"] declaration ;
```

- `import ...;` skipped to statement end; imports come from preparse.
- `rule/include-naming` entering an include region saves visibility and naming and switches naming to unmangled; leaving restores both. Unmatched end: P0003.
- `rule/extern-c-section` `extern "C":` sets naming unmangled and visibility (private unless `public` precedes) until the next access section. Other ABI string: P0011.
- `rule/access-section` `public`/`private` set visibility; outside an include region they also reset naming to the file default. Colon optional.
- `rule/quirk-access-section-specifier` any other specifier at that point: P0001.

### Typedef

*verified uncommitted*

`typedef T name;` adds an untagged type definition of `name` as T at current visibility. No name: P0001.
- `typedef R name(params)` makes T a function type (not a pointer); `noreturn` attributes make its return `unreachable`.
- `rule/quirk-typedef-alias-branch` `typedef S S;` with S standard or already typedef'd takes a separate branch that produces the same statement as the general one.

### Global declaration

*verified uncommitted*

Names bound by `type x` inside parentheses ahead of the declaration (up to `;`, `{`, `=>`, top-level `=`) are temporary type names for its duration, so a return type may use them before the parameter list.

```ebnf
declaration = specifiers type-base declarator
              ( function-tail
              | "=" expr ";"
              | ";"
              | "," declarator ["=" expr] { "," declarator ["=" expr] } ";" ) ;
```

- type with no declarator name: type-only statement, `;` required; only its side registrations remain (struct, enum definitions).
- naming: C mode: namespaced if `static`, else unmangled. Otherwise current section naming.
- `rule/inherited-extern` outside C mode, in a namespaced file inside an unmangled section, with standard linkage: a body-less function becomes `extern`; so do the globals of a declaration whose first declarator has no `=`, later declarators included even when initialized.
- `rule/quirk-global-initializer-comma` the initializer after the first declarator's `=` is parsed in outer comma mode, so `int a = 1, b;` is one global initialized with `1, b`. Later declarators' initializers likewise.
- unexpected end: P0003; other token after the declarator: P0001.

### Function declaration

*verified 425c5ff1*

```ebnf
function-tail = [ "::" name ] "(" params ")" contract ["->" abstract-type] ( ";" | body ) ;
params   = [ param { "," param } ] [ "," "..." ] | "..." ;
param    = ["comptime"] specifiers type-base declarator [ "=" expr ] ;
body     = "{" { statement } "}" | "=>" expr ";" ;
contract = suffixes ["safe"] [ "where" condition { "," condition } suffixes ] ;
condition = "precondition" ":" "(" expr ")"
          | "postcondition" ["(" name ")"] ":" "(" expr ")" ;
```

- a declarator followed by `(` is a function; else backtracks to a variable.
- `A::f` declares an associated function of A; more segments: P0021.
- trailing `-> T` only when the written return type is `auto` and not C mode; it keeps auto's qualifiers.
- `noreturn` attributes, before the type or after the parameters, make the return type `unreachable`.
- second precondition or postcondition: P0008.
- `rule/quirk-c-suffix-skip` identifiers `__asm__`, `__asm`, `asm`, `__declspec`, `__nonnull`, `__nonnull__`, `__wur` and an optional parenthesized group after each are skipped, with attributes, before and after `where`.
- `rule/c-static-linkage` C mode: a function first declared `static` at file scope stays static at later non-static declarations; static functions are namespaced, others unmangled. Block-scope declarations follow but do not record.

### Types

*verified uncommitted*

#### Type start

Yields whether a type begins at the current token. True for: builtin type keyword, any specifier, `struct`, `union`, `enum`, `register`, `type`, `fn`, and `^` outside C mode. A qualified identifier: if a generator, only when `(` follows the name; else when a temporary type name or a registry symbol (in C mode, tag symbols excluded).
- `rule/unimported-generator` outside C mode, a name no registry symbol or temporary type declares is a type start when parenthesized arguments follow it and then an identifier, since no value applies a name and then names another: `std::span(int) s` without the import. Its lookup then fails where the type is resolved.
- `rule/quirk-value-use-lookahead` an identifier type (builtin type names included) is not a type start when the token right after its *first segment* is `=`, `.` or `->`.
- registry ambiguity among equally ranked candidates of differing kinds: P0007. Rank: lexical namespace first, then type kind.

#### Type base

```ebnf
specifiers = { "const" | "volatile" | "restrict" | "extern" | "static"
             | "inline" | "thread_local" | attribute } ;
type-base  = ( qualified-name [ "(" args ")" ] | "auto" | "type" | "fn"
             | "^" abstract-type | builtin-words
             | struct | union | enum ) { qualifier } ;
```

- `inline`, `thread_local` ignored. `public`/`private` stop the list.
- args only for a generator, or a name taken by `rule/unimported-generator`. `auto` and `^` only outside C mode; in C mode `type` and `fn` are identifiers.
- `rule/staged-binds-type` `^` takes a whole abstract type: `^const T&` stages `const T&`.
- builtin words (`unsigned long int`) sorted: sign and `_Complex`, then `short`/`long`, then the rest; joined with spaces.

#### Declarator

```ebnf
declarator = { "*" {qualifier} | "weak" "*" {qualifier} | "&" | "'" lifetime "&" }
             ( name | "(" name ")" | "(" "*" [name] { array } ")" "(" params ")" | ε )
             { array } [ ":" abstract-type ] ;
array      = "[" [expr] "]" ;
```

- suffixes nest C-style: `int a[2][3]` is array 2 of array 3.
- `(*f[4])(int)` is array 4 of pointer to function.
- `rule/quirk-weak-pointer` `weak *` is a plain pointer.
- `: T` after a name replaces an `auto` type (not C mode).
- `fn {"*" {qualifier} | "&"} [name] {array} (params) -> R`: function type R, wrapped by each `*` (pointer) and `&` (reference) innermost first; `fn* f(int) -> int` is a pointer. Without a name or array: the function type itself, whose return type has already taken any `*`, `&`, `[]`; a name then directly followed by `(` is a function declarator returning it (`fn*(int) -> int pick(int which)`), any other name: P0022. No `(`: P0001.
- `fn` function types are marked not decaying; C declarators (`R (*f)(params)`, `typedef R f(params)`) make decaying ones. See [Type lowering](#type-lowering).
- abstract type: specifiers, base, pointer and reference suffixes, arrays; no name. A `fn` base reads its indirections and signature.

#### Aggregates

```ebnf
struct = "struct" [qualified-name] [":" attrs] [ "{" { fields ";" } "}" ] ;
union  = "union" [qualified-name] [ "{" { fields ";" } "}" ] ;
tagged = "enum" "union" [name] [":" attrs] "{" variant { "," variant } [","] "}" ;
variant = name "::" abstract-type ;
attrs  = "@nocopy" | "@nodrop" | "@unsafe_move" | "@copy_traits" "(" name ")" { "," ... } ;
fields = specifiers type-base declarator [":" int-literal] { "," declarator [":" int-literal] } ;
```

- named definition: adds a tagged type definition and yields a tag reference to it; anonymous: the type in place.
- `rule/quirk-hoisted-aggregate` definitions are added as global statements wherever written: a struct, union or enum defined in a function body, nested block included, is a global type seen from the whole unit.
- `rule/quirk-union-attributes` plain unions take no attributes.
- unknown attribute: P0006. Qualified definition name: P0001.
- bitfield width must be an integer literal token.
- `rule/anonymous-member` unnamed struct or union member named `__anonymous_member_<token index>`; other unnamed member: P0011.
- tagged union variant with a name: P0001.
- `rule/predeclaration-registers` `struct S` / `union S` without body, root name, in C mode or an include region or followed by `;`, and no tagged definition of S with that tag yet: adds a forward tag definition. Enum predeclarations never do.

#### Enum

`enum [name] { a [= e], ... }` adds an enum definition statement (its variants) and, if named, a tagged definition of the name as `int`. Yields `int` or the tag reference.

### Statements

*verified 425c5ff1*

```ebnf
statement = ";" | block | name ":" labelled | "case" expr ":" labelled
          | "default" ":" labelled | "if" cond statement ["else" statement]
          | "switch" cond statement | "match" ... | "while" cond statement
          | "do" statement "while" cond ";" | "for" "(" [decl-or-expr] ";" [expr] ";" [expr] ")" statement
          | "goto" name ";" | "goto" "*" expr ";" | "defer" expr ";"
          | "break" ";" | "continue" ";" | decl-or-expr ";" ;
labelled  = "}"-lookahead | statement ;
```

- `;` not required after if, switch, match, for, while, labels, cases, `{}` blocks, nor after a statement ending in `then`.
- a label or case directly before `}` labels an empty statement.
- omitted `for` condition is `1`; omitted init and increment are void.
- `rule/scoped-type-expression` a statement opening with a generator applied and followed by `::` (`opt(int)::some(5);`) is an expression, not a declaration.
- `rule/quirk-register-skipped` `register` ignored.

#### Declaration statement

Specifiers, base, then declarators separated by `,` (comma mode off). Several yield a sequence block. Type-only: void.
- `rule/block-function-declaration` in C mode or with `extern`, a function declarator adds a body-less function definition statement (C mode: standard linkage becomes extern) and yields void.
- another declarator without a name: P0001.

### Expressions

*verified uncommitted*

Shunting-yard over values and operators; an operator pops those of precedence at most its own (assignment: strictly less, right-associative). Leftover stack state: PX001.

Precedence, tightest first:
- 1: `.` `->` call `[]`, postfix `++` `--`, postfix `!` (outside C mode), prefix `move`
- 2: `|>` `<|`; prefix `++` `--` `~` `!` `-` `*` `&`
- 3: `is`, cast
- 4: `*` `/` `%`
- 5: `+` `-`
- 6: `<<` `>>`
- 7: `<` `>` `<=` `>=`
- 8: `&`
- 9: `^`
- 10: `|` `==` `!=`
- 14: `&&`
- 15: `||`
- 17: `=` and compound assignment
- 18: `,` (comma mode on only)

- `rule/quirk-equality-precedence` `==`/`!=` share precedence with `|`, below `&` and `^`, unlike C.
- `rule/quirk-move-precedence` prefix `move` pops before `.`, `[]` and calls: `move a.b` is `(move a).b`, `move f(x)` calls `move f`.
- `rule/quirk-pipe-precedence` pipes bind tighter than arithmetic: `a + b |> f()` pipes b.
- `rule/quirk-spaced-shift` two adjacent `>` or `<` tokens are a shift even with space between.
- `->` is the same operator as `.`.
- no unary `+`.
- `|>(n)` pipes into argument n; plain `|>` into 0.

#### Values

```ebnf
value = {prefix-op} ( type | literal | "true" | "false" | "&&" name | "." block
      | closure | builtin-words | "@" intrinsic | qualified-name | "_" ["::" qualified-name]
      | keyword-expr | "(" ")" | "(" expr ")" | initializer | "[" "]" | "[" expr "]" )
      { "::" name } {postfix-op} ;
closure = "|" [ cparam { "," cparam } ] "|" ( "then" | "=>" expr | block ) ;
cparam  = specifiers type-base declarator ;
initializer = "{" [ ["." name "="] expr { "," ["." name "="] expr } [","] ] "}" ;
```

- `rule/type-in-value` outside C mode and include regions, a type start parses a type as a value; a declarator name there: P0009.
- `rule/quirk-prefix-cast` `(T)` after a type start is a cast unless followed by `;` `)` `]` `}` `,` `::`; then it backtracks to a parenthesized value.
- `&&name` in value position is a label address.
- `.{ ... }` is an expression block (yields); `.` followed by anything else: P0001.
- `{ ... }` in value position is an initializer list; elements in comma mode off.
- `( e )` turns a top-level comma into a *grouped* comma, which a call does not split.
- `()` and `[]` are void; `rule/quirk-bracket-value` `[e]` is just e.
- `A::b` where A is a temporary type name: scope access on type A. `_::a::b`: scope access chain on a hole.
- `rule/unresolved-type` identifier directly followed by an identifier: P0018.
- `rule/typed-closure-param` closure parameter without a type (`|x|`): P0001; `auto x` leaves it to be inferred.
- closure body: `=> e` in comma mode off; a `{ ... }` body is an expression block (yields); anything else: P0001.
- `comptime` in value position: P0012. Other keyword: P0002.
- `rule/quirk-va-arg` `va_arg(list, T)` and `__builtin_va_arg` parse their second argument as a type.

#### Keyword expressions

- `sizeof(T)`, `alignof(T)` when a type start follows, else of an expression. Named type: P0001.
- `offsetof(T, a.b[i])`.
- `return [e]`, `yield [e]`: value omitted only before `;`.
- `emit e`: e in comma mode off.
- `match (e) { pattern => arm, ... }`: arms in comma mode off; `,` optional after a braced arm or before `}`; arm is a block, `break`, `continue`, or an expression. `default`: P0020.

#### Calls

Arguments in comma mode off, each optionally `.name = e`; folded into a comma chain under a call operator. After the call, `::member` suffixes and postfix operators continue.

#### Ternary

After the operator loop, `?` takes as condition everything above assignment precedence; then branch and else branch are full expressions.
- `rule/quirk-greedy-ternary-else` the else branch takes assignments and, in comma mode, commas: `c ? a : b = 1` assigns in the else branch.

#### Patterns

```ebnf
pattern = int-literal | float-literal | "auto" ["&"] name | "const" "auto" "&" name
        | "_" | "_" "::" name variant-tail | qualified-name
        | generator "(" args ")" "::" name variant-tail | qualified-name variant-tail ;
variant-tail = "(" [pattern] ")" ;
```

- bare qualified name without `(`: a value pattern, never a binding.
- `A::b(p)`: variant b of type A (prefix without its last segment); `b(p)` unqualified: variant of the subject's type.
- `const auto x` without `&`: P0001.

#### Intrinsics

`@unsafe {..}` or `@unsafe(e)`; `@leak(e)`, `@copy(e)`, `@adopt(e)`; `@reify(signature, value)`, exactly two arguments else P0001; `@unpack(e) { field: binding, ... }`. Other name: P0006.

## Desugaring

*verified 425c5ff1*

HIR unit to HMIR unit. Plans symbols, lowers each planned def, then appends function-level statics and merges block-scope externs. Yields the unit with its symbol table.

### Planning

*verified 425c5ff1*

One def per (qualified name, class), in first-appearance order; each statement adds a declaration to it. Yields planned defs.
- namespace: root for a root-namespace statement, else the unit namespace. Functions: joined with the associated namespace, `ns::A::f`.
- point: (unit, statement ordinal). Ordered flag from the statement.
- `rule/forward-tag` type definition whose type is a tag reference to its own name with the same tag: declaration with no content. Other type definitions: definition.
- functions: definition with body, else declaration.
- globals: initializer makes a definition; extern without: declaration; else tentative.
- enum definition: one ordinary definition per variant. Anonymous type definitions dropped.
- tagged def names read `struct S`, `union S`, `enum S`.

Symbol table: imports (each imported name's namespace plus name, deduplicated), namespace aliases, planned (name, class) to def. Statics are not in it.

### Link names

*verified 425c5ff1*

Unmangled naming, root namespace, or name `main`: the plain name. Else `_N` followed by length-prefixed parts `<len>_<text>`: the segment count, each segment, the name.

### Defs

*verified uncommitted*

- forward tag: comptime global of type `type`, initialized to the undefined tagged type.
- type: comptime global of type `type`, initialized to the lowered type.
- `rule/comptime-global-initializer` comptime global without initializer: T0052.
- global: type, initial value (see [Declarations](#declarations)), linkage, link name.
- enum variant: comptime global `i32`. Explicit value: its expression. Else with no explicit value before: its index. Else nearest earlier explicit variant `v` plus the distance, `v + (i - j)`.

#### Function

- `rule/void-parameter-list` a lone unnamed `void` parameter declares none.
- parameters and return type lowered in the function's stage; each parameter a local.
- `rule/hole-default-only` a default on a runtime parameter of a runtime function: T0017; a default other than `_`: T0042.
- `rule/detached-contract` conditions lowered in a body of their own with a hole-typed local per parameter, typed from the signature later; postcondition binding is a hole-typed local in a nested scope. No conditions: empty contract keeping `safe`.
- body: braced, a scoped non-yielding block; `=> e`, e itself.

### Body scopes

*verified uncommitted*

Scopes stack inside a body: plain, arguments, staged, block (remaining statements), switch.
- `rule/local-lookup` untagged name whose namespace is root: innermost local or static binding; else left as a name for evaluation.
- `rule/then` `then` lowers the rest of the innermost enclosing block (looking through staged and argument scopes) into an unscoped block, consuming those statements. No block: T0017.
- `rule/case-in-switch` case outside a switch (staged code ends the search): T0018; second `default`: T0007.
- `rule/label-addresses` `&&name` records the label on the owning def.

#### Statics

Runtime-stage `static` or `extern` local declarations; yield unit.
- static: new def after the planned ones, private definition at the body's visible point, link name `name.<def index>`. Bound in the current scope.
- extern: declaration added to the ordinary def of the same name in the unit (planned or new), unmangled, definition only if initialized.
- label addresses taken in the initializer move to the enclosing function.
- `rule/quirk-comptime-static` in a comptime body, `static`/`extern` locals are ordinary locals.

#### Reify

`@reify(function type, value)`: new static runtime function `reify.<def index>` at file scope; parameters from the signature; body returns an escaping splice of the comptime value with each parameter quoted (`emit p`), so the code may `return` from the reified function. Signature not a function type (`fn(params) -> R`, no indirection): T0011.

### Staged type

*verified uncommitted*

Staged signature: `^T`, or a function type (either form) returning `^T`. Parameters of the latter as declared; none is implicitly staged.
- `^T` lowers to staged T; the function form to a non-decaying comptime function type over its lowered parameters returning staged T. `fn f(int) -> ^T` keeps an `int` parameter, which is then no [staged shape](hmir-lowering.md#quote).

### Type lowering

*verified uncommitted*

- `rule/qualifier-wrapper` `const`/`volatile` wrap in a nominal with those semantics. `restrict` dropped.
- `rule/quirk-auto-drops-qualifiers` `auto` lowers to a hole; qualifiers on it dropped.
- identifier: local, static, or name with its tag; with args a call.
- `type`: type of types. `T[n]`: array; `T[]`: array with unit length.
- `T&` and `'a T&`: reference; lifetime dropped.
- function type: signature in the written stage, decaying when written as a C declarator; defaults: T0017.
- struct, union, tagged union: fields in order, anonymous members unnamed; move semantics `@copy_traits(P)` (P looked up as a name) else explicit POD, nocopy, nodrop; `@unsafe_move`. `inv/aggregate-per-lowering` each lowering is a distinct type.

### Expression lowering

*verified uncommitted*

#### Literals

- `rule/int-literal-type` first fitting of `i32`, `u32`, `i64`, `u64`, starting at `i64` with any length suffix; signed only without `u`, unsigned only with `u` or non-decimal. None: T0021.
- float: `f64`, `f` suffix `f32`; long double: T0068.
- `true`/`false`: `i1` unsigned. Strings as written.

#### Operators

- arithmetic, comparison, logical, bitwise: binary op. Compound assignment: assign with that op.
- `,` and grouped `,`: unscoped non-yielding block of both.
- `.`/`->`: member of a name; right side not a name: T0011.
- `[]`: index. `*`, `&`: dereference, address-of. `-`, `~`, prefix `!`: unary. `move`: move. `++`/`--` pre and post.
- postfix `!`: escape control op over the operand.
- cast: C cast coerce. `is`: is with lowered pattern.
- named argument outside a call: T0017. Taken expression: MX017.

#### Control

- `rule/if-scopes` condition and then branch in one scope, each branch in its own; statement-if branches not blocks are wrapped in scoped blocks; ternary branches are not.
- `rule/truthy-condition` if, while, for conditions coerce truthy to `bool`; void condition is `1`.
- while and for: one scope over the loop, nested for the body.
- match: scrutinee, then an anonymous hole-typed subject local; each arm scoped with its pattern.
- switch body in a switch scope. `defer`, `@unsafe`, escape, `return`, `yield`, `break`, `continue`, `goto`, `goto *`, labels: control ops.

#### Declarations

- `rule/let` local declared in the scope; initializer after, so it sees itself.
- `rule/shared-initializer-type` an initializer list as an initial value takes the declared type expression; standalone and nested lists take holes.
- `@unpack(e) {f: x}`: hole-typed locals bound to fields.

#### Calls

Call, pipe and backward pipe lower in an arguments scope.
- `rule/pipe-insert` `x |>(n) f(args)` inserts x at n, innermost pipe applied last; n beyond the argument count: T0039. `f(args) <| x` appends x. Target not a call: T0002.
- `rule/pipe-escape` a target under postfix `!` is lowered as the target with the piped values, then wrapped in escape: `x |> f()!` is `(f(x))!`.
- arguments supplied by `|>` and `<|` marked piped for evaluation, whatever the callee.
- `rule/quirk-leading-void-argument` a call whose first argument is `()` loses it.
- `rule/va-builtins` `va_start(l, last)`, `va_end(l)` (and `__builtin_` forms) with exactly those arities: intrinsics. Named arguments to them: T0017.
- a local is always a local reference; whether a staged local splices is decided in HMIR, see [Local](hmir-lowering.md#local).
- `rule/scope-base` `_::v` base is a hole; type base lowered as type.

#### Closures and quotes

- closure: a quote with its parameters over its body; `then` quotes the rest of the enclosing block.
- quote: parameter types lowered in the enclosing stage as written (`auto` a hole); body lowered in runtime stage in a staged scope with the parameters as locals.
- `emit e`: quote without parameters.

#### Patterns

Wildcard; integer; float; value; binding `auto x` hole-typed, `auto& x` reference to hole, `const auto& x` reference to const hole; variant with its sum type when qualified by a type.

# Temporary agent handoff: typeclasses as metatypes, static dispatch, borrowed erasure

AI-generated conversation state, written 2026-10-06. DESIGN, NOT IMPLEMENTED SPECIFICATION. Repository-relative paths for portability. Retire after incorporation into maintained docs; reading this file does not authorize deletion or implementation.

## Intent / scope

Read-only brainstorm on branch feat/2ltt. No compiler or library changes were made; this file is the only write. Working sketch is lib/wip/iter.cx (does not compile, predates most decisions below, still uses lifetime parameters on the class which this discussion did not revisit).

Goal: one syntax and one semantic reading for typeclasses covering static (monomorphized) dispatch and explicit type erasure, with no implicit allocation and minimal hidden behaviour. All spellings provisional unless marked CONFIRMED. Items marked PROPOSED were suggested by the agent and not explicitly accepted.

## Model (CONFIRMED)

- A typeclass is a metatype: a subset of @type. It classifies types the way a type classifies values. It is not itself a type. Sort keyword: @metatype.
- A dictionary literal constructs a type. `iter(T) { .next = f }` is a @type that is a member of `iter(T)`. Struct-style syntax, every field required, no defaults.
- That type is the subject type plus the dictionary carried in type metadata. The dictionary is comptime; it is never a runtime field of the value. The subject is read off the receiver of the entries.
- The member type is distinct from its subject type. Conversion is explicit: `subject as member_type`. Accepted for sketches, not final.
- Because the type carries its dictionary, evidence is determined by the type. No binding table, no instance search, no dictionary parameters.
- Static types have no vtable. A vtable exists only when a member type is erased.
- Typeclasses stay struct-like: consistent type replacement plus non-null function entries.

## Guiding syntax

~~~cx
comptime @metatype iter(@type T) =>
    typeclass(@type this) : @dyn {
        std::opt(T) next(this&);
    };

// Static dispatch only, so a consuming receiver is allowed.
comptime @metatype sink(@type T) =>
    typeclass(@type this) {
        void  push(this&, T);
        usize finish(this);
    };

std::opt(T) span::next(comptime @type T, std::span(T)& this) {
    if (this.length == 0) return _::none();
    this.length--;
    return _::some(*this.data++);
}

// The member type is named through a generator so signatures can refer to it.
comptime @type span::span_iter_t(@type T)
    => iter(T) { .next = span::next(T) };

span::span_iter_t(T) span::iter(comptime @type T = _, std::span(T)& span)
    => span as span::span_iter_t(T);

// Runtime subject, comptime dictionary. Monomorphized per member type.
usize iter::count(comptime @type T = _, iter(T)& it) {
    usize n = 0;
    while (true) {
        match (it.next()) {
            some(auto value) => n++,
            none() => break
        }
    }
    return n;
}

// Comptime function: ^ marks staged parameters, unmarked ones are known values.
comptime ^void iter::for_each(@type T = _, ^iter(T)& it, @fn callback(^T) -> ^void)
    => emit {
        while (true) {
            match (it.next()) {
                some(auto value) => callback(value),
                none() => break
            }
        }
    };

usize drain(comptime @type T = _, iter(T)& it, sink(T) out) {
    while (true) {
        match (it.next()) {
            some(auto value) => out.push(value),
            none() => break
        }
    }
    return (move out).finish();
}

// Erased parameter: closed type, ordinary function, compiled once.
usize count_erased(std::dyn(iter(int)) it) => iter::count(it);

int main() {
    std::span(int) s = span_factory();

    auto a = std::span::iter(s);
    usize n = iter::count(a);                              // direct calls to span::next

    auto b = std::span::iter(s);
    std::dyn(iter(int)) d = std::dyn::of(iter(int), b);    // explicit, borrows b, no allocation
    usize m = count_erased(d);                             // indirect calls through the static vtable
}
~~~

- CONFIRMED: `typeclass(@type this)` header; the binder may itself be bounded (`typeclass(iter(T) this)`), which yields superclasses.
- CONFIRMED: return types name the member type explicitly through a generator. `auto` in return position is the only plausible shorthand discussed; not adopted.
- PROPOSED: `: @dyn` marker reusing the existing aggregate attribute position (`struct : @nodrop`, `@copy_traits(T)`); `std::dyn::of` as the erasure function name.
- PROPOSED: `as` also converts back to the subject and applies through references (`S&` to `I&`), since representation is identical.
- PROPOSED: type identity of a literal is the generator instantiation that produced it. A literal containing a closure must denote the same type for the same comptime arguments.

## Parameters and stages

| Context | Written | Meaning |
| --- | --- | --- |
| runtime fn | `iter(T) value` | runtime subject, comptime dictionary |
| runtime fn | `comptime iter(T) value` | comptime subject and dictionary, no runtime parameter |
| comptime fn | `iter(T) value` | known comptime value |
| comptime fn | `^iter(T) value` | staged expression |

- CONFIRMED: a parameter is comptime only if marked `comptime` or inside a comptime function. No deduction from the classifier.
- CONFIRMED: the current implicit rule is wrong and should go: compiler/cx-parsing/src/parse/functions.rs:235-238 treats a `@type` parameter as comptime without the keyword. Roughly 45 signature lines in 33 files under lib/ and tests/ rely on it (rough grep, may include continuation lines of comptime functions).
- CONFIRMED: a runtime variant of @type with basic reflection metadata is wanted eventually; unsupported for now. All current @type uses are comptime.
- A metatype where a type is expected means "some member type". `iter(T) value` therefore introduces a hidden comptime type parameter, monomorphized per type. This is the one implicit element left. The type is recovered with `@type(value)`.
- Consequence, unconfirmed: the earlier named-binder form `iter(T) I = _, I value` and the agent's `iter(T) auto value` marker are dropped, since `iter(T) x` now always declares a value. Unbounded `auto value` (Zig anytype) was not revisited.
- Holes in class arguments (`iter(_) value`, or `comptime @type T = _, iter(T) value`) are solved from the value type's metadata. No match is an error.

## Dispatch (CONFIRMED)

- `x.m(args)` applies entry `m` of the dictionary of `@type(x)` to `x` as first argument, under ordinary argument-passing rules. Nothing receiver-specific.
- Receivers are explicit in the class: `this&` or `this`. A by-value receiver needs `move`: `(move bar).by_value()`.
- Static member type: the dictionary is a comptime constant, the entry is a known function, the call is direct.
- Erased type: `@dyn_dispatch(obj, "next", args...)`, name as a comptime string.
- Entries are plain function values: a symbol, a curried generic (`span::next(T)`), or a closure checked against the entry's function type. The earlier "entry is a comptime function from code to code" shape is superseded.

## Member signatures (OPEN)

Agreed: invalid signatures must be rejected when the class is checked, not when codegen fails.

- User proposal: check each member with body-like semantics over a single `this` place. A by-value `this` emulates a move, so `proc(this, this&)` and `proc(this&, this)` are rejected, `proc(this&, this&)` accepted.
- Agent proposal (unconfirmed): `this` is the subject type; the first parameter is the receiver and must be `this&`, `const this&` or `this`; other parameters of that type are separate caller-supplied values. Aliasing and move checks then happen at the call site (`x.proc5(move x)` rejected by the ordinary move rule). Rationale: `this` must act as a type in `std::opt(this&)`, `std::span(this)` and return types; the single-place reading accepts only signatures that add no information and forbids binary members such as `bool eq(this&, this&)`.
- Either way, @dyn classes add the rule below.

## Erasure v0 (CONFIRMED scope)

Rust-style borrowed `&dyn` only.

- `std::dyn(C)` is a two-word value: pointer to a static vtable, reference to the subject. Copyable, bounded by the borrow.
- Erasure is explicit and never allocates.
- The vtable is the member type's dictionary placed as a constant aggregate in global static memory. One per erased member type, emitted only if that type is erased. Slots are the function symbols themselves.
- A @dyn class has only erasable members: reference receiver, no comptime parameters, `this` nowhere else in the signature. This keeps layout and calling convention independent of the subject, which is what makes the vtable pointer reinterpretable at an opaque subject.
- `std::dyn(C)` is itself a member of `C`, through a dictionary of small forwarding functions, so it can be passed to any `C`-bounded parameter (one instantiation, indirect calls).
- Explicit dyn / non-dyn distinction on the class, unlike Rust. Non-dyn means no dynamic dispatch, not that every member is non-erasable. Non-dyn classes may have consuming receivers.
- Preference: intrinsics are wrapped by std functions; idioms are built on std, not on `@` forms.
- PROPOSED intrinsics for v0: `@dyn_dispatch`; one to obtain the address of a member type's dictionary in static memory; one way for `std::dyn` to state forwarding for every member of an arbitrary class (an intrinsic, or comptime member reflection later).

## Deferred

- One value in several classes. Each literal carries one class's dictionary, so this means stacking; needs rules for metadata inheritance and for whether `debug{iter{S}}` equals `iter{debug{S}}`. Also conjunction of bounds.
- Types the author does not own (`int` as `hash`). Today every call site must convert: `put(5 as hash { .hash = int_hash })`. A later binding declaration (one canonical dictionary per class and subject, declared in the module of the class or the subject, no overlap) could add this without changing the model. Painful mainly for property-like classes; irrelevant for `iter`, where conversion is always explicit.
- Metatype in return position ("one type chosen by the body"), and metatypes in struct fields (not possible; needs a concrete name, `@type(expr)`, or erasure).
- Whether a bounded body is checked once against the bound or at each instantiation. User stated the body should have guarantees, which implies the former; consistent with the Rust-style generic checking goal in docs/TEMP_AI_GENERATED_staged_expression_design.md.
- Typed initializer `T { ... }` as a general form: struct gives an initializer list, metatype gives a member type, function type could give a statement body as an alternative to @reify. Only the metatype case was pinned down.
- Owned erasure, consuming receivers in @dyn classes, allocators, runtime-built vtables. Conclusions reached before deferral, to avoid re-deriving:
  - CX has no implicit destructors (`vector::drop_with` takes its destructor as an argument), so there is no drop slot. An owned erased value must be @nodrop and discharged through a consuming member, so a class meant for owned erasure must declare one.
  - A by-value slot takes a pointer and is always a thunk: `by_value(move @adopt(*(I*)p))`. Consuming has two halves: the slot consumes the subject, the container releases storage and `@leak`s itself.
  - `std::dyn` should borrow its vtable, never own it; owning forces one vtable per value or a refcount, and an allocator handle in the fat pointer. Owned vtables belong in a separate handle that `std::dyn` borrows from.
  - "Pseudo-allocation" for static vtables is not the general allocator interface (allocate-then-write would write constant memory). It is a narrower source with acquire/release; construction differs by stage, use is uniform.
  - A reference form and a linear owned form, never conditional borrowing. In-place adoption of a stack slot conflicts with the planned move rule in docs/ideas/reference_lifetimes.md ("Move ... invalidates dependencies on the old region") and needs its own invalidation reason; `&x` plus `@leak(x)` is not a substitute. `@adopt` already names the opposite direction.
  - Simplest owned form later: subject in allocator storage, allocator stateless and fixed by the type, mirroring std::box.
- Aside, unverified intent: `box::unwrap` (lib/std/box.cx:30) adopts the pointee and leaks the box without freeing `ptr`.

## Superseded

- In-place adherence as an attribute on the struct definition (`@impl(...)`).
- Dictionary as a value of a dictionary type `iter(T)(S)`; required a binding step to be usable by bounds.
- `comptime iter(T) iter` as a monomorphizable typeclass parameter generating implicit runtime parameters.
- `iter(T)::typed(S)` / `iter(T)::erased()` as surface types; `[type] is [bound]` declarations.
- Per-value dictionaries built in the factory, as in lib/wip/iter.cx (`subject |> iter(a, index)() { ... }`), and `^iter(a, index)` as a return type.
- Array-of-erased-pointers vtable layout; drop slot.

## Formal reading

Notation from Kovács, "Staged Compilation with Two-Level Type Theory" (2022) and "Closure-Free Functional Programming in a Two-Level Type Theory" (2024), recalled from memory, not re-checked. Using the 2024 style: `Ty : MetaTy`, lift `⇑ : Ty → MetaTy`.

- A class `B` is a subset of `Ty`. A member type is an element of `B`; a tagged union is an element of `Ty` and can never bound.
- `iter(T) value` in a runtime function is `(Σ(I : B). ⇑I) → …`, which in argument position curries to `{I : B} → ⇑I → …`. Each occurrence is a fresh type; legal only in comptime-resolved positions, never as runtime storage.
- Runtime has no type variables; monomorphization is staging. `^T` is ⇑, `emit` is quote, splice is implicit.
- Obligations outside the papers: comptime code can inspect types, so operations like sizeof must be stuck on an abstract subject (this is what the @dyn rule encodes); functions placed in a vtable must be closed; staged code is not linear, so a `^T` spliced twice or never duplicates or drops moves and effects; an erased value's lifetime is bounded by its subject.
- Related: Minamide, Morrisett, Harper, "Typed Closure Conversion" (1996) for existential packaging.

## Parser landmarks (read, not modified)

- compiler/cx-parsing/src/preparse.rs:83-102: generators are recognised only by the literal tokens `comptime @type name(`. Needs a `@metatype` equivalent. An associated name such as `comptime @type span::span_iter_t(` is not matched today, because `::` follows the first identifier.
- compiler/cx-parsing/src/parse.rs:248: type binders are found by scanning for `@type ident`; the scan stops at `=>`, so a `typeclass(@type this)` binder after the arrow must be noted by the typeclass parser.
- compiler/cx-parsing/src/parse/functions.rs:235-238: implicit comptime for `@type` parameters (to be removed).
- compiler/cx-parsing/src/parse/expressions.rs:492: a type in value position followed by `{` is currently a parse error; hook point for `T { ... }`. `:27-102` holds the `@` intrinsics (`@reify`, `@leak`, `@adopt`).
- compiler/cx-parsing/src/parse/types.rs:84-99: aggregate attributes after `:`; `:795` `auto` as a base type outside C mode.
- New syntax must use token sequences that are syntax errors in C; no methods are planned beyond member dispatch; `::` is association by naming convention.

## Prerequisite not verified

Static vtables need a comptime aggregate placed in static memory whose address survives into runtime code as a symbol reference. Whether the comptime engine can represent such a pointer today was not checked. The THIR had constant-aggregate support intended for GCC-style constant initializers.

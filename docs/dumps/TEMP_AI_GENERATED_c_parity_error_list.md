# C parity adversarial review: error list

Reviewed on 2026-10-08 against the uncommitted changes supporting the production C benchmarking suite.

Four P1 correctness failures should be addressed before committing. Five P2 findings identify additional correctness problems or gaps in the expanded support. Recommendations focus on shared semantic boundaries rather than fixes for individual fixtures.

## Validation and scope

- Reproductions used the existing debug compiler and source inspection. The compiler was not rebuilt for this review.
- GCC was used as a reference for the relevant C reproductions.
- String-byte corruption, recursive-type failure, union initialization, string-array typing, and anonymous designators were checked on both LLVM and Cranelift.
- The opaque-alignment failure was reproduced on LLVM at O0 and O2; the same reproduction passed on Cranelift.
- The union-initializer failure and recursive-type panic were also reproduced on LLVM at O2.
- No repository files were edited during the review. Probes were placed in `/tmp/cx-adversarial-review`; the important examples are preserved below because that directory is temporary.
- The full integration suites were not run. `git diff --cached --check` passed.
- Baseline behavior was not systematically compared against HEAD. Coverage gaps below are not claimed to be newly introduced regressions.

## R01 — P1: C string escapes corrupt bytes above 0x7f

- [ ] Preserve literal bytes and preprocessing spelling separately.

**Sources:** [lexer/token_rules.rs](../../compiler/cx-lexer/src/lexer/token_rules.rs), around line 189; [context.rs](../../compiler/cx-lexer/src/context.rs), `string_literal_text` around line 626.

The escape reader returns a byte, but `string.push(char::from(value))` stores it in a Rust `String`. Values above 0x7f are subsequently emitted as UTF-8. Thus `"\xff"` becomes bytes `c3 bf` rather than one byte `ff`.

Both programs compile but return 1 on both CX backends:

```c
int main(void) {
    return sizeof("\xff") != 2;
}
```

```c
int main(void) {
    const unsigned char *s = (const unsigned char *)"\xff";
    return s[0] != 255;
}
```

Lua's actual `LUAC_DATA` literal in `examples/c-parity/lua/upstream/lundump.h` is `"\x19\x93\r\n\x1a\n"`. A probe printing its size, byte 1, and byte 2 produced:

| Compiler | Size including terminator | Byte 1 | Byte 2 |
| --- | ---: | ---: | ---: |
| GCC | 7 | 147 | 13 |
| CX LLVM | 8 | 194 | 147 |

This likely explains the documented Lua binary-chunk header failure. The full upstream suite was not rerun to establish that it is the only cause.

Stringification exposes the related spelling problem:

```c
#define S(x) #x
int main(void) {
    const char *s = S("\xff");
    return s[1] != 92;
}
```

GCC returns 0; CX returns 1. Reconstructing a literal's spelling from its decoded value cannot preserve the original preprocessing token.

**Recommended fix:** Preserve C literal bytes and original preprocessing spelling separately. Repair the compiler representation rather than Lua's dump/load routines.

## R02 — P1: Deferred completion permanently caches an incomplete MIR type

- [ ] Preserve nominal identity through MIR reservation and completion.

**Sources:** [eval/types.rs](../../compiler/cx-hmir-lowering/src/eval/types.rs), array-length eager handling around line 95 and named-pointee deferral; [ty/lower.rs](../../compiler/cx-hmir-lowering/src/ty/lower.rs), `lower_nominal_type` around line 94.

This GCC-valid program panics on both CX backends:

```c
struct A {
    struct B *b;
    int bytes[sizeof(struct B *)];
};
struct B {
    struct A a;
};
int main(void) {
    struct B b;
    b.a.bytes[7] = 9;
    return b.a.bytes[7] != 9;
}
```

The internal compiler error is `aggregate field index out of bounds` at `compiler/cx-mir-lowering/src/lowering/values.rs:235`. The MIR dump declares `b` as `opaque[0 bytes]`, then projects aggregate fields from it.

The new deferral/eager path interacts with an existing lowering behavior: an incomplete nominal is interned as a zero-sized opaque MIR type and inserted into `types.lowered`. Completing the nominal later does not update that mapping.

**Recommended fix:** Reserve a stable MIR identity for a nominal and complete that identity when its definition becomes available. Pointer layout should work without completing the pointee. Another expression-specific eager exception would leave the permanent incomplete-type cache intact.

## R03 — P1: Union initialization selects the last nonzero member

- [ ] Carry the active union member independently of its value.

**Source:** [lowering/globals.rs](../../compiler/cx-mir-lowering/src/lowering/globals.rs), around line 84.

The new `fields.iter().rev().find(|(_, value)| !zero(value))` selects the last nonzero initializer rather than the member selected by initialization.

```c
union U {
    int a;
    int b;
};
union U u = { .a = 7, .b = 0 };
int main(void) {
    return u.b != 0;
}
```

GCC returns 0. Both CX backends return 1, including LLVM at O2. A struct-valued union member followed by a zero-valued scalar override fails similarly.

**Recommended fix:** Normalize union initialization earlier so the constant explicitly contains the selected member. LMIR lowering can overlay that member directly, including zero-valued members, without searching its contents.

## R04 — P1: LLVM opaque-storage alignment misses the new alignment-16 type

- [ ] Make LLVM storage layout agree with LMIR for every opaque type.

**Source:** [typing.rs](../../compiler/cx-backend-llvm/src/typing.rs), around line 134; the new `__float128` entries in the HIR and THIR intrinsic-type tables.

Opaque lowering chooses alignment carriers for alignments 2, 4, and 8, then falls back to an alignment-1 byte array. The changes also introduce `__float128` as an opaque type with size and alignment 16.

```c
struct S {
    char c;
    __float128 f;
    int i;
};
struct S s = { .c = 1, .i = 7 };
int main(void) {
    return s.i != 7;
}
```

GCC and Cranelift return 0; LLVM returns 1 at O0 and O2. A diagnostic probe printed `7 48 32` under GCC and `0 48 32` under CX LLVM for `s.i`, `sizeof(s)`, and the computed offset of `i`.

LLVM emits the global as `{ i8, [16 x i8], i32 }`, while accesses use LMIR's offsets. Giving the global alignment 16 does not make its internal field layout agree with LMIR.

**Recommended fix:** Derive LLVM storage layout, including padding, from LMIR's size/alignment contract. Adding another alignment-specific match arm would leave the broader layout disagreement possible.

## R05 — P2: Anonymous members use a sentinel name and inconsistent lookup

- [ ] Represent anonymity explicitly and resolve promoted members consistently.

**Sources:** [parse/types.rs](../../compiler/cx-parsing/src/parse/types.rs), around line 160; [HIR lowering/ty.rs](../../compiler/cx-hir-lowering/src/ty.rs), around line 212; [HMIR lowering/ty.rs](../../compiler/cx-hmir-lowering/src/ty.rs), `member_path` around line 455.

The parser manufactures names beginning with `__anonymous_member_`, and HIR lowering treats every field with that prefix as anonymous. A named field such as `__anonymous_member_user` consequently becomes inaccessible.

Access and `offsetof` use a recursive member path, but initializer resolution still looks up direct fields only. This GCC-valid program is rejected by both CX backends:

```c
struct S {
    struct {
        int x;
    };
};
struct S s = { .x = 9 };
int main(void) {
    return s.x != 9;
}
```

The diagnostic says `S` has no member `x`. The recursive resolver also selects the first matching path: duplicate promoted names, including direct members colliding with promoted fields, are accepted where GCC rejects them.

**Recommended fix:** Store anonymity as field metadata. Use one member-path resolver for access, initialization, and `offsetof`, with ambiguity checking instead of first-match selection.

## R06 — P2: String-literal sizeof depends on operand syntax

- [ ] Preserve the C literal's array type until expression-context decay.

**Sources:** [eval/types.rs](../../compiler/cx-hmir-lowering/src/eval/types.rs), around line 133; the matching shortcut in [op/unop.rs](../../compiler/cx-typechecker/src/type_checking/op/unop.rs), around line 315.

The new shortcut recognizes a direct string literal and returns its length plus one. It does not give the literal its C array type, so surrounding operations expose the wrong type:

```c
int main(void) {
    return sizeof(*&"abc") != 4;
}
```

GCC returns 0; both CX backends return 1.

**Recommended fix:** Give C string literals their array type before expression-context decay. This fixes address-of, dereference, and `sizeof` together and removes the direct-literal shortcuts. This is distinct from R01's byte-storage problem.

## R07 — P2: Parenthesized declarators remain spelling-specific

- [ ] Parse grouping recursively and align preparse with it.

**Sources:** [parse/types.rs](../../compiler/cx-parsing/src/parse/types.rs), around line 673; [preparse.rs](../../compiler/cx-parsing/src/preparse.rs), typedef scanning around line 105.

The new branch accepts exactly an identifier followed by a closing parenthesis. It does not make grouping recursive, and preparse retains separate depth/pointer heuristics.

GCC accepts these programs; CX rejects them:

```c
int main(void) {
    int ((a)) = 9;
    return a != 9;
}
```

```c
typedef int (T);
int main(void) {
    T a = 9;
    return a != 9;
}
```

The first fails while parsing the declaration; the second leaves `T` unrecognized as a type. Additional GCC-valid probes with `typedef int (*(Fn))(int);` and `int (*p)[3]` also failed.

**Recommended fix:** Use a recursive declarator reader, shared with preparse where practical, instead of branches for individual spellings. These are coverage gaps in the expanded support, not established regressions.

## R08 — P2: Intrinsic-word sorting only normalizes part of the declaration

- [ ] Gather and validate declaration specifiers before canonicalizing the type.

**Source:** [parse.rs](../../compiler/cx-parsing/src/parse.rs), `parse_intrinsic` around line 526.

The sorting collects only consecutive intrinsic tokens. A qualifier interrupts collection, so valid permutations still fail:

```c
long const unsigned int x = 7;
int main(void) {
    return x != 7;
}
```

GCC compiles this and returns 0; CX reports a parse error. `long unsigned const int x = 7;` fails too.

**Recommended fix:** Gather type specifiers and qualifiers together, validate the combination, then construct the canonical type. Sorting a partial source spelling leaves handling fragmented. This is a coverage gap, not an established regression.

## R09 — P2: Explicit benchmark selection can succeed without measurements

- [ ] Report unsupported explicit selections instead of an empty successful report.

**Source:** [benchmarks/main.rs](../../tests/benchmarks/src/main.rs), toolchain filtering around line 83.

```sh
target/debug/cx-benchmarks --case lua --backend cranelift --format json
```

The command exits successfully with:

```json
{
  "schema": 2,
  "cases": []
}
```

Lua's backend restrictions filter out the explicitly requested toolchain, and the runner does not reject the empty selection.

**Recommended fix:** Reject unsupported explicit case/backend combinations or report skipped cases clearly. A successful invocation should indicate what was measured.

## Changes worth retaining

The switch-body/label representation is a useful structural change: it preserves labels in the body and removes duplicated segment reconstruction and sorting by source location. These findings do not justify reverting that direction.

R01 through R04 are the immediate correctness blockers. R05, R07, and R08 are the strongest opportunities to simplify implementation at a shared representation or grammar boundary.

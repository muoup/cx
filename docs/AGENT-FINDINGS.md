# Agent Findings

> AI-generated temporary audit findings. This file is an append-only planning aid, not canonical CX documentation. Each finding should be confirmed by a human owner before implementation or publication.

Audit snapshot: 2026-08-25, commit `0612f0113af2fb397a8166d20176d468f5e809a0`, branch `dev`.

The audit was read-only for compiler, language, tests, editor, and site files. The only workspace change requested by the audit was this temporary findings file. No build or test suite was run; the observations below come from source, tracked files, fixture enumeration, and history inspection.

## Executive assessment

The highest-value work is source-of-truth cleanup, editor integration repair, and test-harness coverage rather than a broad compiler rewrite. The typechecker control-flow code is not stale dead CFG construction: it performs a separate reachability and scope-flow job that is still consumed by return, loop, match, switch, yield, and implicit-return checking. It is over-described as a CFG and contains cleanup candidates, but deleting it would remove active language semantics.

The strongest confirmed problems are:

- The root README points at an obsolete documentation URL and describes the old test-tree layout, while `tests/README.md` documents the newer generated fixture system.
- `docs/pipeline_design.md` contains a broken relative link and does not describe the current MIR representation of staged expressions or the implemented MIR comptime engine.
- The two files with explicit AI-generated markers in `docs/ideas/` are proposal documents whose terminology now conflicts with the implementation and public manual.
- The Zed extension pins the grammar to the March 6 commit `150d6686`, although the grammar and extension files changed substantially after that revision and the current compiler has keywords and operators the grammar does not recognize.
- The generated integration suite contains tracked `runtime-errors` fixtures that its build script never registers, and compile-only and negative fixtures run only through Cranelift even though end-to-end fixtures have a backend matrix.

The main caveat is that several suspected quality problems need behavior-preserving tests before cleanup. In particular, the typechecker flow model should be renamed and simplified only after nested scope, loop, labeled jump, match, staged-exit, and implicit-return behavior is captured.

## Documentation and source-of-truth audit

### Public documentation drift

`README.md:94-95` links to `https://muoup.github.io/cx/docs/build-system`, but the Docusaurus source is `site/docs/getting-started/build-system.md` and the sidebar places it under the `getting-started` section. The README should link to the generated route represented by that sidebar entry and should be checked by a link validation job after every docs deployment.

`README.md:110-116` describes fixture directories directly under `tests/`, including `analysis-errors`. The current fixtures are under `tests/integration/fixtures`, the generator registers the singular `analysis-error` root in `tests/integration/build.rs:17-23`, and `tests/README.md:1-26` documents inline `CX-STDOUT` annotations and the separate benchmark crate. The root README should either link to `tests/README.md` or be updated from the same generated metadata so the two descriptions cannot drift again.

`docs/pipeline_design.md:85` links to `build_system.md`, which does not exist beside the document; the maintained page is under `site/docs/getting-started/build-system.md`. The same file's Stage 5 description at `docs/pipeline_design.md:35-40` says staged expressions remain frozen, typechecked THIR fragments. Current lowering creates `MIRStagedTemplate` values and emits `MakeStaged`, `ApplyStaged`, `StagedReturn`, and `StagedExit` instructions, so the architecture page needs to explain where staged values cross the THIR-to-MIR boundary instead of leaving the reader with a frontend-only model.

`site/docs/manual/modules.md:112-116` contains a duplicated `extern "C"` explanation and a truncated sentence ending in “configuration is For handwritten declarations”. This is a content defect independent of compiler behavior and should be repaired before the page is used as a language reference. `site/docs/getting-started/c-interop.md:48-50` also has an unterminated code-fence title, which should be fixed while running a Markdown/Docusaurus build check.

`site/sidebars.ts` intentionally leaves `contracts.md` and `safe-functions.md` out of the visible manual because they are marked work in progress. That is safer than presenting them as stable semantics, but hidden pages remain discoverable through links and can still become stale. Each experimental page should carry an explicit status block, an owner, and the compiler/test revision it was last checked against, or it should be moved to an internal design section.

### Internal compiler design documentation

The useful material in `docs/pipeline_design.md` should move into a Docusaurus “Compiler architecture” section, but it needs a rewrite rather than a copy. The page should document actual crate and data ownership: tokenization and preparse, HIR/declaration combination, type checking, THIR, semantic MIR, MIR analysis, LMIR lowering, backend code generation, linking, and C header generation. It should also distinguish compile-time value evaluation from staged artifact construction.

The current implementation gives concrete anchors for that rewrite:

- `compiler/cx-parsing/src/parse.rs:61-66` routes `comptime` declarations into a real parser, and `compiler/cx-parsing/src/parse/functions.rs:104-215` parses comptime function and staged-expression initializers.
- `compiler/cx-mir/src/global.rs:31-36` currently defines `Runtime`, `Constexpr`, and `Comptime` function modes; it does not define the `ComptimeOnly` mode described in the old design note.
- `compiler/cx-mir-comptime/src/lib.rs:20-30` dispatches runtime/constexpr evaluation separately from deferred comptime instruction generation, while `:72-99` evaluates pending global initializers in declaration order.
- `compiler/cx-mir-comptime/Cargo.toml:6-12` depends on both `cx-mir` and `cx-thir`, so the statement in the old plan that the evaluator depends on MIR “not THIR” is too broad for the current crate boundary.
- `compiler/cx-thir-lowering/src/lowering.rs:914-922` and `compiler/cx-thir-lowering/src/builder.rs:221-227` show staged values being materialized into MIR instructions, and the MIR analysis and formatting code handle those instructions explicitly.

The architecture page should state which facts are normative, which are implementation details, and which are proposals. A useful long-term split is:

- The public manual defines shipped syntax, user-visible semantics, diagnostics guarantees, and supported interop.
- The compiler architecture section defines current phase ownership and representation invariants.
- A versioned design section contains proposals with an owner, status, alternatives considered, and a removal or review date.
- The fixture corpus demonstrates behavior and should be treated as executable evidence, not as an accidental documentation dump.

This split makes it possible to merge worthwhile internal material into Docusaurus without treating old implementation plans as language guarantees.

### Explicitly AI-generated documents

The repository-wide marker search found two explicit AI-generated documents and no other exact matches:

- `docs/ideas/comptime.md:1-5` begins with “AI-generated summary” and describes a future `constexpr`/`comptime` model that does not match the current implementation status.
- `docs/ideas/mir_comptime.md:1-5` is explicitly an “AI-Generated Document”, and its `ComptimeOnly` and MIR-only dependency model conflict with the current enum and crate dependencies described above.

The recommended action is to remove both files from the canonical tree after extracting any still-useful decisions into a human-reviewed architecture or design page. If the historical reasoning matters, preserve it in an explicitly archived proposal directory with a date and status rather than leaving it adjacent to active documentation. The marker search cannot prove that unmarked documents were or were not AI-assisted, so cleanup should use ownership and review status rather than attempting to infer authorship from prose.

## Compiler design and code-quality findings

### Typechecker control flow: keep the behavior, narrow the abstraction

`compiler/cx-typechecker/src/environment/control_flow.rs` is active code, not an obsolete CFG generator. `ControlFlowSnapshot` tracks reachability across active lexical scopes (`:5-19`), scope arrows route break/continue/merge paths (`:34-47`), and the typechecking control-flow helpers consume those results for fallthrough and loop-increment behavior. MIR later computes actual block successors in `compiler/cx-mir-analysis/src/liveness.rs:7-45` and propagates path-sensitive ownership in `compiler/cx-mir-analysis/src/ownership.rs:71-125`, so the two layers have different responsibilities.

The cleanup opportunity is vocabulary and state shape. The typechecker module should be renamed or documented as scope reachability rather than a full CFG, and its public state should expose only facts needed by typechecking: whether execution may fall through, which break/continue/yield targets are valid, and which paths must replay a loop increment or implicit return. The implementation should then be validated against MIR terminators for representative programs before removing any state.

Specific cleanup candidates are visible in the current representation:

- `Scope.anchor_range` is assigned by `set_scope_anchor` and merge/loop configuration but has no read site in the typechecker source. Either wire it into diagnostics or remove it.
- `MergeScopeState.include_current_snapshot` is an `Option<String>`, but `resolve_scope_flow` only checks whether it is present at `:325-347`; the string value is not used. A boolean or named enum would express the actual invariant.
- `ControlFlowSnapshot` clones a `Vec<bool>` for every arrow. This may be acceptable at current scales, but a compact immutable reachability state or a scope-local merge model should be considered only after tests establish that the current snapshots are the dominant cost.
- `ControlFlow::pop_scope` at `:109-129` has a redundant `expect` after checking `final_reachable` and uses panic-based stack invariants. Those are reasonable internal assertions only if the surrounding compiler error boundary makes them actionable; otherwise they should become structured internal errors.
- Arrow labels carry routing information into loop and match handling, but their diagnostic value is unclear. Either use them in source diagnostics or reduce the label plumbing.

The first sprint should not delete arrow generation. It should add focused tests for nested loops, labeled break/continue, match and switch joins, `for` increment replay, yield, implicit returns, and staged loop exits; rename the module around reachability; then remove only state proven redundant by those tests.

### Bloat and poor-quality implementation targets

Several files are large enough that ownership boundaries are difficult to review, although line count alone is not proof of a design defect:

- `compiler/cx-typechecker/src/symbol/completion.rs` is 930 lines, `type_checking/op/binop/calls.rs` is 797, `symbol/resolution.rs` is 716, and `type_checking/typechecker.rs` is 697. These should be split by semantic responsibility, such as lookup, candidate ranking, call checking, and diagnostics, after the current behavior is covered.
- `compiler/cx-thir-lowering/src/lowering.rs` is 1,084 lines, while `lowering/staged.rs` and `lowering/control_flow.rs` are 766 and 519 lines. A useful boundary is one module per expression family with a narrow builder interface, leaving staged and control-flow lowering as explicit consumers rather than another cross-cutting utility layer.
- `compiler/cx-mir-analysis/src/ownership.rs` is 689 lines and should stay focused on path-sensitive state transfer; formatting, diagnostics, and staged-specific policy should not accumulate there.
- `compiler/cx-mir-comptime/src/engine/execution.rs` is 495 lines. It was recently separated into an engine crate, so the next improvement should be API and invariant cleanup rather than another broad rewrite.

The small, high-confidence cleanup list includes the misspelled internal API `evaluate_compite_expr` in `compiler/cx-mir-comptime/src/lib.rs:57-69`, the now-suspicious staged/comptime simplification TODO in `compiler/cx-mir/src/expr.rs:28`, and the growing `#[allow(dead_code)]` set in THIR lowering and the test runner. Each allow should be classified as intentional public API, planned feature surface, generated-code support, or dead code, then removed or tracked with an issue.

The TODO/FIXME inventory also identifies separate implementation sprints rather than one cleanup bucket: the parser declaration-suffix hack at `compiler/cx-parsing/src/parse/functions.rs:312`, unsupported variadic builtins in the Cranelift backend at `compiler/cx-backend-cranelift/src/instruction.rs:119-120`, missing LLVM assumptions at `compiler/cx-backend-llvm/src/instruction.rs:199`, conversion organization in `compiler/cx-typechecker/src/type_checking/coercion/implicit/conversion.rs:129`, and unsupported cached compilation artifacts in `compiler/cx-pipeline/src/scheduler.rs:376`. These should be triaged by user impact instead of removed mechanically.

There is also a recoverable-error boundary problem. Header generation uses `unwrap` for file output and UTF-8 conversion in `compiler/cx-c-header/src/lib.rs:18,80,99-105`, and LSP diagnostic grouping turns a source read failure into an empty string at `compiler/cx-lsp/src/typecheck_service.rs:201-203`. A dedicated error-propagation pass should distinguish compiler invariants, user input errors, and environmental I/O failures, because replacing every `unwrap` would obscure valid internal assertions while leaving silent user-facing failures untouched.

## Corpus and language-quality findings

### Current corpus shape

The tracked integration fixture tree contains 301 entries: 184 `.cx` files, 108 `.c` files, 8 headers, and one text support file. The source categories contain 158 end-to-end cases, 54 compile-only cases, 64 type-error cases, 13 analysis-error cases, 10 parse-error cases, and 2 runtime-error cases. The end-to-end tree has 12 comptime cases, which gives the current implementation a useful starting point for a more systematic matrix.

The test generator in `tests/integration/build.rs:17-23` registers only `end-to-end`, `compile-only`, `parse-errors`, `type-errors`, and `analysis-error`. The two tracked files under `tests/integration/fixtures/runtime-errors` are therefore not generated into the integration suite, and no source reference to that category exists in the test crate. Decide whether those fixtures are obsolete or add a real runtime-failure category with explicit expected behavior; leaving them in the tree makes the corpus look larger than the executed suite.

`tests/integration/test.rs:16-58` runs compile-only and negative cases only with Cranelift, while `:83-129` runs end-to-end cases with Cranelift and optional LLVM. This is a reasonable fast default for parser and typechecker diagnostics, but compile-only cases still exercise lowering and backend code generation, so the current arrangement can hide backend-specific regressions. Add a backend matrix for selected compile-only and analysis cases, or document and enforce the intentional boundary.

There are no tracked `.cxh` fixtures even though `site/docs/manual/modules.md:178` describes `.cxh` library entry points. The corpus has C headers, but no direct test of CX header generation, generated-header inclusion by C, or a library round trip through the documented workflow. That should be a high-value interop slice because it tests the boundary users depend on and exercises name mangling, layout, visibility, and ABI together.

### Recommended corpus expansion

Build a coverage manifest from explicit feature metadata rather than counting words in filenames or source. Each case should declare its language area, expected compiler stage, backend expectations, C standard mode, ownership mode, and whether it is a positive, negative, diagnostic, ABI, or differential test. This makes missing coverage visible and prevents a feature from appearing covered merely because its name occurs in a fixture.

The first additions should cover:

- C99 declarations, incomplete and recursive tags, tentative definitions, qualifiers, integer promotions, compound assignments, designated initializers, function pointers, varargs, `goto`/labels, and `_Generic` or explicitly unsupported constructs.
- CX-specific `defer`, `safe`/contracts, `@nocopy`/`@nodrop`, namespaces, templates, staged captures, nested staged control flow, `yield`, `emit`, `then`, `alignof`, and pipe operators, each with valid and invalid cases.
- Compiler diagnostics with stable stage, range, and message-category assertions, because the current negative harness checks the broad failure stage but not semantic quality of the diagnostic.
- Both Cranelift and LLVM for ABI-sensitive aggregates, globals, calls, varargs, staged lowering, and generated headers, with independently compiled C callers and callees where the ABI is the subject.
- A small parser/LSP corpus that checks syntax recovery, keyword highlighting, semantic-token ranges, and diagnostics for every newly supported construct.

The corpus should remain split from expensive benchmarks. The existing `tests/README.md:3-17` already separates the integration crate from the benchmark runner; preserve that boundary as the corpus grows.

## LSP, Tree-sitter, and Zed findings

### Confirmed revision and grammar drift

`compiler/cx-zed-extension/extension.toml:10-13` pins the remote grammar to `150d6686a6777a4d26e319b794e9fe0c9666b578`, which is the March 6, 2026 LSP merge. The current checkout is August 25, and `git diff` from that revision to `HEAD` shows 11 changed grammar/extension files with 1,801 insertions and 597 deletions. The extension revision is therefore stale unless the old commit was intentionally frozen.

The current `compiler/cx-treesitter-grammar/grammar.js:103-128` is a lexical/grouping grammar: it repeats elements and groups braces, parentheses, and brackets, but it does not model declarations, function signatures, or expression structure. It cannot provide a comptime-function syntax tree merely by adding `comptime` to the keyword list. Decide whether the project wants a robust syntax tree or only highlighting and delimiter recovery; the grammar, queries, and acceptance tests should match that declared scope.

The grammar's keyword list at `grammar.js:9-50` is behind `compiler/cx-tokens/src/token.rs:370-422`. It omits at least `goto`, `true`, `false`, `alignof`, `as`, and `defer`, even though the compiler token set recognizes them. Its operator list at `:151-186` omits the language's `<|` and `|>` pipe operators and compound `&=`, `|=`, and `^=` assignments. These gaps produce incorrect highlighting or error recovery even if a full syntax tree is not yet planned.

The query files are also inconsistent: `compiler/cx-treesitter-grammar/queries/highlights.scm:1-10` highlights operators, while the copied Zed query at `compiler/cx-zed-extension/languages/cx/highlights.scm:1-9` does not. The Zed language config at `compiler/cx-zed-extension/languages/cx/config.toml:1-4` advertises `.cxl`, which has no compiler or documentation references, and omits the documented `.cxh` library extension. `compiler/cx-treesitter-grammar/tree-sitter.json:2-12` itself lists only `.cx`, so the extension and grammar do not have one agreed file-type policy.

The grammar package is not a root Cargo workspace member (`Cargo.toml:4-11`), and its Rust build script only reruns on the committed generated `parser.c` (`compiler/cx-treesitter-grammar/bindings/rust/build.rs:10-14`), not on `grammar.js`. Add a standalone grammar regeneration/check job or a repository check that fails when generated parser artifacts, highlight queries, and the Zed package disagree. Bumping the Zed revision should be part of the same change as grammar updates.

### LSP architecture and packaging

The LSP server currently typechecks documents and lexes source for semantic tokens; it does not use Tree-sitter for diagnostics. That means a Tree-sitter fix will improve Zed parsing/highlighting and recovery, but it will not fix compiler diagnostics or comptime parsing in the language server itself. This boundary belongs in the architecture documentation so editor issues are triaged against the right component.

`compiler/cx-zed-extension/src/lib.rs:5-15,22-31` hardcodes the language-server command to `target/debug/cx-lsp` relative to the extension checkout. That is a local-development path, not a self-contained installed-extension strategy. Validate the extension in a clean Zed installation and choose an explicit packaging model: build the server as part of extension installation, download a versioned release, or require a configured external command and document that requirement.

The LSP diagnostic path silently substitutes an empty source when a file read fails (`compiler/cx-lsp/src/typecheck_service.rs:201-203`). Preserve the actual error or omit only the affected diagnostic with a clear log entry, because empty text can turn a real I/O failure into incorrect line and column ranges.

## Candidate agent semantics skill

Do not generate the skill as part of this read-only audit. It is a good follow-on once the manual, architecture page, and corpus have explicit ownership.

The skill should be generated from a versioned semantics source rather than written as an unverified C summary. A useful source model would map each rule to its C99 basis or CX extension, the user-facing documentation page, compiler phase and implementation module, positive fixtures, negative fixtures, and an “experimental/unsupported” status. The generated `SKILL.md` could then cover lexical and preprocessor rules, declarations and types, C conversions, ownership and cleanup, safe functions, modules and visibility, templates, comptime/staging, ABI/C interop, and diagnostic conventions.

The skill should explicitly mark where CX departs from C. C knowledge is useful for writing ordinary expressions and declarations, but it cannot define linear ownership, `@nodrop`, staged values, namespace visibility, or backend/ABI constraints. The skill should also instruct an agent to compile and run a small fixture before assuming a rule, and to treat the public manual plus executable corpus as the source of truth when prose and implementation disagree.

The acceptance loop for the skill should be an agent evaluation set: generate small programs from each rule, compile them through both supported backends where applicable, check expected diagnostics for invalid programs, and compare C-subset behavior against GCC or Clang for selected C99 cases. Keep the skill version pinned to a compiler commit and fail its CI job when referenced docs or fixtures disappear.

## Suggested implementation sprints

1. **Establish the source of truth.** Fix the root README routes and test-tree description, repair the malformed manual pages, decide the status of hidden WIP pages, and rewrite `docs/pipeline_design.md` as a current architecture page before moving it into Docusaurus. Remove or archive the two explicitly AI-generated proposal files only after their useful decisions have been reviewed.

2. **Repair the executed corpus.** Decide the fate of `runtime-errors`, add the missing test category or remove the stale fixtures, publish a generated fixture manifest, and add `.cxh`/generated-header/C-caller coverage. Add selected LLVM runs for compile-only and analysis-sensitive cases so backend parity is measured where it matters.

3. **Synchronize editor integration.** Choose lexical highlighting versus a real syntax tree, update the grammar keyword/operator set from the compiler token table, reconcile `.cx`, `.cxh`, and any intentional legacy suffixes, synchronize highlight queries, regenerate artifacts, bump `extension.toml`, and test the installed extension with a built language server rather than only a repository checkout.

4. **Tighten typechecker flow boundaries.** Preserve the current behavior with focused reachability tests, rename the module around scope flow, remove unread fields and stringly-typed flags, and document the division between typechecker fallthrough and MIR CFG/ownership analysis. Review snapshot cloning only after profiling a realistic corpus.

5. **Do a targeted implementation-quality pass.** Correct the comptime API typo, retire superseded TODOs, classify `#[allow(dead_code)]`, split the largest files by semantic ownership, and improve environmental error propagation in header generation, scheduling, and LSP diagnostics. Keep each change narrow enough that the existing corpus identifies semantic regressions.

6. **Generate the semantics skill.** Build it from the reviewed manual, architecture invariants, and corpus manifest, pin it to a compiler revision, and evaluate it with positive, negative, backend, ABI, and C-differential cases. Treat the skill as a consumer of the project’s sources of truth, not as a replacement for them.

## Definition of done for the audit follow-up

- Every public documentation link resolves, every manual page has an explicit stability status, and the root README agrees with the generated test harness.
- The compiler architecture page names the actual IR and crate boundaries, including MIR comptime evaluation and staged MIR values.
- No unreviewed AI-generated draft remains in the canonical documentation tree; archived historical material is labeled and dated.
- The fixture manifest reports what is executed, including runtime failures and backend coverage, and no tracked fixture category is silently ignored.
- The Tree-sitter grammar, generated parser, highlight queries, Zed revision, file suffixes, and LSP packaging are checked together in CI.
- Typechecker reachability and MIR CFG responsibilities are documented separately, and cleanup removes only state covered by behavior tests.
- The future semantics skill links every rule to reviewed documentation and executable evidence, with unsupported and experimental behavior clearly marked.

## Questions for project-owner decisions

- Is `.cxl` an intended future extension, or should the Zed configuration advertise `.cx` and `.cxh` to match the compiler and manual?
- Should `runtime-errors` become a supported integration category, or are those two fixtures historical leftovers?
- Is the desired Tree-sitter deliverable syntax highlighting/recovery or a real parse tree for language tooling?
- Is `constexpr` planned as a public language feature, or should internal `MIRFunctionMode::Constexpr` remain an implementation detail and the manual avoid implying surface support?
- Should contracts and safe functions be presented as experimental public documentation or kept internal until their semantics stabilize?

## Tree-sitter implementation pass

Implementation snapshot: 2026-08-25, working tree changes are uncommitted. The previous lexical/grouping grammar has been replaced with a structured, recovery-oriented grammar covering declarations, types, function and function-pointer declarators, templates, control flow, expressions, comptime functions, staged blocks and continuations, contracts, compiler intrinsics, tagged aggregates, and `@unpack` bindings. The grammar now includes the current CX keyword/operator surface used by the tracked fixtures, including pipes, postfix increment/decrement, multiword built-in types, qualified import aliases, casts, and the `unreachable` type.

The parser was regenerated with Tree-sitter CLI 0.26.3. The positive/negative fixture scan reports no `ERROR` or `MISSING` nodes in 170 tracked `.cx` fixtures from the executed end-to-end, compile-only, type-error, and analysis-error categories after excluding the two standalone preprocessor support files. The intentionally invalid `parse-errors` fixtures remain expected parser failures and were not treated as regressions.

The grammar query and copied Zed query now capture operators and structured type/function/parameter/property nodes consistently. `tree-sitter.json` and the Zed language config both advertise `.cx` and the documented `.cxh` library extension; the old `.cxl` suffix had no compiler or documentation references. Five Tree-sitter corpus tests under `compiler/cx-treesitter-grammar/test/corpus/cx.txt` lock in comptime functions, staged blocks, function pointers/multiword types, unpack/contracts, and C syntax aliases/casts.

The Zed grammar revision remains the old committed hash `150d6686a6777a4d26e319b794e9fe0c9666b578` in `compiler/cx-zed-extension/extension.toml`. It should be bumped to the commit containing this grammar after the change is committed and pushed; pointing the manifest at the current pre-change commit would make the published extension ignore the implementation. The grammar crate is still outside the root Cargo workspace, so Tree-sitter CLI and corpus validation passed locally but a Cargo package test requires either an isolated package workspace or an intentional root-workspace/lockfile change.

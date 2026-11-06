# Standard

Agent-maintained. The rest of `docs/` is human-written; this folder is written and kept current by agents and audited by humans.

A standard states what the compiler *does*, not what it should do. Special cases, value-shape reliance and unimplemented paths are recorded as-is, so they can be found and removed later. Semantic changes are planned against it.

## Files

| file | covers |
|------|--------|
| [preparsing.md](preparsing.md) | tokens to the type-name registry, imports, namespace aliases |
| [parsing.md](parsing.md) | tokens to HIR, HIR to HMIR |
| [hmir-lowering.md](hmir-lowering.md) | HMIR to MIR: units, symbols, types, evaluation, comptime execution, MIR emission |


## Conventions

- Plain truncated English; CX syntax and abstract variables (`T`, `x`, `e`) allowed; no type-theory notation.
- Each section carries `verified <hash>`: the last commit it was checked against. Unchanged hash on changed code means possibly stale.
- Rule ids are stable, unique, and prefixed by kind:
  - `rule/` behavior or rejection.
  - `inv/` property that always holds.
  - `todo/` unimplemented path (panic or `todo!`).
  - a slug starting `quirk-` marks a special case or likely-unintended behavior, e.g. `rule/quirk-...`.
- A rule belongs to its section. Elsewhere, mention it as section link plus id: [Assignment](hmir-lowering.md#assignment) `rule/no-const-reassignment`.
- Diagnostics by code only (`T0005`); messages live in the catalogue.
- Internal helpers sit at the bottom of their section; trivially short ones are inlined where used.

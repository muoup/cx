---
title: Language Manual
description: Authoritative syntax and semantics for the currently implemented CX language surface.
slug: /manual/
---

# Language Manual

This manual describes the currently implemented CX language surface. It is the
public source of truth for syntax and semantics, not a complete statement of all
future design ideas.

CX is an experimental C-like systems language with opt-in ownership and
verification features. It preserves explicit control flow and predictable data
layout while adding:

- templates
- member and static member functions
- contracts
- ownership attributes via `@nocopy` and `@nodrop`
- tagged unions with `match` and `is`
- `safe` functions with optional verification through `--analysis`

Start with [base syntax](./base-syntax.md), then continue through aggregate
types, control flow, ownership, and safe functions.

---
title: Project Status
description: What works in the compiler today, what is partial, and what is still in progress.
---

# Project Status

Chloro is a research preview. The compiler builds working programs, but the language and standard library are still changing.

| Area | State | Notes |
| --- | --- | --- |
| Cranelift backend | Working | Default code generator. |
| Projects and modules | Working | `chl init`, `chl build`, and `chl.lua`. |
| Linear resources, tagged unions, templates | Working | Covered in the [manual](../manual/overview.md). |
| C99 compatibility | Partial | Most C compiles unchanged; some features are missing. |
| LLVM backend | Optional | Build with `--features backend-llvm`. |
| Safe functions | In&nbsp;progress | Syntax is still changing. |
| Contracts | In&nbsp;progress | Syntax is still changing. |

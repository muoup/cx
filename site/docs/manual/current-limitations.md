---
title: Current Limitations
---

# Current Limitations

CX is experimental. The implemented surface is useful for compiler development
and language exploration, but several pieces remain intentionally incomplete.

Current language limitations include:

- Consuming receiver calls require a whole binding or an owned aggregate rvalue.
- Aggregate hierarchy diagnostics are not yet field-precise.
- FMIR verification is opt-in via `--analysis`.
- Template argument inference is not implemented.
- Some C99 compatibility areas are incomplete.

See the repository issue tracker for active implementation work:
https://github.com/muoup/cx/issues

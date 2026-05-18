---
title: Contracts
---

# Contracts

Contracts are attached with `where` clauses.

```c
int contract(int a, int b)
where
    pre: (a > 0 && b > 0),
    post(result): (result > a && result > b)
{
    return a + b;
}
```

Semantics:

- `pre: (expr)` constrains callers.
- `post(name): (expr)` constrains the returned value.
- Multiple clauses are comma-separated.

Current runtime behavior:

- In non-analysis builds, contracts remain runtime-relevant.
- In `safe` functions, contract expressions are also checked as part of the safe
  subset.

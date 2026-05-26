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

- `pre: (expr)` is the precondition of the function, the caller asserts to the callee that the condition over the parameters provided is true.
- `post(name): (expr)` is the postcondition, the callee asserts that such value is true of the returned value in regards to the state of the variables provided at call-time. If the contracted function mutates its passed parameters, the condition will refer to their state prior to any mutation.

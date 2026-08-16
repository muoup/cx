---
title: Contracts
---

# Contracts

Contracts are a work-in-progress feature attached with `where` clauses.

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
- `post(name): (expr)` is the postcondition, the callee asserts that such condition is true of the returned value in regards to the state of the variables provided at call-time. If the contracted function mutates its passed parameters, the condition will refer to their state at call-time. Indirect mutation, i.e. modifying data via a passed pointer or reference however will not be reversed at condition evaluation.
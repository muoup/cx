# Function Contracts

This proposal introduces the concept of "function contracts" to the `cx` language, allowing developers to specify preconditions and postconditions for functions. This feature aims to improve code correctness and robustness by defining formal constraints on function inputs and outputs.

## Syntax

The proposed syntax extends function declarations with a `where` clause:

```c++
void function(int param1, int param2, const int cparam1, const int cparam2)
    where precondition: [condition(param1, param2, cparam1, cparam2)],
          postcondition: [condition(.ret, cparam1, cparam2)]
{ ... }
```

## Semantics and Behavior

Function contracts are primarily informational for the caller of a function. They define the responsibilities of the caller (`precondition`) and the guarantees of the function itself (`postcondition`).

The enforcement of these contracts depends on the build configuration:

*   **Debug Builds:** In a debug build, the contracts are converted into assertions. The `precondition` is checked *before* the function call in the caller's scope. The `postcondition` is checked *after* the function call, also in the caller's scope. A failed assertion will lead to a runtime panic.

*   **Release Builds:** In a release build, the contracts are treated as assumptions for the compiler's optimizer. This means the compiler can use this information to generate more efficient code, assuming the developer has ensured the contracts are always met. There are no runtime checks in release builds, so contract violations can lead to undefined behavior.

### Preconditions

A `precondition` is a condition that must hold true before the function body is executed. It is specified in the `where` clause and can access all of the function's input parameters.

### Postconditions

A `postcondition` is a condition that must hold true after the function has returned. It can access the function's return value (denoted by `.ret`) and its `const` input parameters.

**Important Note:** Postconditions **cannot** access non-const parameters. This is because the contract checks are performed in the caller's context. The caller only has access to the parameter values as they were passed to the function, not their potentially mutated state after the function has executed. While it might be possible to use the *initial* value of a mutable parameter, this could be confusing and error-prone.

## Future Work

While not currently in the works, future iterations of the compiler will attempt to support some in-house optimization and static analysis, meaning that certain contract violations could be caught at compile time rather than runtime, allowing for more robust safety guarantees.
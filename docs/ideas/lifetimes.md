# Lifetimes

In order for our language to have true "safe" semantics, we need some way to reason over lifetimes. We can borrow from Rust here, but given our usage of staged expressions (and future features like ghost variables), we can simplify the syntax and rules behind them while still having fairly ergonomic programming idioms. We should, in unsafe contexts, try to preserve fairly lax and unrestrictive C++-style usage of things like references.

## Idea: Ephemeral Resources

A simple case to cover would be short-lived resources. In a lot of cases, a function that takes in a reference will do so without said reference living beyond the scope of the function. Let's call this kind of value 'ephemeral', it is a value which does not outlive its rvalue that materializes it. These semantics should not persist by default in non-safe functions, but should be default behavior of a reference in a safe function, or safe functions should not be allowed to use unannotated references.

```c
void procedure(resource& ref) safe { ... }

// or

void procedure(resource &' ref) safe { ... }

```

Here, since the reference is not annotated, because this function is safe, its reference is guaranteed (unless using unsafe code, something to be restricted in the future, see #[permissions](permissions.md)) to not outlive the scope of the function. This is vital any kind of 'cell' type. For instance, for the existing std::rcell (runtime cell), we expose a means by which to borrow the reference and check against its runtime-tracked ownership. We introduce a function, std::rcell::borrow, which exposes the passed provided executed expression access to a safely extracted reference to the inner resource, defined as such:

```c
comptime expr void rcell::borrow<T>(expr rcell<T>& cell, expr(T&) void proc) {
    return emit .{
        if (cell.sema != 0) {
            std::panic("Cannot mutably borrow cell while it is currently borrowed");
        }
        cell.sema = -1;

        defer rcell::release(cell);
        proc(cell.inner);
    };
}
```

This function cannot guarantee the safety of the usage of the reference, unless it can prove that the 'defer' statement only executes after no more copies of that reference exist. If a copy of the provided reference persists, say through a locally stored variable with a reference field, and we reset the semaphore, the semaphore would no longer accurately reflect the ownership state. This is okay as long as the code explicitly takes on the extra responsibility of unsafe code via an @unsafe block.

To improve the durability of the above code, we need 'proc' to be guaranteed to be safe code. This behavior could be enforced through syntax through marking 'proc' as safe, or by marking the function as safe. They both come with their pros and cons. An important note to keep in mind about marking rcell::borrow as safe and thus its arguments may be assumed to be safe, is that in non-safe functions, we would have no way to enforce these lifetime rules, even though it seems rather silly to expose rcell in unsafe contexts without the reason you'd use it.

A compromise worth considering here is to allow references to be declared expicitly as ephemeral.
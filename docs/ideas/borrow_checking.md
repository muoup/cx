```cpp

// safe borrowable runtime cell -- equivalent to Rust's refcell 
struct rcell<T> : @copy_traits(T), @unsafe_move {
    T obj;
    i8 sema;
}

void rcell::drop<T>(rcell<T>& cell) /* not safe */ {
    cell.sema = 0;
}

void rcell::drop_const<T>(rcell<T>& cell) /* not safe */ {
    cell.sema--;
}

const T& rcell::borrow_const(rcell<T>& cell) safe {
    if (cell.sema < 0) {
        panic("Cannot immutably borrow cell while it is currently mutably borrowed");
    }

    cell.sema++;
    return cell.obj;
}

T& rcell::borrow(rcell<T>& cell) safe {
    if (cell.sema != 0) {
        panic("Cannot mutably borrow cell while it is currently borrowed");
    }

    cell.sema = -1;
    return cell.obj;
}

...

int main() {
    rcell<int> data = ...;

    proc1(data |> rcell::borrow());
    // Without destructors, how can we force a call (or allow user defined macros to generate calls) to the rcell::drop function, also while avoiding naked @unsafe?
    proc2(data |> rcell::borrow());
}

// IDEA 1:

// Expr T& is a further nod into the pseudo-closure nature of staged expressions, it is a function vals -> expr which allow for consistent bindings in staged expressions
comptime expr void rcell::borrow<T>(rcell<T>& cell, expr(T&) void proc) {
    return emit .{
        if (cell.sema > 0) {
            panic(...);
        }

        proc(cell.obj);
    };
}

comptime expr void rcell::borrow_then<T>(rcell<T>& cell, expr(T&) void proc, expr void then) {
    return emit .{
        rcell::borrow(cell, proc);
        emit then;
    };
}

int main() {
    rcell<int> data = ...;

    data |> rcell::borrow(|ref| proc1(ref), .{
        data |> rcell::borrow(|ref| proc2(ref), .{});
    });

    // Say we introduce some operator <|, which appends the rhs to the parameter list of the lhs, like the inverse of the |> operator, we could shorten this to:
    data |> rcell::borrow(|ref| proc1(ref)) <| 
        .{
            data |> rcell::borrow(|ref| proc2(ref), .{});
        };

    // What we really want however is something sort of similar to Gleam's 'use' operator. Say we introduce some 'then' operator which, which when typechecked in a block, returns the remaining statements in the block as an expr T, we could then simplify this to:

    data |> rcell::borrow(|ref| proc1(ref)) <| then
    data |> rcell::borrow(|ref| proc1(ref));

    // This example is a bit clunky, requiring us to do away with semicolons which is ~okay if the language takes a more strongly functional stance, though I'd like to avoid this given the language's C roots.
    // Ideally we'd want to eventually enable syntax similar to below, however none of these quite slot into the rules previously mentioned.

    int& ref = data |> rcell::borrow() /* `<| then` */ /* `; then` */ /* `;;` */ 

    proc1(ref);
    proc2(ref);
}

```
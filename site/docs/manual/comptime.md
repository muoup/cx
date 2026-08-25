---
title: Comptime and Staged Expressions
---

# Comptime and Staged Expressions

CX provides compile-time functions that can compute values and produce typed runtime code. The latter mechanism uses staged expressions: runtime expressions retained in a typed form so a comptime function can compose them and emit the result at its call site.

Functions that can be evaluated at compile time as well as runtime, similar to C++ `constexpr`, are not yet implemented but are planned for the future. A `comptime` function is different: it must be evaluated during compilation and is not emitted as a runtime function.

## Comptime Functions

A comptime function is declared with `comptime`. Its ordinary parameters are compile-time values, and calling it evaluates the function during compilation:

```cx
comptime int add(int lhs, int rhs) {
    return lhs + rhs;
}
```

Comptime functions may also be templated or associated with a namespace using the syntax described in the preceding chapters.

## Staged Expressions

The syntax `expr T` denotes a staged expression that will produce a runtime value of type `T`. It is not a compile-time-known `T`, and the comptime function cannot inspect its eventual runtime value. Instead, the function can place the expression is used for codegen, acting as a 'frozen' expression which can be inserted and manipulated as needed to enable richer code generation. For instance, Rust's `?` operator, which serves to return None from a optional-returning function if its operand is none, allowing for a concise and safe unwrap, can be neatly reimplemented in CX using a comptime function and staged expressions like so:

```cx
comptime expr T opt::try(expr opt<T> this) {
    return emit match (move this) {
        opt::some(val) => val;
        opt::none => return opt::none();
    };exposed
}
```

Above also introduces the 'emit' operator. In comptime contexts, the 'emit' operator converts the provided operand into a staged expression. In a runtime function, an rvalue expression passed through a method call to a comptime function expecting a staged expression will handle 'staging' the provided parameter automatically, as such the above function can be invoked like so:

```cx
std::opt<int> parse_integer(const str&_ input) { ... }

int i = parse_integer(string)
    |> std::opt::try();

// or equivalently,

int i = std::opt::try(parse_integer(string));
```

For a more technical explanation, expressions are lowered eagerly to derive their value-producing semantic instructions which are implicitly inserted into the function body. 'emit' defers this, allowing the compiler to lower the expression and store its semantic instructions separately in a 'templated'-like form, only inserted into the function body at materialization point -- i.e. where the compiler lowers a staged expression, either through a variable reference or directly as an rvalue. This enables staged expressions to defer things like the target of a break/continue expression as well so that it properly aligns with the context in which the instructions are inserted.

In the future, runtime functions will be able to store comptime variables and as such will need to use the 'emit' operator to stage the initialization rather than evaluate it directly to a value. This however is to-be-implemented.

```cx

```

## Block Expressions

The syntax `.{ ... }` creates a block in a position where CX expects an expression. They are semantically identical to standard blocks, however in places where an expression value is expected, the dot-prefix is used to parse the subsequent expression as a block rather than an initializer list. This syntax proves useful when you want to pass a staged implementation block to a comptime function:

```cx
comptime expr T if_then(bool condition, expr void proc) {
    if (condition) proc;
}

if_then(cond, .{ printf("Condition was true!"); });
```

## Parameterized Staged Expressions

A staged expression can accept parameters at its materialization point, useful for when a comptime function wants to do internal evaluation and ensure that its results can be exposed as context to the provided staged expression, for instance:

```cx
comptime expr void opt::map<T, U>(opt<T> this, expr U(T) proc) {
    match (move this) {
        opt::some(val) => opt::some(proc(this)),
        opt::none => opt::none(),
    }
}
```

At the call site, `|parameters| statement` constructs the staged expression. The parameter types come from the comptime function prototype, so the call site supplies only their names:

```cx
std::opt<_str&> str_val = ...;
std::opt<usize> size_val = move i_val
    |> std::opt::map(|val| std::str_length(val));
```

It should be noted that this syntax, while similar, is not equivalent to other language's concept of anonymous functions or closures. These blocks must be resolved at comptime-time, acting as a way to parameterize a set of staged functionality. One can think of them as a meta-closure, in which its execution logic is used to construct the final code state rather than any runtime execution value.

## Continuations with `then`

When a parameterized staged expression should contain the remainder of the current lexical block, its direct body can be the `then` keyword:

```cx
resource_handle |> with_resource() <| |resource| then

action1(resource);
action2(resource);
```

The above code snippet would be equivalent to:

```cx
resource_handle |> with_resource() <| |resource| {
    action1(resource);
    action2(resource);
};
```

The main use-case for the 'then' keyword is to avoid excess indentation. If the example say was:

```cx
resource_handle1 |> with_resource() <| |resource1|
resource_handle2 |> with_resource() <| |resource2|
resource_handle3 |> with_resource() <| |resource3|

action1(resource1, resource2);
action2(resource3, resource1);
action3(resource2, resource3);
```

Trying to desugar this into its indented form would thus contain 3 layers of nested block contexts, which can quickly hinder the readability
of the code, especially for what is a rather common idiom in the language.

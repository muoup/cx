# CX Language Specification

> This document describes the current implemented surface, not the full intended design space.

## Overview

CX is an experimental C-like systems language with opt-in ownership and verification features. It preserves explicit control flow and predictable data layout while adding:

- templates
- member and static member functions
- contracts
- ownership attributes via `@nocopy` and `@nodrop`
- tagged unions with `match` and `is`
- `safe` functions with an explicitly restricted sublanguage allowing formal verification via the `--analysis` flag

## 1. Base Syntax

CX remains intentionally close to C syntax. It is also not order-dependent: types and functions may be declared in any order within a module. Forward declarations are generally unnecessary.

## 2. Lexical Conventions

### 2.1 Identifiers

Names beginning with `_` are reserved for the implementation.

### 2.2 Additional Keywords

In addition to C keywords, CX reserves:

- `import`
- `defer`
- `strong`
- `weak`
- `new`
- `template`
- `type`
- `match`
- `is`
- `class`
- `safe`
- `where`
- `move`

### 2.3 `@`-Prefixed Compiler Identifiers

CX also uses `@`-prefixed compiler identifiers. These are not ordinary user identifiers.

- `@nocopy`
- `@nodrop`
- `@unsafe`
- `@leak`

## 3. Aggregate Types

### 3.1 Structs

Structs follow C's layout rules for padding and alignment.

Ownership attributes are attached after the name with `:`.

```c
struct Data : @nocopy {
    int value;
};
```

Multiple attributes are comma-separated:

```c
struct Resource : @nocopy, @nodrop {
    int value;
};
```

Since `@nodrop` implies `@nocopy`, the idiomatic form is to leave out the latter:

```c
struct Resource : @nodrop {
    int value;
};
```

### 3.2 Tagged Unions

Tagged unions are declared with `enum union`.

```c
enum union Output {
    integer :: int,
    fp      :: double,
    string  :: const char*,
    error   :: void
};
```

They also support the same ownership attributes:

```c
enum union Result : @nodrop {
    ok  :: int,
    err :: Resource
};
```

Variant construction uses `::`:

```c
Output o1 = Output::integer(42);
Output o2 = Output::fp(3.14);
```

### 3.3 Aggregate Attribute Hierarchy

The attribute hierarchy is:

- trivially copyable; C-style POD semantics
- `@nocopy`
- `@nodrop`

Containment must respect this order.

- A copyable aggregate may not contain a `@nocopy` or `@nodrop` member.
- A `@nocopy` aggregate may not contain a `@nodrop` member.
- A `@nodrop` aggregate may contain either.

This rule applies to:

- struct fields
- all tagged-union variants

The check is transitive over realized member types.

## 4. Member Functions

CX supports namespaced member syntax without dynamic dispatch.

### 4.1 Borrowed Receiver

An instance method declares `this` as its receiver.

```c
struct Counter {
    int value;
};

void Counter::print(this) {
    printf("%d\n", this->value);
}
```

Call syntax:

```c
Counter c;
c.print();
```

### 4.2 Consuming Receiver

A method may consume its receiver with `*this`.

```c
void Box<T>::drop(*this) safe {
    @unsafe {
        @leak(this);
    };
}
```

Current behavior:

- `x.drop()` consumes `x` when `x` is a whole binding
- owned aggregate rvalues may also call consuming receivers
- non-binding places such as `obj.field.drop()` are currently rejected

### 4.3 Static Member Functions

A member declaration with no receiver is a static member function.

```c
struct MyStruct {
    int value;
};

MyStruct MyStruct::create() {
    return { .value = 42 };
}
```

Call syntax:

```c
MyStruct s = MyStruct::create();
```

### 4.4 Destructors

Ordinary structs may define destructors:

```c
~MyStruct(this) {
    printf("destroy\n");
}
```

Current constraints:

- destructors use `this`, not `*this`
- `@nodrop` types may not define destructors
- for `@nodrop` types, cleanup must be explicit via move or `@leak`

## 5. Templates

CX supports templated functions and aggregate types.

Function example:

```c
T add<T>(T a, T b) {
    return a + b;
}
```

Type example:

```c
struct Box<T> {
    T value;
};
```

Current limitations:

- template argument inference is not implemented
- specialization is effectively one definition per function identifier / realized member slot

## 6. Control Flow

### 6.1 `match`

`match` provides tagged-union pattern dispatch without fallthrough.

```c
match (out) {
    Output::integer(i) => printf("%d\n", i);
    Output::fp(d) => printf("%f\n", d);
    default => printf("unknown\n");
}
```

### 6.2 `is`

`is` checks a tagged-union variant and introduces the payload binding on the success path.

```c
if (out is Output::string(s)) {
    printf("%s\n", s);
}
```

## 7. Modules and Visibility

`import` brings public declarations from another module into scope. Imported declarations are not implicitly re-exported.

Visibility is controlled by `public:` and `private:` section headers. Declarations are private by default.

## 8. Contracts

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

- `pre: (expr)` constrains callers
- `post(name): (expr)` constrains the returned value
- multiple clauses are comma-separated

Current runtime behavior:

- in non-analysis builds, contracts remain runtime-relevant
- in `safe` functions, contract expressions are additionally subject to the safe subset

## 9. Ownership Attributes

### 9.1 `@nocopy`

`@nocopy` disables implicit copying.

```c
struct Data : @nocopy {
    int value;
};

void consume(Data data) {}

int good() {
    Data data = (Data) { .value = 1 };
    consume(move data);

    data = (Data) { .value = 2 };
    consume(move data);
    return 0;
}
```

### 9.2 `@nodrop`

`@nodrop` implies `@nocopy` and additionally forbids silent scope exit.

```c
struct Resource : @nodrop {
    int value;
};
```

A reachable scope exit with an undisposed `@nodrop` binding is a type error.

### 9.3 `@leak`

`@leak(x)` marks a local `@nodrop` binding as discharged without transferring ownership.

Current restrictions:

- the operand must be a local identifier
- the binding must be a stack local
- the binding must have `@nodrop`
- conditional `@leak` does not satisfy ownership discharge

### 9.4 Control-Flow Merge Rules

Move state is tracked across:

- `if`
- `match`
- `switch`
- loops
- `break`
- `continue`
- `return`

An inconsistent move state at a reachable join is a type error. Later uses of such a binding are also rejected.

## 10. Safe Functions

`safe` marks a function body for safe-subset checking.

```c
int fn() safe
where
    post(ret): (ret == 1)
{
    return 1;
}
```

`safe` currently means:

- safe signatures only
- safe expressions only, except inside `@unsafe`
- pure contract expressions
- optional FMIR verification when compilation is run with `--analysis`

### 10.1 Core Restrictions

Current enforced restrictions include:

- raw pointers are rejected in safe signatures and safe expressions
- safe code may call only other `safe` functions
- contracts in safe functions must be pure
- `@leak` is unsafe-only in safe code

Note however that safe functions may use unsafe features, however they must be enclosed in `@unsafe` blocks or expressions. The safe-subset checks are suppressed for the enclosed subtree only.

Examples:

```c
int bad(int* ptr) safe { return 0; }
```

```c
int helper() { return 1; }
int bad() safe { return helper(); }
```

```c
int helper() { return 1; }
int bad() safe
where
    post(ret): (helper() == ret)
{
    return 1;
}
```

### 10.2 `@unsafe`

`@unsafe` is the explicit escape hatch inside safe code.

Supported forms:

- `@unsafe(expr)`
- `@unsafe { ... }`

The unsafe island suppresses safe-subset checks for its enclosed subtree only.

### 10.3 FMIR Verification

When the compiler is invoked with `--analysis`, `safe` functions are lowered to FMIR and analyzed before normal MIR-to-LMIR lowering continues.

Example:

```c
int fn() safe
where
    post(ret): (ret == 1)
{
    int x = 0;
    return x;
}
```

Under `--analysis`, this is a compile-time verification error.

## 11. Current Limitations

- consuming receiver calls currently require a whole binding or an owned aggregate rvalue
- aggregate hierarchy diagnostics are not yet field-precise
- FMIR verification is opt-in via `--analysis`
- template argument inference is not implemented

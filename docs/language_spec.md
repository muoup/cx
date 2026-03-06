# CX Language Specification

> This specification reflects the current state of the CX language and may evolve as the language matures.

## Overview

CX is a safe-by-default systems programming language designed as a superset of C99. It extends C with modern type system features — linear types, contracts, and compile-time formal verification — while maintaining full C ABI compatibility and predictable memory layout. CX aims to bring the rigor of modern type theory to systems-level programming without sacrificing the simplicity and transparency that make C effective.

## 1. Base Syntax

CX is a superset of C99 (see the [C99 Standard](https://www.dii.uchile.cl/~daespino/files/Iso_C_1999_definition.pdf)). Any valid C99 program should be valid CX with identical semantics, with the sole exception of additional reserved keywords listed in [§2.2](#22-keywords).

A significant divergence from C/C++ is that CX is **not order-dependent**. Types and functions can be declared in any order; the compiler resolves all declarations within a module before compilation begins. Forward declarations are never required.

## 2. Lexical Conventions

### 2.1: Identifiers

As in standard C, names beginning with `_` are reserved for the implementation and should be avoided in user code.

### 2.2: Keywords

CX reserves the following keywords in addition to those defined by C99. Programs using these as identifiers will require modification:

*   `import`, `defer`, `strong`, `weak`, `new`, `template`, `type`, `match`, `is`, `private`, `public`, `class`, `safe`, `where`, `move`.

#### Compiler Attributes

CX also defines `@`-prefixed compiler attributes, which are not identifiers and cannot collide with user code:

*   `@nocopy` — marks a struct as not trivially copyable. Values of this type must either be initialized with an rvalue, or by an lvalue explicitly
                moved via the `move` keyword. Assignment without `move` is a compile error.
*   `@nodrop` — marks a struct as unable to be implicitly dropped. Every `@nodrop` value is implicitly `@nocopy`, and can never fall out of scope
                without being explicitly consumed via a move or leaked via `@leak`.
*   `@leak(variable)` — 'moves' a '@nodrop' variable without transferring ownership. Used as the escape hatch destructor for linear types. This
                operation is inherently unsafe and requires the programmer to ensure that the contents of the variable are adequately cleaned up
                manually beforehand.

## 3. Additional Type Semantics

### 3.1: Structs

Structs in CX share the same guarantees for padding and alignment as C. You may also use attributes as mentioned in [Compiler Attributes](#compiler-attributes) to enforce ownership semantics for complex non-POD types.

#### Instance Methods

CX allows declaring member functions for structured types. These do not enable polymorphism, but provide a more ergonomic calling convention for functions that operate on a struct. An instance method declares an explicit `this` parameter, and is called via dot notation on an instance.

```c++
struct MyStruct {
    int x;
};

// An instance method: note the explicit `this` parameter.
void MyStruct::print(this) {
    printf("x = %d\n", this->x);
}

void func() {
    MyStruct s { .x = 10 };
    s.print(); // Calls the instance method.
}
```

#### Static Member Functions

A member function declared *without* a `this` parameter is a **static member function**. It belongs to the type's namespace but does not operate on an instance.

*   **Syntax:** `ReturnType TypeName::function_name(params) { ... }` — no `this` parameter.
*   **Invocation:** Called via `TypeName::function_name(args)`, never via dot notation on an instance.

Static member functions are commonly used for factory patterns:

```c++
struct MyStruct {
    int c;
};

// A static member function — no `this` parameter.
MyStruct MyStruct::create_struct() {
    return { .c = 42 };
}

int main() {
    MyStruct s = MyStruct::create_struct();
    printf("s.c = %d\n", s.c);
    return 0;
}
```

#### Destructors

Structs may define destructors, which the compiler calls when an instance goes out of scope. When a struct contains a destructable object (e.g., a strong pointer or another struct with destruction logic), the compiler inserts implicit destruction calls as needed. A pointer to a destructable type carries no special semantics — the compiler cannot deduce ownership through a raw pointer.

```c++
~MyStruct(this) {
    printf("MyStruct destructor\n");
}
```

### 3.2: Tagged Unions

CX provides tagged unions (sum types / variants) declared with the `enum union` keywords. Each member is a variant with a distinct name and type, defined using the `::` separator.

```c++
enum union Output {
    integer :: int,
    fp      :: double,
    string  :: const char*,
    data    :: struct { int a; double b; char c; }
};
```

Instances are constructed via the `::` operator:

```c
Output o1 = Output::integer(42);
Output o2 = Output::fp(3.14);
Output o3 = Output::string("Hello, World!");
```

## 4. Templates

CX supports templates for writing generic functions and types. Template parameters are declared using angle brackets (`<>`). For functions, the template declaration comes after the function name; for types, after the type name.

For information regarding templated symbols, see the [name mangling documentation](name_mangling.md).

**Function Syntax Example:**
```c
T add<T>(T a, T b) {
    return a + b;
}

int main() {
    int int_result = add<int>(5, 10);       // 15
    float float_result = add<float>(5.5, 10.5); // 16.0
}
```

**Type Syntax Example:**
```c
typedef<T> T* Ptr;

struct Data<T> {
    Ptr<T> ptr;
};

int main() {
    int val = 0;
    Data<int> data;
    data.ptr = &val;
    *data.ptr = 42; // val is now 42
}
```

### 4.1. Current Limitations

**Type Inference:** Template arguments must be explicitly specified at instantiation sites (e.g., `add<int>(...)`). Inference is not yet supported.

**Specialization:** Only one definition is allowed per function identifier. For member functions on templated types, the restriction is one definition per type parameterization + function name combination.

```c
struct Vec<T> { /* ... */ };

// If this is declared:
void Vec<int>::some_function() { /* ... */ }
// This will produce an error:
void Vec<float>::some_function() { /* ... */ }
```

## 5. Control Flow

### 5.1. Match Expressions

CX provides a `match` expression as a safer alternative to the C `switch` statement.

*   **No Fallthrough:** Each arm is an independent, scoped block.
*   **Tagged Union Support:** Arms can deconstruct tagged union variants.

```c
void print_output(Output out) {
    match (out) {
        Output::integer(i) => printf("Integer: %d\n", i);
        Output::fp(d) => printf("Float: %f\n", d);
        Output::string(s) => printf("String: %s\n", s);
        default => printf("Unknown type\n");
    }
}
```

### 5.2. The `is` Operator

The `is` operator checks whether a tagged union instance matches a specific variant at runtime, returning a boolean. When the check succeeds, the variant's inner value is extracted into a new variable available within the conditional's scope.

```c
Output out = generate_output(2);

if (out is Output::string(s)) {
    printf("The output is a string: %s\n", s);
} else {
    printf("The output is not a string.\n");
}
```

**Warning:** The `is` operator performs an **unchecked coercion**. If used outside of a conditional check, accessing the extracted variable when the union is not of the expected variant results in **undefined behavior**. Tagged union variants are not yet safely deconstructed; destruction logic for contained variants is not automatically invoked.

## 6. Modules and Visibility

### 6.1. Importing Modules

The `import` keyword brings all public declarations from another module into the current file's scope. Imported declarations are implicitly private — they are not re-exported to modules that import the current one.

### 6.2. Visibility Control

Declaration visibility is controlled by `public:` and `private:` headers.

*   `public:` — declarations following this header are visible to importing modules.
*   `private:` — declarations following this header are local to the current module.

These headers can appear multiple times to toggle visibility. Declarations are `private` by default.

```
// my_library.cx
public:

struct PublicType {
    int value;
};

void public_function() { /* ... */ }

private:

void internal_helper() { /* ... */ }
```

## 7. Contracts — Pre/Post-Conditions

CX supports function contracts via `where` clauses on function declarations. Contracts specify preconditions and postconditions that constrain function behavior.

### Syntax

```
return_type function_name(params)
where
    pre: (expr),
    post(result_name): (expr)
{ ... }
```

*   **`pre: (expr)`** — a precondition evaluated before each call site. The expression must hold true for all callers.
*   **`post(result_name): (expr)`** — a postcondition evaluated after the function body. `result_name` binds the return value for use in the expression.
*   Multiple clauses are separated by commas.

### Runtime Behavior

In **debug builds**, precondition and postcondition violations abort execution at runtime. In **release builds**, the compiler treats contract expressions as optimization assumptions.

### Example

```c
#include <stdio.h>

int contract(int a, int b)
where
    pre: (a > 0 && b > 0),
    post(result): (result > a && result > b)
{
    return a + b;
}

int main() {
    int z = contract(5, 10);
    printf("Result: %d\n", z);
    return z;
}
```

## 8. Linear Types — `@nocopy` and `@nodrop`

CX supports linear type attributes on structs to enforce ownership discipline at compile time.

### `@nocopy`

A struct marked `@nocopy` cannot be implicitly copied. Values must be explicitly transferred via the `move` keyword. Assignment without `move` is a compile error.

```c
struct Data : @nocopy {
    int data;
};

void consume(Data data) {}

int good() {
    Data data = (Data) { .data = 1 };
    consume(move data);

    // After moving, `data` can be reinitialized:
    data = (Data) { .data = 2 };
    consume(move data);
    return 0;
}
```

### `@nodrop`

A struct marked `@nodrop` is a **linear type** — it implies `@nocopy` and additionally cannot silently fall out of scope. Every value of a `@nodrop` type must be explicitly consumed via a destructive move or leaked via `@leak(variable)`. `@nodestruct` is accepted as an alias for `@nodrop`.

```c
struct Resource : @nodrop {
    int data;
};
```

If a `@nodrop` value reaches the end of its scope without being consumed, the compiler emits an error:

```c
int bad() {
    Resource value;
    return 0; // ERROR: linear type `Resource` not consumed before scope exit.
}
```

### `@leak(variable)`

The `@leak` attribute explicitly suppresses the `@nodrop` obligation, marking the binding as consumed without transferring ownership.

### Control-Flow Awareness

The compiler tracks moves across all control-flow paths — branches, loops, `break`, and `continue` — to ensure linearity is satisfied on every possible execution path.

**Valid: consumed in both branches.**
```c
int good() {
    int cond = 0;
    Data data = (Data) { .data = 1 };

    if (cond) {
        consume(move data);
    } else {
        consume(move data);
    }
    return 0;
}
```

**Invalid: consumed in only one branch.**
```c
int bad() {
    int cond = 0;
    Data data = (Data) { .data = 1 };

    if (cond) {
        consume(move data);
    }
    // ERROR: `data` may not be consumed on the else path.
    return 0;
}
```

**Invalid: conditional `@leak` does not satisfy linearity.**
```c
int bad() {
    int cond = 0;
    Resource value = (Resource) { .data = 1 };

    if (cond) {
        @leak(value);
    }
    // ERROR: `value` may not be consumed on the else path.
    return 0;
}
```

## 9. Safe Functions and Formal Verification

CX provides a `safe` keyword for function declarations that enables compile-time formal verification.

### Syntax

```c
int fn() safe
where
    post(ret): (ret == 1)
{
    int x = 1;
    return x;
}
```

### Restrictions

Safe functions enforce the following constraints:

1.  **No raw pointers.** Parameters and local variables cannot have pointer types.

    ```c
    // ERROR: raw pointer parameter in safe function.
    int bad(int* ptr) safe { return 0; }

    // ERROR: raw pointer local in safe function.
    int bad() safe {
        int value = 1;
        int* ptr = &value;
        return value;
    }
    ```

2.  **Safe-only calls.** A safe function can only call other functions marked `safe`.

    ```c
    int helper() { return 1; }

    // ERROR: safe function calls non-safe function `helper`.
    int bad() safe { return helper(); }
    ```

3.  **Pure contracts.** Contract expressions in safe functions must be pure — they cannot contain function calls or side effects.

    ```c
    int helper() { return 1; }

    // ERROR: impure expression in contract of safe function.
    int bad() safe
    where
        post(ret): (helper() == ret)
    { return 1; }
    ```

### Formal Verification via FMIR

The compiler lowers safe functions to **FMIR (Functional MIR)**, a pure functional intermediate representation that models C memory operations as monadic state transformations. This enables compile-time formal verification of contracts.

When a contract is **tautologically violated** — the postcondition is provably false given the function body — the compiler emits an error at compile time rather than deferring to runtime:

```c
int fn() safe
where
    post(ret): (ret == 1)
{
    int x = 0;
    return x; // ERROR: postcondition `ret == 1` is provably violated.
}
```

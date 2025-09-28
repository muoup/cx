# CX Language Documentation

NOTE: No feature in this document is currently set in stone, code should not be written
at the moment under the assumption it will not break in the future. As well, given the
early stage of the language, this document will not be a complete specification of the
semantic guarantees of the language, but rather a general overview of features with examples.

## 1. Base Syntax

The syntax of CX is implemented with the intention of being a superset of at least C99 syntax.
In theory, all valid C99 code should be valid CX code, however at this stage supporting all of C99 
is not a priority, however can be added in the future if deemed necessary. Any divergences from
the semantics of the language, or much less likely syntax, will be noted in the document, but
all syntax available in the [C99 Standard](https://www.dii.uchile.cl/~daespino/files/Iso_C_1999_definition.pdf) should be available in CX.

A significant divergence from C/C++ is that CX is not order-dependent. Types and functions can be declared in any order, as the compiler resolves all declarations within a module before compilation begins. This means a function can be called by another function that appears earlier in the same source file without needing a forward declaration.

## 2. Lexical Conventions

### 2.1. Identifiers
As is the case in standard C, while the underscore character ('_') is allowed in identifiers,
identifiers beginning with an underscore are reserved for the implementation. When generating
code or intrinsic type definitions, the compiler may generate identifiers beginning with
at least one underscore, and thus may conflict with user-defined identifiers.

### 2.2. Keywords

The only exception to section #1 with regards to CX's backwards compatibility is in the usage
of additional keywords. Any programs using the following keywords may not compile in CX without
slight modifications. The following keywords are currently reserved in addition to the C99 keywords:

*   `import`, `defer`, `strong`, `weak`, `new`, `template`, `type`, `match`, `is`, `private`, `public`, `class`.


The meaning of these keywords will be explained in their respective sections.

## 3. Additional Type Semantics

### 3.1. Strong Pointers

CX introduces the concept of strong pointers. This can be thought of as an equivalent to C++'s
`std::unique_ptr`, and Rust's `Box<T>`, among similar constructs. A strong pointer type is
declared just as you would a pointer, but with the `strong` keyword prepending the asterisk
or its `[]` array declaration, and can be initialized using the `new` keyword.

Syntax example:
```c
// A strong pointer to an integer.
int strong* ptr = new int;

// An array of strong pointers to integers.
int strong[] ptrs = new int[10];
```

A strong pointer semantically may either be NULL or point to a valid object, or array of objects if declared as such. As well,
any strong pointer that points to a valid object must 'own' that memory, meaning only one strong pointer may point to a given part
of memory at any time. 

After a strong pointer or data containing a strong pointer goes out of scope, the compiler will ensure that the memory
is freed if and only if the strong pointer is not NULL. Any memory that the pointer owns will also be deconstructed likewise.
This means that in the case of a strong array pointer, all elements in the array with be deconstructed individually before
the memory is freed.

### 3.2. Structs

Structs in CX exist with the same guarantees for padding and alignment as in C, however some additional abstractions are
provided for common use patterns.

For instance, CX allows for the declaration of member functions for structured types. These functions do not themselves
allow for polymorphism, however they do allow for a more object-oriented conception of using structured data. Member
functions act as syntactic sugar for creating a method that acts on a struct type, to allow for alternative syntax
for a function which takes in a pointer to a struct. Note that while not currently implemented, the explicit `this`
parameter at the beginning of the function declaration is enforced, as its exclusion will be treated in the future
as a function inside the namespace of the struct, similar to how Rust handles its `self` keyword.

As well, structs may have destructors, which are called when a struct goes out of scope. When a struct either contains
a destructable object (e.g a strong pointer, or another struct with a deconstruction logic), or has a destructor defined,
the compiler must create implicit logic for deconstructing the struct as is required. Note that a pointer to a destructable
type has no special meaning, as the compiler cannot deduce that the type `owns` the memory it points to. As of right now,
a struct is always deconstructed when it goes out of scope, however in the future this may be subject to change where
structs which are `moved` are not deconstructed, creating a more clear ownership model over structs.

```c++
struct MyStruct {
    int x;
};

// A member function.
void MyStruct::print(this) {
    printf("x = %d\n", this->x);
}

// NOTE: Not currently supported, but since `this` is an explicit parameter,
// it can be marked as `const` to create a read-only method, similar to C++'s `const` methods.

// A destructor.
~MyStruct(this) {
    printf("MyStruct destructor\n");
}

// Example usage.
void func() {
    MyStruct s { .x = 10 };
    
    s.print(); // Calls the member function.
    
    // s goes out of scope here, thus a call to ~MyStruct(this) is inserted by the compiler. 
}
```

### 3.3. Tagged Unions

CX provides support for tagged unions, also known as sum types or variants, which are a powerful tool for representing a value that could be one of several types. They are declared using the `union class` keywords.

Each member of a `union class` is a "variant" with a distinct name and type, defined using the `::` separator.

**Syntax Example:**
```c
union class Output {
    integer :: int,
    fp      :: double,
    string  :: const char*,
    data    :: struct { int a; double b; char c; }
};
```

To create an instance of a tagged union, you use the `::` operator on the union type, followed by the variant name and the value to be stored.

**Construction Example:**
```c
Output o1 = Output::integer(42);
Output o2 = Output::fp(3.14);
Output o3 = Output::string("Hello, World!");
```

## 4. Templates

CX supports templates, which allow you to write generic functions and types that can work with any type. 
Templates are declared using the `template` keyword, followed by a list of type parameters enclosed in
angle brackets. Note that while the `type` keyword may seem redundant, future syntax will allow for
restrictions on the type parameters, requiring either a specific subset of types, or certain guarantees
about the type itself (e.g an integer being divisible by N, or certain methods being defined on a type).

The language currently cannot do type inference for templates, and uses C++'s model of copy and paste
instantiation. In the future, it may be required that the function is partially compilable before instantiation,
like as in Rust where templates can only assume the information that is guaranteed by the type bounds, but
for now this is not the case.

```c++
template <T1 : type, T2 : type, Ret : type>
Ret add(T1 a, T2 b) {
    return a + b;
}

int main() {
    int x = add<int, int, int>(1, 2); // x is 3
    float y = add<float, float, float>(1.0, 2.0); // y is 3.0
}
```

One other current limitation of the language is that it only supports one definition per type and function name,
see below. This is likely to be changed with a different ABI scheme.

```c++
template <T : type>
struct Vec {
    ...
};

Vec<int>::length() { ... }
Vec<float>::length() { ... } // invalid as Vec::length is already defined, templated types do not currently mangle their own name identifiers.
```

## 5. Control Flow

### 5.1. Match Expressions

CX includes a `match` expression, which is a semantically enhanced version of the C `switch` statement. It is designed to safely handle different cases of a value, such as the variants of a tagged union.

The key differences from a C `switch` are:
*   **No Fallthrough:** Each arm of a `match` expression is an independent, scoped block. There is no fallthrough behavior, which eliminates a common source of bugs.
*   **Tagged Union Support:** It can deconstruct tagged unions, allowing you to handle each variant in a separate arm.

Currently, `match` is primarily used for tagged unions. However, it is planned to support integer-like types in the future, making it a more general-purpose control flow tool.

**Syntax Example:**
```c
void print_output(Output out) {
    match (out) {
        Output::integer(i) => printf("Integer: %d\n", i);
        Output.fp(d) => printf("Float: %f\n", d);
        Output::string(s) => printf("String: %s\n", s);
        default => printf("Unknown type\n");
    }
}
```

### 5.2. The `is` Operator

The `is` operator is used to check if a tagged union instance corresponds to a specific variant at runtime. It returns a boolean `true` if the instance matches the variant and `false` otherwise.

Its primary use is within conditional statements like `if` and `while` to safely inspect a tagged union's current type.

When the `is` operator evaluates to true, it also extracts the variant's inner value into a new variable that is made available within the conditional's scope.

**Syntax Example:**
```c
Output out = generate_output(2);

if (out is Output::string(s)) {
    // 's' is now available here and holds the string value
    printf("The output is a string: %s\n", s);
} else {
    printf("The output is not a string.\n");
}
```

**Important Warning:** The `is` operator also performs an **unchecked coercion**. If used outside of a conditional check where the type is not guaranteed, it will still attempt to extract the value. Accessing this extracted variable when the tagged union instance is not of the expected variant results in **undefined behavior**. 
Currently also, tagged unions are not safely deconstructed, any deconstruction logic for a contained variant is not invoked automatically, but this will be changed in the future.

## 6. Modules and Visibility

CX includes a simple and powerful module system to help organize code.

### 6.1. Importing Modules

The `import` keyword brings all public declarations from another module into the current file's scope. Once imported, a module's public functions and types can be used directly. Note that any
imported information is implicitly privately declared, so no information imported will be available to any module that imports the current one, as one might expect with #include and a header file.

### 6.2. Visibility Control

The visibility of declarations within a module is controlled by `public:` and `private:` headers.

*   `public:`: All declarations following this header will be visible to other modules that `import` this one.
*   `private:`: All declarations following this header will be local to the current module and cannot be accessed from outside.

These headers can be used multiple times within a file to toggle the visibility of different sections of code. If no visibility header is specified, declarations are `private` by default.

```
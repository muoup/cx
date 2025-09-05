# CX Language Specification

## 1. Introduction

This document specifies the CX programming language. CX is a statically-typed, compiled language that is designed to be a modern alternative to C. It aims to provide the same level of performance and control as C, while also offering more modern features and a more expressive syntax.

## 2. Relationship to C

CX is heavily inspired by C and, in many cases, is a superset of C. Where this specification does not explicitly differ from the C11 standard, it should be assumed that CX follows the C11 standard.

## 3. Lexical Conventions

The lexical conventions of CX are the same as those of C, with the addition of a few new keywords.

### 3.1. Keywords

The following are the keywords in CX that are not also keywords in C:

*   `template`
*   `strong`
*   `weak`
*   `move`

## 4. Types

The CX type system is based on the C type system, but with a few key differences.

### 4.1. Strong Pointers

CX introduces the concept of strong pointers, which are a form of smart pointer that automatically manages the memory of the object they point to. Strong pointers are declared using the `strong` keyword.

```c
// A strong pointer to an integer.
int strong* ptr = new int;

// An array of strong pointers to integers.
int strong[] ptrs = new int[10];
```

A strong pointer semantically may either be NULL (0) or point to a valid object, or array of objects if declared as such. As well,
any strong pointer that points to a valid object must 'own' that memory, meaning only one strong pointer may point to a given part
of memory at any time. 

After a strong pointer goes out of scope, or a struct containing a strong pointer does so, the compiler generates code to automatically
check if a strong pointer is non-null, and if so, deallocates the memory it points to. If the strong pointer points to a struct (or array of structs) with
a defined destructor or that also contains strong pointers, said cleanup routine(s) is invoked first, and then the memory is deallocated.

### 4.2. Structs

Structs in CX are similar to structs in C, but they can also have member functions and destructors.

```c++
struct MyStruct {
    int x;
};

// A member function.
void MyStruct::print(this) {
    printf("x = %d\n", this->x);
}

// A destructor.
~MyStruct(this) {
    printf("MyStruct destructor\n");
}
```

## 5. Functions and Templates

### 5.1. Member Functions

As shown in the previous section, structs can have member functions. Member functions are called using the `.` operator, just like in C++.

```c
MyStruct s;
s.x = 10;
s.print(); // Prints "x = 10"
```

### 5.2. Templates

CX supports templates, which allow you to write generic functions and types that can work with any type. Templates are declared using the `template` keyword.

```c++
template <T : type>
T add(T a, T b) {
    return a + b;
}

int main() {
    int x = add<int>(1, 2); // x is 3
    float y = add<float>(1.0, 2.0); // y is 3.0
}
```

The language, however, as opposed to C++ only supports one definition per function name. The language strives for a more
stable and less complex ABI, however this specific decision may be revisited in the future. In the meantime, this in effect 
means that partial specialization is heavily kneecapped, as something such as:

```c++
template <T : type>
struct Vec {
    ...
};

Vec<int>::length() { ... }
Vec<float>::length() { ... } // invalid as Vec::length is already defined, templates do not currently mangle their own name identifiers.
```

## 6. Memory Management

### 6.1. `new` and `delete`

CX uses the `new` and `delete` keywords for manual memory management, just like in C++. For now, 'new' and 'delete' can only be used
with strong pointers, and act as wrappers around libc's `malloc` and `free` functions respectively. Arrays allocated with new store
the length of the array in the 8-bytes before the pointer returned by 'new'.

```c

### 6.2. `move`

The `move` keyword is used to transfer ownership of a strong pointer from one variable to another.

```c
int strong* ptr1 = new int;
int strong* ptr2 = move ptr1; // ptr2 now owns the memory, ptr1 is null.
```

## 7. Preprocessor

CX has a preprocessor that is similar to the C preprocessor. It supports the `#include` directive for including header files.

## 8. Standard Library

The CX standard library is still under development, but it aims to provide a rich set of functions and types for common tasks. The standard library is inspired by the C standard library and the C++ standard library.

```
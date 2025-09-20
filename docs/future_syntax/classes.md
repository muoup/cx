# IDEA: 'Classes' - Typeclasses w/ Guarantees

## Motivation

In a modernized C-like language, it would be useful to be able to strengthen the types used
to express more intent, allowing both for better optimizations and debugging capabilities by the
compiler. Roughly, there are three things type-based features I would like to implement:

- Polymorphism / First-class VTable Support
- Reduced Boilerplate for Types of a Similar Shape
- Refinement Types / Dependent Types

Note: Since this is not a standardized language, I am just using C++ syntax highlighting
for the code snippets below.

```c++

// 1. Class as Refinement Type
class even {
    constraint(this % 2 == 0);
}

// Note: an 'even [type]' object in memory is operationally equivalent to the type itself,
// the class wrapper only exists to communicate that any constraints are guaranteed to hold,
// as well as allowing the possibility for better debug messages in case 'int' at some point
// no longer supports coercion to 'even'.
void foo(even int x) { .. }

void test() {
    foo(3); // Runtime Error: x is not guaranteed to be even
    foo(4); // Succeeds
}

// 2. Class for Polymorphism
class printable {
    void print(this);
}

instance printable int {
    void print(this) { 
        printf("%d", this);
    }
}

// Note: a 'printable' class without specialization is stored as a wide pointer,
// i.e. two words consecutively in memory, the first to the data, and the second to a vtable
// containing function pointers to the methods of the class type
void print_twice(printable p) {
    // p.print() is dispatched dynamically, approximately (p.vtable['print'])(p.data)
    p.print();
    p.print();
}

// This semantically works very similar to point #1, where the resolved generic will use T
// just as it would the underlying type, however the class here works to restrict which
// classes T can be, as well as to ensure that the methods of the class are available.
template <T : printable>
void print_twice(T p) {
    // p.print() is dispatched statically, meaning it is a single direct call to the method
    p.print();
    p.print();
}

// 3. Class for Boilerplate Reduction
template <T : type>
class iterable : printable {
    T at(this, int index);
    
    void print(this) {
        printf("[");
        for (int i = 0; i < this.length; i++) {
            if (i > 0) {
                printf(", ");
            }
            printf("%d", this.at(i));
        }
        printf("]");
    }
    
    template <acc : type>
    void lfold(this, acc init, acc func(acc, T)) {
        acc result = init;
        for (int i = 0; i < this.length; i++) {
            result = func(result, this.at(i));
        }
        return result;
    }
    
    // ... other useful methods ...
}

template <T : type>
struct vec<T> {
    T* data;
    int length;
}

// ... dynamic array implementation ...

template <T : type>
instance iterator vec<T> {
    T at(this, int index) {
        return this.data[index];
    }
}

template <T : type, N : val int>
struct fixed_array<T, N> {
    T data[N];
    int length = N;
}

// ... fixed-size array implementation ...

template <T : type, N : val int>
instance iterator fixed_array<T, N> {
    T at(this, int index) {
        return this.data[index];
    }
}

void test() {
    vec<int> v = ...;
    fixed_array<int, 4> a = ...;
    
    print_twice(v); // prints the vector
    print_twice(a); // prints the fixed array
    
    int sum_v = v.lfold(0, (acc, int x) => acc + x);
    int sum_a = a.lfold(0, (acc, int x) => acc + x);
}
```
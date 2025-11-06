# Move Semantics vs C++

```c++

struct Data { .. };

void pass_by_value(Data d) { .. }
void pass_by_rvalue_ref(Data&& d) { .. }

void caller() {
    Data data;
    pass_by_value(data);
    pass_by_rvalue_ref(std::move(data));
}

```

C semantic equivalent:

```c

struct Data { .. };

void pass_by_value(struct Data* d) {
    // Data is not stored in callee's stack space, it operates on a *copied* version
    // of the object passed by the caller, copied by the caller.
}

void pass_by_rvalue_ref(struct Data* d) {
    // Data is not stored in callee's stack space here either, and because the parameter
    // is treated as an rvalue reference, we can simply use the pointer to the original object.
}

void caller() {
    struct Data data;
    
    struct Data data_copy = __data_copy_constructor(data);
    pass_by_value(&data_copy);
    pass_by_rvalue_ref(&data);
}

```

Alternative semantics:
 - Treat struct values like C++'s rvalue references, however change coercion rules:
 - A reference to a local object is a reference to that object (like C++ lvalue reference), so
   when coercing this reference to a value:
     - A copy of the object is made when the object is trivially-copyable (C structs / POD types)
     - An *explicit* move operation is specified to tell the compiler to simply use the original object
       and consider it dead after the move.
     - An *explicit* clone operation is called to create a deep copy of the object when needed.

For example (using CX syntax):

```c++

struct Data { 
    ..
};

Data Data::clone() const { .. } // deep copy

void pass_by_value(Data d) { .. }

void caller() {
    Data data;
    
    // deep copy, leave data unmodified
    pass_by_value(data.clone());
    
    // 'data' is considered dead after being moved 
    // Even if pass_by_value does not move it elsewhere, it will invoke the destructor on data
    // and thus data cannot be used after this point.
    // Even if pass_by_value returns data back to us as well, and thus the object is still alive,
    // the original 'data' alias is dead and is UB to use after the move. 
    pass_by_value(move data);     
}
```

C semantic equivalent:

```c

struct Data { .. };

struct Data _data_clone(const struct Data* self) { .. }
struct Data _data_destructor(struct Data* self) { .. }

void pass_by_value(struct Data* d) {
    // Like in the C++ examples, 'd' is still not stored in callee's stack space,
    // and thus pass_by_value is free to use the d pointer directly to the caller's
    // object.
    
    _data_destructor(&d);
}

void caller() {
    struct Data data;
    
    // pass_by_value(data.clone());
    struct Data data_clone = _data_clone(&data);
    pass_by_value(&data_clone);
    
    // pass_by_value(move data);
    pass_by_value(&data);
}
```

## Considerations:

In C++, moving an object does not invalidate the original object; it simply transfers ownership of any resources.
C++'s std::vector for instance only nullifies its internal pointers when moved from, but at lifetime end its
destructor is still invoked. 

If we want to remove this behavior and make moved-from objects completely dead, we need to consider how a compiler
should determine when an object is moved-from. Either, the compiler should determine this as *compile-time* or use
some runtime flag attached to the object to determine if it has been moved-from. To prevent confusing ABIs between
C and CX, or even hidden allocations and other runtime behavior against the spirit of C, the former is
heavily preferred.

The compile-time approach however has its limitations. Given the undecidability of program flow, the use-cases of
moving an object must be heavily limited.

### 1. Conditional Moving

Consider the following code:

```c++

struct Data { .. }; // Struct with non-trivial move semantics

void processing(Data d) { .. }

void process_data(Data d, bool condition) {
    if (condition) {
        processing(move d);
    }
    
    // otherwise do something else non-moving here
    // ...
    
    // Q: The compiler need's to invoke the destructor on 'd' here if it is still live. How can it know this?
    // It is trivial here, given the simple control flow, but what about more complex cases?
}

```

The approach to this problem can draw heavily from the semantics of Rust's ownership model implementation. The compiler can generate IR similar to the following:

```

void processing(ptr d, bool condition) {
  %d_liveness = alloca bool   
  store bool true, %d_liveness
  
  // processing logic here
  br condition, label %cond_true, label %merge
    
.cond_true:
  store bool false, %d_liveness
  call void processing(d)
  jmp .merge

.merge:
  // other logic here
  // ...
  
  %is_live = load bool, %d_liveness
  br %is_live, label %destruct_d, label %end
    
.destruct_d:
  call void _data_destructor(d)
  jmp .end
    
.end:
  ret void
}

```

While this may seem like a naive approach, it produces a traceable control-flow that the backend (i.e. LLVM) can
optimize into what the more obvious approach is here to simplify the control-flow and avoid the second branch
entirely.

### 2. Moving References

The previous example only considered moving local objects. What about dealing with references?

```c++

struct Data { .. };

void processing(vector<Data>* data_array) {
    for (Data& d_ref : data_array) {
        if (some_condition(d_ref)) {
            // Q: Can we move d_ref here?
            processing(move d_ref);
        }
    }
    
    // ...

    // Here, the compiler is not responsible for destroying the objects in data_array as it's lifetime
    // extends beyond this function scope, however when it comes time for the vector here to be destroyed,
    // how does the compiler know which objects were moved-from and which weren't?
}

```

This presents a bit more complex of a solution, that would certainly require some kind of runtime tracking
overhead of moved-from state. This goes against the point before of avoiding hidden runtime behavior, so
we could simply disallow moving non-local objects entirely. However this limits the usefulness of move semantics.
There is a third option however, which is to allow for opt-in moved-from tracking on certain types.

For example, consider the following CX code:

```c++

struct Data {
    bool _moved;
    ..
};

move::Data {
    move(this) {
        this->moved = true;
    };
    
    bool is_moved() const {
        return this->_moved;
    };
}

```

This syntax is purely illustrative, but the idea is to allow for opt-in moved-from tracking on certain types.
You could even imagine a moved-from implementation on an Option<T> type, or a special Move<T> wrapper type to
allow for a simpler streamlined approach to moving references.
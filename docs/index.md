# The CX Programming Language

&nbsp;

The CX Programming Language is a work-in-progress language designed to be a superset to the C99 standard with a simple template and metaprogramming syntax suite and a linear type system to power opt-in type-safety, correctness, and formal verification while still upholding C's explicit nature and no hidden functionality philosophy. 

CX as a name may be subject to change, but has been so for quite some time and may have found its stay. The 'X' started as a placeholder to indicate a future name should be some word or wordplay beginning with C, but the name can also be conceptualized as 'C-Xtended' if you so please.

### If You Are a Passerby,

you don't care for fancy buzzwords like 'linear typing' and 'type safety' and want a quick overview of the language to see what makes its approach different from the other C-like languages nowadays like [Zig](https://ziglang.org) and [C3](https://c3-lang.org), consider this very simple program:

```c++

import std::box;
import std::io;

struct SomeData @nodrop { ... }

int main() {
    // Say SomeData is some large struct of data we are going to use later in the project
    box<SomeData> data = box::new(SomeData { ... });
    
    if (argc == 1) {
        println("Usage: myprogram [file]");
        
        // In a language like Rust or C++, right here would be an implicit call to box<SomeData>'s destructor, but in a more C-style
        // language like CX we don't want any function calls, so uh oh, we just leaked memory on accident!
        
        // Luckily for us however, we don't leak memory here because this program doesn't compile. Since we marked `SomeData` as
        // @nodrop, the compiler is there to keep us from leaking an object without explicitly deciding what we want to do with it.
        
        // Of course, in this case it's no big deal, the OS will clean up our leaked memory for us, but imagine if this wasn't
        // in main. Imagine if this was in your hot path, a place of sprawling control flow where you need full control over every 
        // instruction executed. It would be nice to know both where your data is being cleaned up, and that nothing is being leaked, right?
        return 1;
    }
    
    // Good path logic
    data.top_secret_information = ...;
}

```
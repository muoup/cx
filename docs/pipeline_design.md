# Pipeline Design

## Stages

### Stage 1: Lexing
This is the first and most basic stage of compilation, converting the raw text of a file into a stream of tokens.

- **Input**: Raw text of the file.
- **Output**: A list of lexemes/tokens.

### Stage 2: Pre-parsing
This stage consists of parsing all type declarations and function signatures, while skipping over any function implementations.
While it may not be immediately obvious why this is necessary to do as a separate stage, consider seeing an expression
like so in a function body:
```c
a * b (...);
```
This can be parsed both as a valid variable declaration (a* b) and a multiplication expression (a * b). The only way
for the parser to know which one it is, is to be able to determine if a is a type and not a variable. As such, in the
example code below, trying to parse the whole code in one pass is impossible due to the ambiguity of the expression.
```c
int main() {
    a * b;
}

typedef int a;
```
Given this, along with the additional complexity of using modules, where types may be declared in multiple compilation
units, the decided approach to solve this is to parse all type declarations and function signatures as its own pipeline
stage. During this stage, all imports are also collected, for use by the parser.

- **Prerequisites**: The source code must be lexed and tokenized before this stage can be performed.
- **Input**: List of lexemes/tokens.
- **Output**: A list of type declarations and function signatures, including templates, and a list of imports.

### Stage 3: Import Combining
This stage is responsible for combining the pre-parsed data from all imported modules. This is necessary to make
all type and function declarations from imported modules available to the parser in the next stage.

- **Prerequisites**: The pre-parsing stage for the compilation unit and all of its imports must be complete.
- **Input**: A list of pre-parsed data from the compilation unit and all of its imports.
- **Output**: A single combined data structure containing all type and function declarations.

### Stage 4: Parsing
This stage is the main parsing stage, where the actual syntax tree is built. It uses the declared types and
function signatures from the previous stage to parse the function bodies. The actual design of the AST will
be annotated separately (TBD). Prior to the actual parsing, one additional step is performed to ensure that
all relevant types and function signatures from imports are available to the parser. The implementation of
this is subject to change, currently all type data is given to the parser, however in cases of type declarations
spanning over multiple nested imports may allow for truncating type trees to allow for "extern" opaque types.

- **Prerequisites**: The import combining stage for the compilation unit must be complete.
- **Input**: List of type declarations, function signatures, and imports.
- **Output**: An abstract syntax tree (AST) representing the program.

#### Handling Templates
Because this stage is mostly type-agnostic besides knowing if type symbols exist, templates are all but ignored
during this stage. When a templated function is encountered, it is parsed by adding the type symbols to the
type map as if they are defined with real type information, and as this stage does not concern itself with
said information, it works just as parsing a normal function.

### Stage 5: Type Checking and Template Realization
This stage is responsible for checking the types of expressions and statements in the AST. It ensures that
no type errors exist in the program, as well as adding implicit casts and conversions where necessary to
ensure the guarantees of C semantics are met.

Additionally, this stage is also responsible for handling templated types and functions. While an expression is
being typechecked, if a templated reference not previously realized is encountered, in the case of a templated type,
it is simply instantiated and added to the type map, however in the case of a function, the function prototype is
instantiated in the same manner, however the type checker also records a request to typecheck the function body under
this template input later, where typechecking as a stage can only end once all requests have been fulfilled and all
functions have been typechecked. Because the idiomatic multi-file structure of CX is to use modules, the
'request to pipeline' system also comes with the advantage that redundant instantiations between compilation units can
be avoided when possible.

- **Prerequisites**: The AST must be built before this stage can be performed. ASTs for all imports must also
    be available to the type checker.
- **Input**: AST from the parsing stage, type and function signature information.
- **Output**: A modified AST containing additional implicit AST elements, type information for each node, and realized template functions and types.

### Stage 6: MIR Generation
In the code, MIR will often be referred to as `Bytecode` as was it's old moniker, however it is more
accurately described as a flat SSA MIR for the frontend to use before handing off to the supported
codegen backends, currently LLVM and Cranelift. This IR does not currently come with its own special
optimizations, however it is slowly being redesigned for such. While backends typically do their own
optimizations, as MIR is more high-level and aware of the semantics of the language, it has the ability
to be aware of safe transformations at a higher level than the backends.

- **Prerequisites**: The type-checked and template-realized AST.
- **Input**: Type-checked and template-realized AST. Type map, and function signature map.
- **Output**: A flat SSA IR representation of the program, along with a converted type map and function signature to match the lower-level representation.

### Stage 7: Backend Code Generation
This stage is responsible for converting the flat SSA IR to a target-specific representation. This is
usually a fairly straightforward process, as the IR is designed to be easily ported to different backends.

- **Prerequisites**: The bytecode of only the compilation unit, and the target architecture information.
- **Input**: Flat SSA IR representation of the program.
- **Output**: Object code or assembly code for the target architecture.

### Stage 8: Linking
After all is said and done, the final stage links all generated object files into a single executable.
See GCC for more information on the linking process, as it is fairly standard and does not require
any special handling for this language.

- **Prerequisites**: All object files generated from the previous stage.
- **Input**: Object files from the backend code generation stage.
- **Output**: A single executable file containing the linked program.

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

### Stage 5: Type Checking and Template Realization
This stage performs a type checking pass on the contextless AST produced by the parser. It is responsible both for completing
types and templated types -- i.e. flattening identifiers to their full type information, including across module boundaries, as well as converting
the initially parsed expressions inside of function bodies to properly typed MIR nodes which have resolved expression types as
well as applied implicit coercions where necessary. Any expressions which involved templates are realized as well at this stage,
the MIR stage currently uses a C++-style copy-and-paste template system, so a templated function must be instantiated and fully
typechecked each time a unique set of template parameters are used.

Any type errors will cause this stage to fail and report the cause of the fail. This stage is the last failable stage, any errors that
occur in later stages are considered internal compiler errors and should be reported as such.

- **Prerequisites**: The AST must be built before this stage can be performed. ASTs for all imports must also
    be available to the type checker.
- **Input**: AST from the parsing stage, type stumps and function signature information.
- **Output**: MIR, "Mid-Level Intermediate Representation", a typed AST-style IR with resolved types and fleshed out expression semantics.

### Stage 5.5: Safe Function Analysis (Optional — FMIR)
This stage runs only for functions marked `safe`. It lowers MIR to **FMIR (Functional MIR)**, a pure functional intermediate representation that models C memory operations as monadic state transformations. The FMIR enables compile-time formal verification to throw compile-time errors for tautological invariant violations caused by common logic errors.

This stage is implemented by `cx-safe-analyzer` operating on the `cx-safe-ir` data structures.

- **Prerequisites**: Type-checked MIR for safe functions, function contracts.
- **Input**: MIR for functions marked `safe`, along with their `where` clause contracts.
- **Output**: Verification diagnostics (errors if contracts are provably violated). Functions that pass verification proceed unchanged to LMIR generation.

### Stage 6: LMIR Generation
This stage converts the type-checked AST into a Lower-level MIR (LMIR), which is a flat SSA (Static
Single Assignment) intermediate representation. The LMIR serves as the final frontend IR before
handing off to the supported codegen backends (currently LLVM and Cranelift). The LMIR is
designed to be easily portable across different backends while maintaining language semantics.

The compiler internally distinguishes between two MIR levels:
- **MIR (Middle-level IR)**: Type-checked AST with semantic information, produced during type checking
- **LMIR (Lower-level MIR)**: Flat SSA representation produced from MIR, optimized for backend codegen

- **Prerequisites**: The type-checked and template-realized AST.
- **Input**: Type-checked and template-realized AST. Type map, and function signature map.
- **Output**: A flat SSA IR representation of the program (LMIR), along with converted type and function signature maps to match the lower-level representation.

### Stage 7: Backend Code Generation
This stage is responsible for converting the LMIR to a target-specific representation. This is
usually a fairly straightforward process, as the LMIR is designed to be easily ported to different backends.

- **Prerequisites**: The LMIR of only the compilation unit, and the target architecture information.
- **Input**: Flat SSA IR representation of the program (LMIR).
- **Output**: Object code or assembly code for the target architecture.

### Stage 8: Linking
After all is said and done, the final stage links all generated object files into a single executable.
See GCC for more information on the linking process, as it is fairly standard and does not require
any special handling for this language.

- **Prerequisites**: All object files generated from the previous stage.
- **Input**: Object files from the backend code generation stage.
- **Output**: A single executable file containing the linked program.

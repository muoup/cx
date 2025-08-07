# Pipeline Design

## Stages

### Stage 1: Preprocessing / Lexing
In the vein of traditional C parsing, preparsing and lexing are treated as trivial
in the pipeline, both can be done rather quickly and so are conceptualized as a single stage.

- **Input**: Raw text of the file with or without C preprocessor directives.
- **Output**: A list of lexemes/tokens.

### Stage 2: Preparsing
This stage is not necessary when parsing a language like C or C++ given its restrictions regarding
declaration order, however since this language strives to be less restrictive in that regard. This stage
consists of parsing all type declarations and function signatures, while skipping over any function implementations.
While it may not be immediately obvious why this is necessary to do as a separate stage, consider seeing an expression
like so in a function body:
```c
a * b (...)
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

### Stage 3: Parsing
This stage is the main parsing stage, where the actual syntax tree is built. It uses the declared types and
function signatures from the previous stage to parse the function bodies. The actual design of the AST will
be annotated separately (TBD). Prior to the actual parsing, one additional step is performed to ensure that
all relevant types and function signatures from imports are available to the parser. The implementation of
this is subject to change, currently all type data is given to the parser, however in cases of type declarations
spanning over multiple nested imports may allow for truncating type trees to allow for "extern" opaque types.

- **Prerequisites**: The preparsing stage for the compilation unit, along with all of the imports must be available
    before this stage can be performed.
- **Input**: List of type declarations, function signatures, and imports.
- **Output**: An abstract syntax tree (AST) representing the program.

#### Handling Templates
Because this stage is mostly type-agnostic besides knowing if type symbols exist, templates are all but ignored
during this stage. When a templated function is encountered, it is parsed by adding the type symbols to the
type map as if they are defined with real type information, and as this stage does not concern itself with
said information, it works just as parsing a normal function.

### Stage 4: Type Checking
This stage is responsible for checking the types of expressions and statements in the AST. It ensures that
no type errors exist in the program, as well as adding implicit casts and conversions where necessary to
ensure the guarantees of C semantics are met. The design of the type system, similar to the AST, will be
annotated separately (TBD).

- **Prerequisites**: The AST must be built before this stage can be performed. ASTs for all imports must also
    be available to the type checker, as template implementations are resolved during this stage (see below).
- **Input**: AST from the parsing stage, type and function signature information.
- **Output**: A modified AST containing additional implicit AST elements, and type information for each node.

#### Handling Templates: Functions
While any function is being type-checked, the implementation details of other functions are not of concern
to the type checker, what matters is the prototype of the function. Therefore, when a templated function
is referenced in the type checking stage, the type checker will create a request to be processed afterwards
to generate the function implementation. For now, this consists of simply cloning the function body and
adding it with a mangled name to the AST.

#### Handling Templates: Types


### Stage 5: Bytecode Generation
In its current form, the "bytecode" is implemented as an SSA flat IR that can be converted to different
codegen backends, which for now includes LLVM and Cranelift. In the future this will be re-implemented,
likely during self-hosting, to use a more TAC-like representation that could be used for interpretation
rather than just compilation, where compilation will happen during execution of the program to allow for
better debugging.

- **Prerequisites**: The type-checked AST of only the compilation unit.
- **Input**: Type-checked AST. Type map, and function signature map.
- **Output**: A flat SSA IR representation of the program, along with a converted type map and function signature to match the lower-level representation.

#### Additional Considerations

### Stage 6: Backend Code Generation
This stage is responsible for converting the flat SSA IR to a target-specific representation. This is
usually a fairly straightforward process, as the IR is designed to be easily ported to different backends.

- **Prerequisites**: The bytecode of only the compilation unit, and the target architecture information.
- **Input**: Flat SSA IR representation of the program.
- **Output**: Object code or assembly code for the target architecture.


### Stage 7: Linking
After all is said and done, the final stage links all generated object files into a single executable.
See GCC for more information on the linking process, as it is fairly standard and does not require
any special handling for this language.

- **Prerequisites**: All object files generated from the previous stage.
- **Input**: Object files from the backend code generation stage.
- **Output**: A single executable file containing the linked program.
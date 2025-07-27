use crate::CompilationUnit;

pub struct CompilationJob {
    pub requirements: Vec<CompilationStep>,
    pub step: CompilationStep,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompilationStep {
    // Note: for the sake of simplicity and the relatively minimal computational complexity,
    // preprocessing and lexing are done for now whenever the source code is needed.

    /**
     *  Determine the special type identifiers for the compilation unit. This step should be
     *  relatively short, but even for trying to determining what is a function for the next step,
     *  the C grammar requires that you are aware of the names of types that are defined in the
     *  compilation unit. This stage will collect the names of types as well as the direct imports
     *  of the file.
     *
     *  Requires: The source code of the compilation unit.
     *  Outputs:  A preparse map containing type identifiers and the imports of the compilation unit.
     */
    PreParse1(CompilationUnit),
    
    /**
     *  The second step of the pre-parsing phase, which is responsible for determining the names of
     *  functions in the compilation unit. This is mostly only needed for templates, as expressions
     *  when parsing the AST will need to differentiate between val < ... and val<... (..>) where the
     *  latter is a templated function call and the former is a binary operator.
     * 
     *  Requires: PreParseTypes identifiers to be available.
     *  Outputs:  An updated preparse map containing function names as well.
     */
    PreParseFunctions(CompilationUnit),

    /**
     *  Parse the AST from the source code. This is the main parsing step that converts the source
     *  code into an abstract syntax tree (AST) representation. In the process, a type map and
     *  function map is also created to be used later for typechecking purposes.
     *
     *  Requires: CX identifier information for each import and itself.
     *  Outputs:  An AST, a .cx-types file and a .cx-fns file containing the type map and function map.
     */
    ASTParse(CompilationUnit),

    /**
     *  Typecheck the AST. This step validates expressions, statements, and declarations, as well
     *  as adding implicit type conversions and checking for type errors.
     *
     *  Requires: An AST, along with type and function definitions of imports and the current unit.
     *  Outputs:  A type-checked AST.
     */
    TypeCheck(CompilationUnit),

    /**
     *  Generates a custom bytecode / Flat IR representation from the type-checked AST. This, unlike
     *  most codegen backends contains support for higher-level constructs such as deferred logic,
     *  special function types, and templates.
     *
     *  Requires: A type-checked AST. Along with the type and function definitions of imports and self.
     *  Outputs:  A bytecode representation of the type-checked AST along with a .cx-impl file containing
     *            implementations of templates, and potentially small always-inline functions in the future,
     *            to allow for more context-aware (and necessary in the case of templates) code generation.
     */
    BytecodeGen(CompilationUnit),

    /**
     *  Generate object files from the bytecode representation. Because templated and non-templated functions
     *  are handled separately in this IR, this step will also require access to the .cx-impl files of the
     *  imports and the current unit to ensure that all required implementations are available in the binary.
     *
     *  Requires: Bytecode representation of the type-checked AST, along with the .cx-impl files of imports
     *            and the current unit.
     */
    Codegen(CompilationUnit)
}

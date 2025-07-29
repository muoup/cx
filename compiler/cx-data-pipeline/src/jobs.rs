use std::collections::HashMap;
use std::hash::Hash;
use std::ops::{BitAnd, BitOr};
use uuid::Uuid;
use crate::CompilationUnit;

pub struct JobProgressMap {
    data: HashMap<CompilationUnit, CompilationStepRepr>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilationJob {
    pub uuid: Uuid,
    pub requirements: Vec<CompilationJobRequirement>,
    pub step: CompilationStep,
    pub unit: CompilationUnit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilationJobRequirement {
    pub step: CompilationStep,
    pub unit: CompilationUnit
}

pub type CompilationStepRepr = u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum CompilationStep {
    // Note: for the sake of simplicity and the relatively minimal computational complexity,
    // preprocessing and lexing are done for now whenever the source code is needed.

    /**
     *  Determine the special type identifiers for the compilation unit. This step should be
     *  relatively short, but even for trying to determine what is a function for the next step,
     *  the C grammar requires that you are aware of the names of types that are defined in the
     *  compilation unit. This stage will collect the names of types as well as the direct imports
     *  of the file.
     *
     *  Requires: The source code of the compilation unit.
     *  Outputs:  A preparse map containing type identifiers and the imports of the compilation unit.
     */
    PreParseStage1 = 1 << 0,
    
    /**
     *  The second step of the pre-parsing phase, which is responsible for determining the names of
     *  functions in the compilation unit. This is mostly only needed for templates, as expressions
     *  when parsing the AST will need to differentiate between val < ... and val<... (..>) where the
     *  latter is a templated function call and the former is a binary operator.
     * 
     *  Requires: PreParseTypes identifiers to be available.
     *
     *  Outputs:  An updated preparse map containing function names as well.
     */
    PreParseStage2 = 1 << 1,

    /**
     *  Parse the AST from the source code. This is the main parsing step that converts the source
     *  code into an abstract syntax tree (AST) representation. In the process, a type map and
     *  function map is also created to be used later for typechecking purposes.
     *
     *  Requires: CX identifier information for each import and itself.
     *
     *  Outputs:  An AST, a .cx-types file and a .cx-fns file containing the type map and function map.
     */
    ASTParse = 1 << 2,

    /**
     *  Typecheck the AST. This step validates expressions, statements, and declarations, as well
     *  as adding implicit type conversions and checking for type errors.
     *
     *  Requires: An AST, along with type and function definitions of imports and the current unit.
     *
     *  Outputs:  A type-checked AST.
     */
    TypeCheck = 1 << 3,

    /**
     *  Generates a custom bytecode / Flat IR representation from the type-checked AST. This, unlike
     *  most codegen backends contains support for higher-level constructs such as deferred logic,
     *  special function types, and templates.
     *
     *  Requires: A type-checked AST. Along with the type and function definitions of imports and self.
     *
     *  Outputs:  A bytecode representation of the type-checked AST along with a .cx-impl file containing
     *            implementations of templates, and potentially small always-inline functions in the future,
     *            to allow for more context-aware (and necessary in the case of templates) code generation.
     */
    BytecodeGen = 1 << 4,

    /**
     *  Generate object files from the bytecode representation. Because templated and non-templated functions
     *  are handled separately in this IR, this step will also require access to the .cx-impl files of the
     *  imports and the current unit to ensure that all required implementations are available in the binary.
     *
     *  Requires: Bytecode representation of the type-checked AST, along with the .cx-impl files of imports
     *            and the current unit.
     *
     *  Outputs:  One object file per compilation unit, containing the compiled code for the unit.
     */
    Codegen = 1 << 5
}

impl CompilationJob {
    pub fn new(
        requirements: Vec<CompilationJobRequirement>,
        step: CompilationStep,
        unit: CompilationUnit
    ) -> Self {
        CompilationJob {
            uuid: Uuid::new_v4(),
            requirements,
            step,
            unit
        }
    }
}

impl JobProgressMap {
    pub fn new() -> Self {
        JobProgressMap {
            data: HashMap::new()
        }
    }

    pub fn insert_progress(
        &mut self,
        unit: CompilationUnit,
        step: CompilationStep
    ) -> Option<()> {
        let entry = self.data.entry(unit).or_insert(0);
        *entry = *entry | (step as CompilationStepRepr);
        Some(())
    }

    pub fn get_progress(
        &self,
        unit: &CompilationUnit,
        step: CompilationStep
    ) -> bool {
        let Some(entry) = self.data.get(unit) else {
            return false;
        };

        (entry & (step as CompilationStepRepr)) != 0
    }

    pub fn get_job_progress(
        &self,
        job: &CompilationJob
    ) -> bool {
        self.get_progress(&job.unit, job.step)
    }
}
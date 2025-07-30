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
     *  Parses all type and function definitions from the source compilation unit. This will produce
     *  a (most of the time) incomplete type and function map, which can contain unresolved references
     *  to types and functions that are defined in imported modules. This must be done separate from
     *  the main parsing step as the C grammar necessitates that type symbols are known before parsing
     *  expressions inside a function body (e.g. a * b is an ambiguous multiplication or pointer
     *  variable declaration unless the compiler knows if "a" is a type)
     *
     *  Requires: Lexed and preprocessed source code. This is done when-needed for now as it is not
     *            too computationally expensive.
     *  
     */
    PreParse = 1 << 0,
    
    /**
     *  Parse the AST from the source code. This is the main parsing step that converts the source
     *  code into an abstract syntax tree (AST) representation. In the process, a type map and
     *  function map is also created to be used later for typechecking purposes.
     *
     *  Requires: CX type and function definitions from the preparse step for both the current
     *            compilation unit and all imports, as well as the lexed and preprocessed source code.
     *            
     *
     *  Outputs:  A naively parsed AST.
     */
    ASTParse = 1 << 1,

    /**
     *  Typecheck the AST. This step validates expressions, statements, and declarations, as well
     *  as adding implicit type conversions and checking for type errors.
     *
     *  Requires: The AST, along with the type and function definitions of imports and self.
     *
     *  Outputs:  A type-checked AST.
     */
    TypeCheck = 1 << 2,

    /**
     *  Generates a custom bytecode / Flat IR representation from the type-checked AST. This, unlike
     *  most codegen backends contains support for higher-level constructs such as deferred logic,
     *  special function types, and templates.
     *
     *  Requires: A type-checked AST. Along with the type and function definitions of imports and self.
     *
     *  Outputs:  A bytecode representation of the type-checked AST along with publicly accessible
     *            implementations of templated functions, types, and potentially in the future small
     *            always-inlined functions.
     */
    BytecodeGen = 1 << 3,

    /**
     *  Compiles the full compilation units from the flat IR bytecode representation. In effect, this
     *  will consist of combining the bytecode of the current compilation unit along with all needed
     *  implementations of templates and types from itself and its imports.
     *
     *  Requires: Bytecode representation of the type-checked AST, along with the .cx-impl files of imports
     *            and the current unit.
     *
     *  Outputs:  One object file per compilation unit, containing the compiled code for the unit.
     */
    Codegen = 1 << 4
    
    // For now, linking is a single step that is done after all compilation above is done. This 
    // could be abstracted into a CompilationStep, but seeing as it is not specified to a single compilation
    // unit, it is just handled separately for now.
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

    pub fn step_complete(
        &self,
        unit: &CompilationUnit,
        step: CompilationStep
    ) -> bool {
        let Some(entry) = self.data.get(unit) else {
            return false;
        };

        (entry & (step as CompilationStepRepr)) != 0
    }

    pub fn job_complete(
        &self,
        job: &CompilationJob
    ) -> bool {
        self.step_complete(&job.unit, job.step)
    }
}
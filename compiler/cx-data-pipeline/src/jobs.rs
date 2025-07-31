use crate::CompilationUnit;
use std::cmp::PartialEq;
use std::collections::{HashMap, VecDeque};
use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum JobState {
    InQueue,
    Completed
}

pub struct JobQueue {
    progress_map: HashMap<(CompilationUnit, CompilationStep), JobState>,
    data: VecDeque<CompilationJob>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilationJob {
    pub step: CompilationStep,
    pub unit: CompilationUnit,
    pub requirements: Vec<CompilationJobRequirement>,
    
    pub compilation_exists: bool
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
            requirements, step, unit,
            
            compilation_exists: false
        }
    }
    
    pub fn as_requirement(&self) -> CompilationJobRequirement {
        CompilationJobRequirement {
            step: self.step,
            unit: self.unit.clone()
        }
    }
}

impl Hash for CompilationJob {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.unit.hash(state);
        self.step.hash(state);
    }
}

impl JobQueue {
    pub fn new() -> Self {
        JobQueue {
            progress_map: HashMap::new(),
            data: VecDeque::new()
        }
    }

    pub fn push_new_job(&mut self, job: CompilationJob) {
        let pair = (job.unit.clone(), job.step);
        
        if !self.progress_map.contains_key(&pair) {
            self.data.push_back(job);
            self.progress_map.insert(pair, JobState::InQueue);
        }
    }
    
    pub fn push_job(&mut self, job: CompilationJob) {
        let pair = (job.unit.clone(), job.step);
        
        self.data.push_back(job);
        self.progress_map.insert(pair, JobState::InQueue);
    }

    pub fn pop_job(&mut self) -> Option<CompilationJob> {
        self.data.pop_front()
    }
    
    pub fn complete_job(&mut self, job: &CompilationJob) {
        self.progress_map.insert((job.unit.clone(), job.step), JobState::Completed);
    }
    
    pub fn complete_all_unit_jobs(&mut self, unit: &CompilationUnit) {
        self.progress_map.insert((unit.clone(), CompilationStep::PreParse), JobState::Completed);
        self.progress_map.insert((unit.clone(), CompilationStep::ASTParse), JobState::Completed);
        self.progress_map.insert((unit.clone(), CompilationStep::TypeCheck), JobState::Completed);
        self.progress_map.insert((unit.clone(), CompilationStep::BytecodeGen), JobState::Completed);
        self.progress_map.insert((unit.clone(), CompilationStep::Codegen), JobState::Completed);
    }
    
    pub fn job_complete(&self, job: &CompilationJob) -> bool {
        self.progress_map.get(&(job.unit.clone(), job.step)) == Some(&JobState::Completed)
    }
    
    pub fn requirements_complete(&self, job: &CompilationJob) -> bool {
        job.requirements.iter().all(|req| {
            self.progress_map.get(&(req.unit.clone(), req.step)) == Some(&JobState::Completed)
        })
    }
    
    pub fn finish_job(&mut self, job: &CompilationJob) {
        self.progress_map.insert((job.unit.clone(), job.step), JobState::Completed);
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}
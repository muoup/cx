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
     *  A relatively minor step, in order for expressions to be parsed correctly, the parser must
     *  know all declared type names, and function names as well in the case of templates. This
     *  means that AST parsing, since "import" does not create a unified compilation unit, must
     *  be preceded by a step that combines the public interfaces of all imports into the current
     *  compilation unit's preparse data.
     *
     *  Requires: The preparse data of the current compilation unit and its imports.
     *
     *  Outputs:  A slightly modified preparse data that contains the public interfaces of imports
     */
    ImportCombine = 1 << 1,

    /**
     *  Parse the AST from the source code. This is the main parsing step that converts the source
     *  code into an abstract syntax tree (AST) representation. In the process, a type map and
     *  function map is also created to be used later for typechecking purposes.
     *
     *  Requires: CX type and function definitions from the preparse step for both the current
     *            compilation unit and all imports, as well as the lexed and preprocessed source code.
     *
     *  Outputs:  A naively parsed AST.
     */
    ASTParse = 1 << 2,

    /**
     *  Part 1 of typechecking: Ensures that all expressions in directly implemented functions and
     *  types are type-correct, including adding implicit type coercion where necessary.
     *
     *  Importantly, and why two steps are needed, this creates a fully-contextual type and function map
     *  that combines declarations from other ASTs. Meaning templates declared in other compilation units
     *  require that those units are typechecked with this step before they can be realized. In theory, this
     *  step could be limited to just generating the type and function map of the current compilation unit,
     *  and then all functions and templates could be typechecked in a single pass, but realizing templates,
     *  requires cooperation with the pipeline to be given the needed context, meaning indirect and direct
     *  typechecking would just be unignorably two separate mechanisms included in the same step.
     *
     *  Requires: The naive AST, along with the type and function definitions of imports and self.
     *
     *  Outputs:  A type-checked AST of direct implementation, including to-be type-checked requests to
     *  be fulfilled.
     */
    DirectTypechecking = 1 << 3,

    /**
     *  Part 2 of typechecking: Typechecks all indirectly implemented functions and types to a type-checked
     *  AST. This for the most part consists of realizing templated functions, however in the future other
     *  use-cases may arise related to the implementation of dependent types or other advanced type system features.
     *  As well, in the future, these steps being separated could allow for the pipeline to make better decisions
     *  regarding duplicate template instantiations across multiple compilation units.
     *
     *  Requires: A directly type-checked AST, along with its requests, and the directly type-checked ASTs of
     *  the compilation units where the declaration referenced by the requests were defined.
     *
     *  Outputs:  A fully type-checked AST.
     */
    IndirectTypechecking = 1 << 4,

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
    BytecodeGen = 1 << 5,

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
    Codegen = 1 << 6
    
    // For now, linking is a single step that is done after all compilation above is done. This 
    // could be abstracted into a CompilationStep, but seeing as it is not a job that occurs
    // per-compilation unit, it handled as its own mechanism.
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

impl Default for JobQueue {
    fn default() -> Self {
        Self::new()
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
        self.progress_map.insert((unit.clone(), CompilationStep::DirectTypechecking), JobState::Completed);
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
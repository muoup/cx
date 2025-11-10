use crate::CompilationUnit;
use std::cmp::PartialEq;
use std::collections::{HashMap, VecDeque};
use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum JobState {
    InQueue,
    Completed,
}

pub struct JobQueue {
    progress_map: HashMap<(CompilationUnit, CompilationStep), JobState>,
    data: VecDeque<CompilationJob>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilationJob {
    pub step: CompilationStep,
    pub unit: CompilationUnit,
    pub requirements: Vec<CompilationJobRequirement>,

    pub compilation_exists: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilationJobRequirement {
    pub step: CompilationStep,
    pub unit: CompilationUnit,
}

pub type CompilationStepRepr = u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum CompilationStep {
    /**
     *  Parses all type aliases in a compilation unit, i.e. typedef identifiers, along with any identifiers
     *  declared with a struct/enum/union keyword. This step is necessary to resolve ambiguities during parsing.
     *
     *  Also handles lexing and preprocessing of the source code, which is required before parsing can occur.
     *  
     *  Requires: The raw source code of the compilation unit.
     * 
     *  Outputs:  A list of lexemes / tokens from lexing and preprocessing, along with a type symbol set.
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
     *  Outputs:  A naively parsed AST.
     */
    ASTParse = 1 << 1,
    
    /**
     *  Prior to typechecking, the compiler must combine all publically accessible types and functions
     *  from imports into the current compilation unit's AST. This is similar to ImportCombine, however
     *  instead of combining preparse data, or the names of types needed for clearing ambiguities during
     *  parsing, this step combines actual type and function data so that if the current compilation unit
     *  references a type or function. Note that this does not generate completed type or function definitons,
     *  but rather a list of incomplete type's and their module origins, so that during typechecking, if a type
     *  or function is needed, the typechecker can complete the type knowing where relevant information about
     *  internal types that not be publicly accessible is stored.
     * 
     *  Requires: A naively parsed AST, along with the type and function definitions of imports.
     *  Outputs:  A base data structure, containing the publically accessible types and functions of imports
     *  along with their module origins, along with the same data from the current compilation unit.
     */
    InterfaceCombine = 1 << 2,

    /**
     *  Typechecks all indirectly implemented functions and types to a type-checked
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
    Typechecking = 1 << 3,

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
    BytecodeGen = 1 << 4,

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
    Codegen = 1 << 5, // For now, linking is a single step that is done after all compilation above is done. This
                      // could be abstracted into a CompilationStep, but seeing as it is not a job that occurs
                      // per-compilation unit, it handled as its own mechanism.
}

impl CompilationJob {
    pub fn new(
        requirements: Vec<CompilationJobRequirement>,
        step: CompilationStep,
        unit: CompilationUnit,
    ) -> Self {
        CompilationJob {
            requirements,
            step,
            unit,

            compilation_exists: false,
        }
    }

    pub fn as_requirement(&self) -> CompilationJobRequirement {
        CompilationJobRequirement {
            step: self.step,
            unit: self.unit.clone(),
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
            data: VecDeque::new(),
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
        self.progress_map
            .insert((job.unit.clone(), job.step), JobState::Completed);
    }

    pub fn complete_all_unit_jobs(&mut self, _unita: &CompilationUnit) {
        todo!()
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
        self.progress_map
            .insert((job.unit.clone(), job.step), JobState::Completed);
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

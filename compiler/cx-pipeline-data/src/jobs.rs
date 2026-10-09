use crate::CompilationUnit;
use std::cmp::PartialEq;
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum JobState {
    InQueue,
    Completed,
}

pub struct JobQueue {
    shallow_progress_map: HashMap<(CompilationUnit, CompilationStep), JobState>,
    deep_progress_map: HashSet<(CompilationUnit, CompilationStep)>,
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
    pub shallow: bool,
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
    Parse = 1 << 1,

    HMIR = 1 << 2,

    /**
     *
     *  Requires: A fully type-checked AST.
     *
     *  Outputs: An analyzed MIR representation.
     */
    MIR = 1 << 3,

    /** Lowers analyzed MIR into ABI- and layout-aware LMIR. */
    LMIR = 1 << 4,

    /** Compiles one LMIR unit into an object file. */
    Codegen = 1 << 5,
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
            shallow: true,
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
            shallow_progress_map: HashMap::new(),
            deep_progress_map: HashSet::new(),
            data: VecDeque::new(),
        }
    }

    pub fn push_new_job(&mut self, job: CompilationJob) {
        let pair = (job.unit.clone(), job.step);

        if !self.shallow_progress_map.contains_key(&pair) {
            self.data.push_back(job);
            self.shallow_progress_map.insert(pair, JobState::InQueue);
        }
    }

    pub fn push_job(&mut self, job: CompilationJob) {
        let pair = (job.unit.clone(), job.step);

        self.data.push_back(job);
        self.shallow_progress_map.insert(pair, JobState::InQueue);
    }

    pub fn pop_job(&mut self) -> Option<CompilationJob> {
        self.data.pop_front()
    }

    pub fn complete_job(&mut self, job: &CompilationJob) {
        self.shallow_progress_map
            .insert((job.unit.clone(), job.step), JobState::Completed);
    }

    pub fn complete_all_unit_jobs(&mut self, unit: &CompilationUnit) {
        for step in [
            CompilationStep::PreParse,
            CompilationStep::Parse,
            CompilationStep::MIR,
            CompilationStep::LMIR,
            CompilationStep::Codegen,
        ] {
            self.shallow_progress_map
                .insert((unit.clone(), step), JobState::Completed);
        }
    }

    pub fn job_complete(&self, job: &CompilationJob) -> bool {
        self.shallow_progress_map.get(&(job.unit.clone(), job.step)) == Some(&JobState::Completed)
    }

    pub fn requirements_complete<F>(&mut self, job: &CompilationJob, imports_for_unit: F) -> bool
    where
        F: Fn(&CompilationUnit) -> Option<Vec<CompilationUnit>>,
    {
        let mut visiting = HashSet::new();

        job.requirements
            .iter()
            .all(|req| self.requirement_complete(req, &imports_for_unit, &mut visiting))
    }

    fn requirement_complete<F>(
        &mut self,
        req: &CompilationJobRequirement,
        imports_for_unit: &F,
        visiting: &mut HashSet<(CompilationUnit, CompilationStep)>,
    ) -> bool
    where
        F: Fn(&CompilationUnit) -> Option<Vec<CompilationUnit>>,
    {
        if req.shallow {
            return self.shallow_requirement_complete(&req.unit, req.step);
        }

        self.deep_requirement_complete(&req.unit, req.step, imports_for_unit, visiting)
    }

    fn shallow_requirement_complete(&self, unit: &CompilationUnit, step: CompilationStep) -> bool {
        self.shallow_progress_map.get(&(unit.clone(), step)) == Some(&JobState::Completed)
    }

    fn deep_requirement_complete<F>(
        &mut self,
        unit: &CompilationUnit,
        step: CompilationStep,
        imports_for_unit: &F,
        visiting: &mut HashSet<(CompilationUnit, CompilationStep)>,
    ) -> bool
    where
        F: Fn(&CompilationUnit) -> Option<Vec<CompilationUnit>>,
    {
        let key = (unit.clone(), step);

        if self.deep_progress_map.contains(&key) {
            return true;
        }

        if !self.shallow_requirement_complete(unit, step) {
            return false;
        }

        if !visiting.insert(key.clone()) {
            return true;
        }

        let Some(imports) = imports_for_unit(unit) else {
            visiting.remove(&key);
            return false;
        };

        let complete = imports
            .iter()
            .all(|import| self.deep_requirement_complete(import, step, imports_for_unit, visiting));

        visiting.remove(&key);

        if complete {
            self.deep_progress_map.insert(key);
        }

        complete
    }

    pub fn finish_job(&mut self, job: &CompilationJob) {
        self.shallow_progress_map
            .insert((job.unit.clone(), job.step), JobState::Completed);
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

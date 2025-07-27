pub mod jobs;
mod db;

use std::collections::{HashSet, VecDeque};
use cx_exec_data::{CompilerBackend, OptimizationLevel};
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Mutex;
use crate::db::ModuleData;
use crate::jobs::CompilationStep;

pub type CompilationUnit = Rc<str>;

pub struct GlobalCompilationContext {
    pub config: CompilerConfig,
    pub module_db: Mutex<ModuleData>
}

pub struct CompilationJobHandler {
    pub completed_steps: HashSet<CompilationStep>,
    pub todo_steps: VecDeque<(Option<CompilationStep>, CompilationStep)>
}

pub struct CompilerConfig {
    pub backend: CompilerBackend,
    pub optimization_level: OptimizationLevel,
    pub output: PathBuf,
}
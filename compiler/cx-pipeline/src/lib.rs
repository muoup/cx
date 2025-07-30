mod scheduler;
mod backends;
mod linker;

use std::path::Path;
use std::sync::{Mutex, RwLock};
use cx_data_pipeline::{CompilationUnit, CompilerConfig, GlobalCompilationContext};
use cx_data_pipeline::db::ModuleData;
use cx_data_pipeline::jobs::{CompilationJob, CompilationStep};
use crate::linker::link;
use crate::scheduler::scheduling_loop;

pub fn standard_compilation(
    config: CompilerConfig,
    base_file: &Path
) -> Option<()> {
    let compiler_context = GlobalCompilationContext {
        config,
        module_db: RwLock::new(ModuleData::new()),
        linking_files: Mutex::new(vec![]),
    };

    let initial_job = CompilationJob::new(
        vec![],
        CompilationStep::PreParse,
        CompilationUnit::new(base_file.to_str()?),
    );

    scheduling_loop(&compiler_context, initial_job)?;
    link(&compiler_context)?;

    Some(())
}
mod scheduler;
mod backends;
mod linker;

use crate::linker::link;
use crate::scheduler::scheduling_loop;
use cx_data_pipeline::db::ModuleData;
use cx_data_pipeline::jobs::{CompilationJob, CompilationStep};
use cx_data_pipeline::{CompilationUnit, CompilerConfig, GlobalCompilationContext};
use std::collections::HashSet;
use std::path::Path;
use std::sync::Mutex;

pub fn standard_compilation(
    config: CompilerConfig,
    base_file: &Path
) -> Option<()> {
    let compiler_context = GlobalCompilationContext {
        config,
        module_db: ModuleData::new(),
        linking_files: Mutex::new(HashSet::new()),
    };

    let initial_job = CompilationJob::new(
        vec![],
        CompilationStep::PreParse,
        CompilationUnit::from_str(base_file.to_str()?),
    );

    scheduling_loop(&compiler_context, initial_job)?;
    link(&compiler_context)?;

    Some(())
}
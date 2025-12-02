// mod backends;
mod linker;
mod scheduler;
mod template_realizing;

use crate::linker::link;
use crate::scheduler::scheduling_loop;
use cx_pipeline_data::db::ModuleData;
use cx_pipeline_data::jobs::{CompilationJob, CompilationStep};
use cx_pipeline_data::{CompilationUnit, CompilerConfig, GlobalCompilationContext};
use std::collections::HashSet;
use std::path::Path;
use std::sync::Mutex;

pub fn standard_compilation(config: CompilerConfig, base_file: &Path) -> Option<()> {
    let previous_dir = std::env::current_dir().ok()?;

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

    std::env::set_current_dir(previous_dir).ok()?;

    Some(())
}

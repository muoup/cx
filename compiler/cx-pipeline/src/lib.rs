// mod backends;
mod linker;
mod scheduler;
mod template_realizing;
mod backends;

use crate::linker::link;
use crate::scheduler::scheduling_loop;
use cx_util::format::with_dump_directory;
use cx_pipeline_data::db::ModuleData;
use cx_pipeline_data::jobs::{CompilationJob, CompilationStep};
use cx_pipeline_data::{CompilationUnit, CompilerConfig, GlobalCompilationContext};
use std::collections::HashSet;
use std::path::Path;
use std::sync::Mutex;

pub fn standard_compilation(config: CompilerConfig, base_file: &Path) -> Option<()> {
    let compiler_context = GlobalCompilationContext {
        config,
        module_db: ModuleData::new(),
        linking_files: Mutex::new(HashSet::new()),
    };

    let initial_job = CompilationJob::new(
        vec![],
        CompilationStep::PreParse,
        CompilationUnit::from_rooted(base_file.to_str()?, &compiler_context.config.working_directory),
    );

    with_dump_directory(compiler_context.config.internal_directory.clone(), || {
        scheduling_loop(&compiler_context, initial_job)?;
        link(&compiler_context)
    })?;

    Some(())
}

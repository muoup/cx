mod scheduler;

use std::path::Path;
use std::sync::Mutex;
use cx_data_pipeline::{CompilationUnit, CompilerConfig, GlobalCompilationContext};
use cx_data_pipeline::db::ModuleData;
use cx_data_pipeline::jobs::{CompilationJob, CompilationStep};
use crate::scheduler::scheduling_loop;

pub fn standard_compilation(
    config: CompilerConfig,
    base_file: &Path
) -> Option<()> {
    let compiler_context = GlobalCompilationContext {
        config,
        module_db: Mutex::new(ModuleData::new()),
    };
    let initial_job = CompilationJob::new(
        vec![],
        CompilationStep::PreParseStage1,
        CompilationUnit::from(base_file.to_str().unwrap()),
    );

    scheduling_loop(&compiler_context, initial_job)?;

    Some(())
}
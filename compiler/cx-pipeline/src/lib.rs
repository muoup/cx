// mod backends;
mod linker;
mod scheduler;
mod template_realizing;
mod backends;

use crate::linker::link;
use crate::scheduler::scheduling_loop;
use crate::scheduler::scheduling_loop_collect_errors;
use cx_pipeline_data::db::ModuleData;
use cx_pipeline_data::jobs::{CompilationJob, CompilationStep};
use cx_pipeline_data::{CompilationUnit, CompilerConfig, GlobalCompilationContext};
use std::collections::HashSet;
use std::path::Path;
use std::sync::Mutex;

// Re-export LSPErrors for use by cx-lsp
pub use crate::scheduler::LSPErrors;

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

/// Typecheck-only compilation for LSP integration.
///
/// This function runs the typechecker on the project starting from the given file
/// and returns all errors found. Unlike standard_compilation, this does not
/// generate code or link - it only performs lexing, parsing, and typechecking.
///
/// This is designed for LSP use cases where we want to report errors to the user
/// without the overhead of full compilation.
///
/// # Arguments
/// * `context` - The global compilation context (module database, config)
/// * `initial_file` - The file to start typechecking from (typically the saved file)
///
/// # Returns
/// A vector of LSPErrors (both type errors and fatal errors) found during compilation
pub fn typecheck_only_lsp(
    context: &GlobalCompilationContext,
    initial_file: &CompilationUnit,
) -> Vec<LSPErrors> {
    let mut errors = Vec::new();

    let initial_job = CompilationJob::new(
        vec![],
        CompilationStep::PreParse,
        initial_file.clone(),
    );

    scheduling_loop_collect_errors(context, initial_job, &mut errors);
    errors
}

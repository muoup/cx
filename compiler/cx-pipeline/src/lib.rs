// mod backends;
mod linker;
mod scheduler;
mod template_realizing;
mod backends;

use crate::linker::link;
use crate::scheduler::scheduling_loop;
use crate::scheduler::scheduling_loop_collect_errors;
use cx_util::CXError;
use cx_util::CXResult;
use cx_util::format::with_dump_directory;
use cx_pipeline_data::db::ModuleData;
use cx_pipeline_data::jobs::{CompilationJob, CompilationStep};
use cx_pipeline_data::{CompilationUnit, CompilerConfig, GlobalCompilationContext};
use std::collections::HashSet;
use std::path::Path;
use std::sync::Mutex;

// Re-export LSP diagnostic types for use by cx-lsp
pub use crate::scheduler::{LSPErrorSpan, LSPErrors};

pub fn standard_compilation(config: CompilerConfig, base_file: &Path) -> CXResult<()> {
    let compiler_context = GlobalCompilationContext {
        config,
        module_db: ModuleData::new(),
        linking_files: Mutex::new(HashSet::new()),
    };
    
    let base_file_str = base_file.to_str()
        .ok_or(CXError::create_boxed("Base file path is not valid UTF-8"))?;

    let initial_job = CompilationJob::new(
        vec![],
        CompilationStep::PreParse,
        CompilationUnit::from_rooted(base_file_str, &compiler_context.config.working_directory),
    );

    with_dump_directory(compiler_context.config.internal_directory.clone(), || {
        scheduling_loop(&compiler_context, initial_job)?;
        link(&compiler_context)
    })?;

    Ok(())
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

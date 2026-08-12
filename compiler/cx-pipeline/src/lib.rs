pub mod progress;
mod scheduler;

use crate::progress::ProgressReporter;
use crate::scheduler::scheduling_loop;
use crate::scheduler::scheduling_loop_collect_errors;
use cx_ast::registry::ExportNameMode;
use cx_log::{
    CXResult,
    error::{CXErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_pipeline_data::config::{CXProjectConfig, TargetConfig};
use cx_pipeline_data::db::ModuleData;
use cx_pipeline_data::jobs::{CompilationJob, CompilationStep};
use cx_pipeline_data::{
    CompilationMode, CompilationUnit, CompilerConfig, GlobalCompilationContext,
};
use cx_util::format::{with_dump_directory, without_dumps};
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

// Re-export LSP diagnostic types for use by cx-lsp
pub use crate::scheduler::LSPErrors;

pub struct LSPCheckResult {
    pub errors: Vec<LSPErrors>,
    pub checked_files: HashSet<PathBuf>,
}

pub(crate) fn pipeline_error(code: impl Into<String>, message: impl Into<String>) -> CXErr {
    CXErr::new(
        CXStdErrMessage::error(code, message),
        CXInternalContext::error("pipeline operation failed outside source context"),
    )
}

pub fn standard_compilation(config: CompilerConfig, base_file: &Path) -> CXResult<()> {
    let verbose = config.verbose;
    let compiler_context = GlobalCompilationContext {
        module_mode: config.module_mode,
        config,
        module_db: ModuleData::new(),
        linking_files: Mutex::new(HashSet::new()),
    };

    let base_file_str = base_file.to_str().ok_or(pipeline_error(
        "COMPILATION ERROR",
        "Base file path is not valid UTF-8",
    ))?;
    let entry_unit =
        CompilationUnit::from_rooted(base_file_str, &compiler_context.config.working_directory);
    compiler_context
        .module_db
        .symbol_registry
        .set_export_name_mode(entry_unit.to_namespace_path(), ExportNameMode::Root);

    let initial_job = CompilationJob::new(vec![], CompilationStep::PreParse, entry_unit);
    let mut reporter = ProgressReporter::new(verbose);

    let result = with_dump_directory(compiler_context.config.internal_directory.clone(), || {
        scheduling_loop(&compiler_context, initial_job, &mut reporter)
    });

    if result.is_err() {
        reporter.clear_line();
        return result;
    }

    reporter.finish();
    Ok(())
}

pub fn project_compilation(
    base_config: CompilerConfig,
    project_config: &CXProjectConfig,
    target_filter: Option<&str>,
) -> CXResult<Vec<PathBuf>> {
    let workspace = project_config.workspace.as_ref().ok_or(pipeline_error(
        "COMPILATION ERROR",
        "cx.toml has no [workspace] section",
    ))?;

    let filter_name;
    let targets: Vec<(&String, &TargetConfig)> = if let Some(filter) = target_filter {
        let target = workspace.targets.get(filter).ok_or(pipeline_error(
            "COMPILATION ERROR",
            format!("Target '{}' not found in cx.toml", filter),
        ))?;
        filter_name = filter.to_string();
        vec![(&filter_name, target)]
    } else {
        workspace.targets.iter().collect()
    };

    for (target_name, target_config) in targets {
        let include_dirs = target_config
            .include_dirs
            .clone()
            .unwrap_or_default()
            .into_iter()
            .map(|path| {
                let path = PathBuf::from(path);
                if path.is_absolute() {
                    path
                } else {
                    base_config.working_directory.join(path)
                }
            })
            .collect::<Vec<_>>();

        if let Some(binaries) = &target_config.binaries {
            for binary in binaries {
                reject_project_c_source(&binary.entry)?;

                let mut config = base_config.clone();
                config.compilation_mode = CompilationMode::Executable;
                config.include_dirs = include_dirs.clone();

                eprintln!(
                    "Generating MIR for binary '{}' (target: {})",
                    binary.name, target_name
                );
                standard_compilation(config, Path::new(&binary.entry))?;
            }
        }

        if let Some(libraries) = &target_config.libraries {
            for library in libraries {
                reject_project_c_source(&library.entry)?;

                let mut config = base_config.clone();
                config.compilation_mode = CompilationMode::Library;
                config.include_dirs = include_dirs.clone();

                eprintln!(
                    "Generating MIR for library '{}' (target: {})",
                    library.name, target_name
                );
                standard_compilation(config, Path::new(&library.entry))?;
            }
        }
    }

    // Artifact generation is intentionally disconnected while MIR is refactored.
    Ok(Vec::new())
}

fn reject_project_c_source(entry: &str) -> CXResult<()> {
    if Path::new(entry).extension().and_then(|ext| ext.to_str()) == Some("c") {
        Err(pipeline_error(
            "COMPILATION ERROR",
            "C sources are currently supported only in single-file compilation mode",
        ))
    } else {
        Ok(())
    }
}

/// Typecheck-only compilation for LSP integration.
pub fn typecheck_only_lsp(
    context: &GlobalCompilationContext,
    initial_file: &CompilationUnit,
) -> LSPCheckResult {
    let mut errors = Vec::new();
    let mut checked_files = HashSet::new();

    let initial_job = CompilationJob::new(vec![], CompilationStep::PreParse, initial_file.clone());

    without_dumps(|| {
        scheduling_loop_collect_errors(context, initial_job, &mut errors, &mut checked_files);
    });

    LSPCheckResult {
        errors,
        checked_files,
    }
}

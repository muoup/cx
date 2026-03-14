mod linker;
mod scheduler;
mod template_realizing;
mod backends;
pub mod progress;

use crate::linker::link;
use crate::progress::ProgressReporter;
use crate::scheduler::scheduling_loop;
use crate::scheduler::scheduling_loop_collect_errors;
use cx_pipeline_data::config::{CXProjectConfig, TargetConfig};
use cx_pipeline_data::db::ModuleData;
use cx_pipeline_data::jobs::{CompilationJob, CompilationStep};
use cx_pipeline_data::{CompilationMode, CompilationUnit, CompilerConfig, GlobalCompilationContext};
use cx_util::CXError;
use cx_util::CXResult;
use cx_util::format::with_dump_directory;
use std::collections::HashSet;
use std::path::Path;
use std::sync::Mutex;

// Re-export LSP diagnostic types for use by cx-lsp
pub use crate::scheduler::{LSPErrorSpan, LSPErrors};

pub fn standard_compilation(config: CompilerConfig, base_file: &Path) -> CXResult<()> {
    let verbose = config.verbose;
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

    let mut reporter = ProgressReporter::new(verbose);

    let result = with_dump_directory(compiler_context.config.internal_directory.clone(), || {
        scheduling_loop(&compiler_context, initial_job, &mut reporter)?;
        link(&compiler_context, &reporter)
    });

    if result.is_err() {
        reporter.clear_line();
        return result;
    }

    reporter.finish();

    Ok(())
}

pub fn library_compilation(config: CompilerConfig, base_file: &Path) -> CXResult<(cx_lmir::LMIRUnit, HashSet<std::path::PathBuf>)> {
    let verbose = config.verbose;
    let compiler_context = GlobalCompilationContext {
        config,
        module_db: ModuleData::new(),
        linking_files: Mutex::new(HashSet::new()),
    };

    let base_file_str = base_file.to_str()
        .ok_or(CXError::create_boxed("Base file path is not valid UTF-8"))?;

    let entry_unit = CompilationUnit::from_rooted(base_file_str, &compiler_context.config.working_directory);

    let initial_job = CompilationJob::new(
        vec![],
        CompilationStep::PreParse,
        entry_unit.clone(),
    );

    let mut reporter = ProgressReporter::new(verbose);

    let result = with_dump_directory(compiler_context.config.internal_directory.clone(), || {
        scheduling_loop(&compiler_context, initial_job, &mut reporter)
    });

    if result.is_err() {
        reporter.clear_line();
        return Err(result.unwrap_err());
    }

    reporter.finish();

    // Take only the entry file's LMIR unit (not imports/dependencies)
    let entry_lmir = compiler_context.module_db.lmir.take(&entry_unit);

    // Drain the linking files so the caller can copy them to the output directory
    let linking_files = compiler_context.linking_files.lock()
        .expect("Deadlock on linking files mutex")
        .drain()
        .collect();

    Ok((entry_lmir, linking_files))
}

pub fn project_compilation(
    base_config: CompilerConfig,
    project_config: &CXProjectConfig,
    target_filter: Option<&str>,
) -> CXResult<()> {
    let workspace = project_config.workspace.as_ref()
        .ok_or(CXError::create_boxed("cx.toml has no [workspace] section"))?;

    let filter_name;
    let targets: Vec<(&String, &TargetConfig)> = if let Some(filter) = target_filter {
        let target = workspace.targets.get(filter)
            .ok_or(CXError::create_boxed(format!("Target '{}' not found in cx.toml", filter)))?;
        filter_name = filter.to_string();
        vec![(&filter_name, target)]
    } else {
        workspace.targets.iter().collect()
    };

    for (target_name, target_config) in targets {
        let link_entries = target_config.link.clone().unwrap_or_default();

        let output_dir = base_config.internal_directory.join("output").join(target_name);
        std::fs::create_dir_all(&output_dir)
            .map_err(|e| CXError::create_boxed(format!(
                "Failed to create output directory {}: {}", output_dir.display(), e
            )))?;

        // Build binaries
        if let Some(binaries) = &target_config.binaries {
            for binary in binaries {
                let output = output_dir.join(&binary.name);
                let mut config = base_config.clone();
                config.output = output;
                config.compilation_mode = CompilationMode::Binary;
                config.link_entries = link_entries.clone();

                eprintln!("Building binary '{}' (target: {})", binary.name, target_name);
                standard_compilation(config, Path::new(&binary.entry))?;
            }
        }

        // Build libraries
        if let Some(libraries) = &target_config.libraries {
            for library in libraries {
                let mut config = base_config.clone();
                config.compilation_mode = CompilationMode::Library;
                config.link_entries = link_entries.clone();
                config.output = output_dir.join(&library.name);

                eprintln!("Building library '{}' (target: {})", library.name, target_name);

                let (lmir_units, _) = library_compilation(config.clone(), Path::new(&library.entry))?;

                // Generate .h header from LMIR
                let Ok(header) = cx_c_header::generate_header(&library.name, &lmir_units, &link_entries) else {
                    eprintln!("Warning: Failed to generate header for library '{}': {}", library.name, "Header generation is best-effort and will not fail the build");
                    continue;
                };
                
                let header_path = output_dir.join(format!("{}.h", library.name));
                std::fs::write(&header_path, header)
                    .map_err(|e| CXError::create_boxed(format!(
                        "Failed to write header {}: {}", header_path.display(), e
                    )))?;

                eprintln!("  Generated {}", header_path.display());
            }
        }
    }

    Ok(())
}

/// Typecheck-only compilation for LSP integration.
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

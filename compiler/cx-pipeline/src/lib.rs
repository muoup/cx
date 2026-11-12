mod backends;
mod linker;
mod log;
mod scheduler;
mod sources;

pub mod pipeline;
pub mod progress;
pub mod project;

use cx_log::catalogue::driver as catalogue;
use log::pipeline_error;

use crate::linker::{link, link_objects, link_relocatable};
use crate::pipeline::{Pipeline, PipelineFailure};
use crate::progress::ProgressReporter;
use cx_log::error::CXError;
use cx_log::CXResult;
use cx_namespace::module::{ModulePath, NamespacePath};
use cx_pipeline_data::internal_storage::resource_path;
use cx_pipeline_data::jobs::CompilationStep;
use cx_pipeline_data::{
    CompilationMode, CompilationUnit, CompilerConfig, GlobalCompilationContext,
};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

// Compilation recurses over source structure, so it runs on threads with a generous stack
pub const COMPILER_STACK_SIZE: usize = 256 * 1024 * 1024;

pub fn link_object_files(output: &Path, object_files: &[PathBuf]) -> CXResult<()> {
    link_objects(output, object_files)
}

pub fn compile(config: CompilerConfig, sources: &[PathBuf]) -> CXResult<BTreeMap<PathBuf, String>> {
    if sources.is_empty() {
        return Err(pipeline_error(&catalogue::NO_SOURCES, None));
    }
    let context = GlobalCompilationContext::new(config);
    let namespace = (!context.config.module_mode).then(NamespacePath::root);
    let units = sources
        .iter()
        .map(|source| root_unit(&context.config, source, namespace.clone()))
        .collect::<CXResult<Vec<_>>>()?;

    let mut reporter = ProgressReporter::new(context.config.verbose);
    let result = Pipeline::new(&context, CompilationStep::Codegen)
        .roots(units.iter().cloned())
        .run(|job| reporter.start_job(job))
        .map_err(earliest_failure)
        .and_then(|()| finish_artifact(&context, &units, &mut reporter));

    if let Err(error) = result {
        reporter.clear_line();
        return Err(error);
    }
    reporter.finish();
    Ok(context.headers.into_inner().unwrap())
}

fn earliest_failure(failures: Vec<PipelineFailure>) -> CXError {
    failures
        .into_iter()
        .min_by_key(|failure| failure.job.step)
        .expect("a failed pipeline run reports at least one failure")
        .error
}

fn finish_artifact(
    context: &GlobalCompilationContext,
    units: &[CompilationUnit],
    reporter: &mut ProgressReporter,
) -> CXResult<()> {
    match context.config.compilation_mode {
        CompilationMode::Executable => link(context, reporter),
        CompilationMode::Object => match units {
            [unit] => copy_object(context, unit),
            _ => Ok(()),
        },
        CompilationMode::Library => {
            let symbols = context.library_symbols.lock().unwrap();
            let roots = units
                .iter()
                .filter_map(|unit| symbols.get(unit.module().as_path()))
                .flatten()
                .cloned()
                .collect::<BTreeSet<_>>()
                .into_iter()
                .collect::<Vec<_>>();
            drop(symbols);
            link_relocatable(context, &roots, reporter)
        }
    }
}

fn root_unit(
    config: &CompilerConfig,
    source: &Path,
    namespace: Option<NamespacePath>,
) -> CXResult<CompilationUnit> {
    source
        .to_str()
        .ok_or_else(|| pipeline_error(&catalogue::PATH_ENCODING, "source file".into()))?;
    let unit = CompilationUnit::new(
        &config.source_root,
        ModulePath::new(source.to_path_buf()),
        namespace.clone(),
    );
    if namespace.is_none() && !unit.module().as_path().starts_with(&config.source_root) {
        return Err(pipeline_error(
            &catalogue::OUTSIDE_SOURCE_ROOT,
            (
                format!("{}", source.display()),
                format!("{}", config.source_root.display()),
            ),
        ));
    }
    Ok(unit)
}

fn copy_object(context: &GlobalCompilationContext, unit: &CompilationUnit) -> CXResult<()> {
    let object_path = resource_path(context, unit, ".o");
    let output = &context.config.output;
    let failure = |operation: &str, resource: &str, path: &Path, error: std::io::Error| {
        pipeline_error(
            &catalogue::FILE_OPERATION,
            (
                operation.into(),
                resource.into(),
                Some(format!("{}", path.display())),
                format!("{}", error),
            ),
        )
    };
    if let Some(parent) = output.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|error| failure("create", "object output directory", parent, error))?;
    }
    std::fs::copy(&object_path, output)
        .map_err(|error| failure("write", "object file", output, error))?;
    Ok(())
}

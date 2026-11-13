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

use crate::linker::{link_executable, link_relocatable};
use crate::pipeline::{Pipeline, PipelineFailure};
use crate::progress::ProgressReporter;
use cx_log::error::CXError;
use cx_log::CXResult;
use cx_namespace::module::{ModulePath, NamespacePath};
use cx_pipeline_data::internal_storage::resource_path;
use cx_pipeline_data::jobs::CompilationStep;
use cx_pipeline_data::package::PackageId;
use cx_pipeline_data::{
    CompilationMode, CompilationUnit, CompilerConfig, GlobalCompilationContext,
};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

// Compilation recurses over source structure, so it runs on threads with a generous stack
pub const COMPILER_STACK_SIZE: usize = 256 * 1024 * 1024;

pub fn link_object_files(output: &Path, object_files: &[PathBuf]) -> CXResult<()> {
    link_executable(output, object_files, &[])
}

pub fn compile(config: CompilerConfig, package: PackageId, sources: &[PathBuf]) -> CXResult<()> {
    if sources.is_empty() {
        return Err(pipeline_error(&catalogue::NO_SOURCES, None));
    }
    let context = GlobalCompilationContext::new(config);
    let units = sources
        .iter()
        .map(|source| root_unit(&context.config, package, source))
        .collect::<CXResult<Vec<_>>>()?;

    reporting(context.config.verbose, |reporter| {
        run_pipeline(&context, &units, reporter)?;
        finish_artifact(&context, &units, reporter)
    })
}

pub(crate) fn reporting<T>(
    verbose: bool,
    work: impl FnOnce(&mut ProgressReporter) -> CXResult<T>,
) -> CXResult<T> {
    let mut reporter = ProgressReporter::new(verbose);
    let result = work(&mut reporter);
    match &result {
        Ok(_) => reporter.finish(),
        Err(_) => reporter.clear_line(),
    }
    result
}

pub(crate) fn run_pipeline(
    context: &GlobalCompilationContext,
    units: &[CompilationUnit],
    reporter: &mut ProgressReporter,
) -> CXResult<()> {
    Pipeline::new(context, CompilationStep::Codegen)
        .roots(units.iter().cloned())
        .run(|job| reporter.start_job(job))
        .map_err(earliest_failure)
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
    let config = &context.config;
    let objects = context.linking_files.lock().unwrap().iter().cloned().collect::<Vec<_>>();
    match config.compilation_mode {
        CompilationMode::Executable => {
            reporter.link_status("Linking");
            let objects = objects.into_iter().chain(config.native_objects.iter().cloned()).collect::<Vec<_>>();
            link_executable(&config.output, &objects, &config.link_entries)
        }
        CompilationMode::Object => match units {
            [unit] => copy_object(context, unit),
            _ => Ok(()),
        },
        CompilationMode::Library => {
            reporter.link_status("Linking (relocatable)");
            link_relocatable(&config.output, &objects, &library_roots(context, units))
        }
    }
}

pub(crate) fn library_roots(context: &GlobalCompilationContext, units: &[CompilationUnit]) -> Vec<String> {
    let symbols = context.library_symbols.lock().unwrap();
    units
        .iter()
        .filter_map(|unit| symbols.get(unit.module().as_path()))
        .flatten()
        .cloned()
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect()
}

pub(crate) fn root_unit(
    config: &CompilerConfig,
    package: PackageId,
    source: &Path,
) -> CXResult<CompilationUnit> {
    source
        .to_str()
        .ok_or_else(|| pipeline_error(&catalogue::PATH_ENCODING, "source file".into()))?;
    if !config.module_mode {
        return Ok(CompilationUnit::new(
            package,
            ModulePath::new(source.to_path_buf()),
            NamespacePath::root(),
        ));
    }
    config.packages.unit(package, source).ok_or_else(|| {
        pipeline_error(
            &catalogue::OUTSIDE_SOURCE_ROOT,
            (
                format!("{}", source.display()),
                format!("{}", config.packages.get(package).source_root.display()),
            ),
        )
    })
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

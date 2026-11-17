mod backends;
mod linker;
mod log;
mod objects;
mod scheduler;
mod sources;

pub mod pipeline;
pub mod progress;
pub mod project;

use chl_log::catalogue::driver as catalogue;
use log::pipeline_error;

use crate::linker::{link_executable, link_relocatable};
use crate::pipeline::{Pipeline, PipelineFailure};
use crate::progress::ProgressReporter;
use chl_log::error::CHLError;
use chl_log::CHLResult;
use chl_namespace::module::{ModulePath, NamespacePath};
use chl_pipeline_data::internal_storage::resource_path;
use chl_pipeline_data::jobs::CompilationStep;
use chl_pipeline_data::package::PackageId;
use chl_pipeline_data::{
    CompilationMode, CompilationUnit, CompilerConfig, GlobalCompilationContext,
};
use crate::scheduler::imported_units;
use rustc_hash::FxHashSet;
use std::path::{Path, PathBuf};

// Compilation recurses over source structure, so it runs on threads with a generous stack
pub const COMPILER_STACK_SIZE: usize = 256 * 1024 * 1024;

pub fn link_object_files(output: &Path, object_files: &[PathBuf]) -> CHLResult<()> {
    link_executable(output, object_files, &[])
}

pub fn compile(config: CompilerConfig, package: PackageId, sources: &[PathBuf]) -> CHLResult<()> {
    if sources.is_empty() {
        return Err(pipeline_error(&catalogue::NO_SOURCES, None));
    }
    let context = GlobalCompilationContext::new(config);
    let units = sources
        .iter()
        .map(|source| root_unit(&context.config, package, source))
        .collect::<CHLResult<Vec<_>>>()?;

    reporting(context.config.verbose, |reporter| {
        let mut pipeline = Pipeline::new(&context, CompilationStep::Codegen);
        run_pipeline(&mut pipeline, &units, reporter)?;
        finish_artifact(&context, package, &units, reporter)
    })
}

pub(crate) fn reporting<T>(
    verbose: bool,
    work: impl FnOnce(&mut ProgressReporter) -> CHLResult<T>,
) -> CHLResult<T> {
    let mut reporter = ProgressReporter::new(verbose);
    let result = work(&mut reporter);
    match &result {
        Ok(_) => reporter.finish(),
        Err(_) => reporter.clear_line(),
    }
    result
}

pub(crate) fn run_pipeline(
    pipeline: &mut Pipeline,
    units: &[CompilationUnit],
    reporter: &mut ProgressReporter,
) -> CHLResult<()> {
    pipeline
        .run(units.iter().cloned(), |job| reporter.start_job(job))
        .map_err(earliest_failure)
}

pub(crate) fn reachable_units(
    context: &GlobalCompilationContext,
    roots: &[CompilationUnit],
) -> Vec<CompilationUnit> {
    let mut seen = roots.iter().cloned().collect::<FxHashSet<_>>();
    let mut reachable = roots.to_vec();
    let mut index = 0;
    while index < reachable.len() {
        for unit in imported_units(context, &reachable[index]) {
            if seen.insert(unit.clone()) {
                reachable.push(unit);
            }
        }
        index += 1;
    }
    reachable
}

fn earliest_failure(failures: Vec<PipelineFailure>) -> CHLError {
    failures
        .into_iter()
        .min_by_key(|failure| failure.job.step)
        .expect("a failed pipeline run reports at least one failure")
        .error
}

fn finish_artifact(
    context: &GlobalCompilationContext,
    package: PackageId,
    units: &[CompilationUnit],
    reporter: &mut ProgressReporter,
) -> CHLResult<()> {
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
            let (own, dependencies): (Vec<_>, Vec<_>) = reachable_units(context, units)
                .into_iter()
                .partition(|unit| unit.package() == package);
            let own = own.iter().map(|unit| resource_path(context, unit, ".o")).collect::<Vec<_>>();
            let dependencies = dependencies
                .iter()
                .map(|unit| resource_path(context, unit, ".o"))
                .chain(config.native_objects.iter().cloned())
                .collect::<Vec<_>>();
            link_relocatable(&config.output, &own, &dependencies, &config.link_entries)
        }
    }
}

pub(crate) fn root_unit(
    config: &CompilerConfig,
    package: PackageId,
    source: &Path,
) -> CHLResult<CompilationUnit> {
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

fn copy_object(context: &GlobalCompilationContext, unit: &CompilationUnit) -> CHLResult<()> {
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

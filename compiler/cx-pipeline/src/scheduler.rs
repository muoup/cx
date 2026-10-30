use crate::backends::{cranelift_compile, llvm_compile};
use crate::{COMPILER_STACK_SIZE, pipeline_error};
use cx_hir::intrinsic_types::INTRINSIC_IMPORTS;
use cx_hir_lowering::generate_hmir;
use cx_hmir_lowering::{
    env::{HMIREnvironment, settings::HMIRSettings},
    generate_mir,
};
use cx_log::catalogue::driver as catalogue;
use cx_log::{CXResult, error::CXError};
use cx_mir_analysis::{MIRAnalysisOptions, analyze};

use cx_mir_lowering::generate_lmir;
use cx_namespace::module::{ModulePath, NamespacePath, QualifiedName};
use cx_parsing::preparse::PreparseConfig;
use cx_parsing::{parse_ast, preparse};
use cx_pipeline_data::internal_storage::resource_path;
use cx_pipeline_data::jobs::{
    CompilationJob, CompilationJobRequirement, CompilationStep, JobQueue,
};
use cx_pipeline_data::{
    CompilationMode, CompilationUnit, CompilerBackend, GlobalCompilationContext,
};
use cx_preparse_data::Import;
use cx_tokens::TokenIter;
use cx_util::format::{dump_data, dumps_enabled, with_dump_directory, with_dump_file};
use cx_util::identifier::CXIdent;
use fs2::FileExt;
use std::io::Write;
use std::num::NonZeroUsize;
use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};
use std::sync::mpsc;

pub(crate) type JobFailure = (CompilationJob, CXError);

pub(crate) fn scheduling_loop(
    context: &GlobalCompilationContext,
    initial_jobs: impl IntoIterator<Item = CompilationJob>,
    final_step: CompilationStep,
    mut on_start: impl FnMut(&CompilationJob),
) -> Result<(), JobFailure> {
    let mut queue = JobQueue::new();
    for initial_job in initial_jobs {
        queue.push_job(initial_job);
    }

    let workers = std::thread::available_parallelism().map_or(1, NonZeroUsize::get);
    let retain_lmir = context.config.compilation_mode == CompilationMode::Library;
    let dumps = dumps_enabled();
    let (sender, receiver) = mpsc::channel();

    std::thread::scope(|scope| {
        let mut running = 0;
        let mut failures = Vec::new();

        loop {
            while failures.is_empty() && running < workers {
                let Some(job) = queue.pop_ready(|unit| import_units_for_unit(context, unit)) else {
                    break;
                };
                context.module_db.register_unit(&job.unit);
                on_start(&job);

                let sender = sender.clone();
                std::thread::Builder::new()
                    .stack_size(COMPILER_STACK_SIZE)
                    .spawn_scoped(scope, move || {
                        let result = catch_unwind(AssertUnwindSafe(|| {
                            with_dump_directory(
                                context.config.internal_directory.clone(),
                                dumps,
                                || handle_job(context, &job, retain_lmir),
                            )
                        }));
                        let _ = sender.send((job, result));
                    })
                    .expect("failed to spawn a compiler worker thread");
                running += 1;
            }

            if running == 0 {
                break;
            }

            let (job, result) = receiver
                .recv()
                .expect("compiler workers hold a sender until they report");
            running -= 1;

            match result {
                Ok(Ok(next_jobs)) => {
                    queue.complete_job(&job);
                    for next_job in next_jobs.into_iter().filter(|next| next.step <= final_step) {
                        queue.push_new_job(next_job);
                    }
                }
                Ok(Err(error)) => failures.push((job, error)),
                Err(payload) => resume_unwind(payload),
            }
        }

        if let Some(failure) = failures.into_iter().min_by_key(|(job, _)| job.step) {
            return Err(failure);
        }
        let stalled = std::iter::from_fn(|| queue.pop_job()).collect::<Vec<_>>();
        if let Some(job) = stalled.first() {
            let jobs = stalled
                .iter()
                .map(|job| format!("{} ({:?})", job.unit, job.step))
                .collect::<Vec<_>>()
                .join(", ");
            return Err((job.clone(), pipeline_error(&catalogue::SCHEDULER_STALL, jobs)));
        }
        Ok(())
    })
}

fn import_jobs_for_unit(
    context: &GlobalCompilationContext,
    imports: &[Import],
) -> CXResult<Vec<CompilationJob>> {
    let mut jobs = Vec::new();

    for import in import_units(imports, &context.config.working_directory) {
        if !context.config.module_mode && !import.is_std_lib() {
            return Err(pipeline_error(
                &catalogue::SINGLE_FILE_IMPORT,
                format!("{}", import),
            ));
        }

        jobs.push(CompilationJob::new(
            vec![],
            CompilationStep::PreParse,
            import,
        ));
    }

    Ok(jobs)
}

fn import_requirements_for_unit(
    context: &GlobalCompilationContext,
    imports: &[Import],
    step: CompilationStep,
    shallow: bool,
) -> Vec<CompilationJobRequirement> {
    import_units(imports, &context.config.working_directory)
        .map(|import| CompilationJobRequirement {
            unit: import,
            step,
            shallow,
        })
        .collect()
}

fn import_units_for_unit(
    context: &GlobalCompilationContext,
    unit: &CompilationUnit,
) -> Option<Vec<CompilationUnit>> {
    context
        .module_db
        .preparse_base
        .lock()
        .get(&unit.namespace())
        .map(|preparse| {
            import_units(&preparse.imports, &context.config.working_directory).collect()
        })
}

fn import_units<'a>(
    imports: &'a [Import],
    working_directory: &'a std::path::Path,
) -> impl Iterator<Item = CompilationUnit> + 'a {
    imports
        .iter()
        .flat_map(move |import| {
            import.names.iter().map(move |name| {
                let mut path = std::path::PathBuf::new();
                for segment in name.namespace.segments() {
                    path.push(segment.as_str());
                }
                path.push(name.name.as_str());
                let namespace = name.namespace.clone().child(name.name.clone());
                let module_path = if namespace
                    .segments()
                    .first()
                    .is_some_and(|s| s.as_str() == "std")
                {
                    ModulePath::new(std::path::PathBuf::from(
                        cx_namespace::cx_library_directory(&format!(
                            "{}.cx",
                            path.to_string_lossy()
                        )),
                    ))
                } else {
                    ModulePath::new(working_directory.join(path).with_extension("cx"))
                };
                CompilationUnit::new(working_directory, module_path, Some(namespace))
            })
        })
        .collect::<Vec<_>>()
        .into_iter()
}

fn import_from_module_path(path: ModulePath) -> Import {
    let segments = path
        .as_path()
        .components()
        .map(|component| CXIdent::new(component.as_os_str().to_string_lossy().as_ref()))
        .collect::<Vec<_>>();
    let (namespace, name) = NamespacePath::new(segments)
        .parent_and_name()
        .expect("compiler-injected import path should have a name");

    Import {
        names: vec![QualifiedName::new(namespace, name)],
        alias: None,
    }
}

pub(crate) fn handle_job(
    context: &GlobalCompilationContext,
    job: &CompilationJob,
    retain_lmir: bool,
) -> CXResult<Box<[CompilationJob]>> {
    let map_reqs_new_stage = |new_step: CompilationStep, shallow: bool| {
        let new_requirements = job
            .requirements
            .iter()
            .map(|req| {
                CompilationJobRequirement {
                    unit: req.unit.clone(),

                    // requirement for the next step of a standard job is that all imports
                    // have completed the step it has just completed
                    step: job.step,
                    shallow,
                }
            })
            .collect::<Vec<_>>();

        Ok([CompilationJob::new(
            new_requirements,
            new_step,
            job.unit.clone(),
        )]
        .into())
    };

    let result = perform_job_with_dump(context, job, retain_lmir)?;

    match job.step {
        CompilationStep::PreParse => {
            let pp_data = context.module_db.preparse_base.get(&job.unit.namespace());
            let mut new_jobs = import_jobs_for_unit(context, &pp_data.imports)?;

            let mut next_job = job.clone();
            next_job.step = CompilationStep::Parse;
            next_job.requirements = import_requirements_for_unit(
                context,
                &pp_data.imports,
                CompilationStep::PreParse,
                true,
            );
            next_job.compilation_exists =
                matches!(result, JobResult::UnchangedSinceLastCompilation);
            new_jobs.push(next_job);

            Ok(new_jobs.into())
        }
        CompilationStep::Parse => map_reqs_new_stage(CompilationStep::HMIR, false),
        CompilationStep::HMIR => map_reqs_new_stage(CompilationStep::MIR, false),
        CompilationStep::MIR => map_reqs_new_stage(CompilationStep::LMIR, true),
        CompilationStep::LMIR => map_reqs_new_stage(CompilationStep::Codegen, true),
        CompilationStep::Codegen => Ok([].into()),
    }
}

pub(crate) enum JobResult {
    StandardSuccess,

    #[allow(dead_code)]
    UnchangedSinceLastCompilation,
}

fn require_explicit_return(context: &GlobalCompilationContext, unit: &CompilationUnit) -> bool {
    context.config.require_explicit_return.unwrap_or_else(|| {
        unit.module()
            .as_path()
            .extension()
            .and_then(|extension| extension.to_str())
            != Some("c")
    })
}

fn perform_job_with_dump(
    context: &GlobalCompilationContext,
    job: &CompilationJob,
    retain_lmir: bool,
) -> CXResult<JobResult> {
    if !dumps_enabled() {
        return perform_job(context, job, retain_lmir);
    }
    let dump_path = resource_path(context, &job.unit, ".dump");
    if matches!(job.step, CompilationStep::PreParse) {
        std::fs::create_dir_all(dump_path.parent().unwrap()).map_err(|error| {
            pipeline_error(
                &catalogue::FILE_OPERATION,
                (
                    "create".into(),
                    "dump directory".into(),
                    Some(format!("{}", dump_path.parent().unwrap().display())),
                    format!("{}", error),
                ),
            )
        })?;
        std::fs::File::create(&dump_path).map_err(|error| {
            pipeline_error(
                &catalogue::FILE_OPERATION,
                (
                    "create".into(),
                    "dump file".into(),
                    Some(format!("{}", dump_path.display())),
                    format!("{}", error),
                ),
            )
        })?;
    }

    with_dump_file(dump_path, || perform_job(context, job, retain_lmir))
}

pub(crate) fn perform_job(
    context: &GlobalCompilationContext,
    job: &CompilationJob,
    retain_lmir: bool,
) -> CXResult<JobResult> {
    match job.step {
        CompilationStep::PreParse => {
            let file_path = job.unit.module().as_path().to_path_buf();
            let file_contents = std::fs::read_to_string(&file_path).map_err(|error| {
                pipeline_error(
                    &catalogue::FILE_OPERATION,
                    (
                        "read".into(),
                        "source file".into(),
                        Some(format!("{}", file_path.display())),
                        format!("{}", error),
                    ),
                )
            })?;

            // let mut hasher = DefaultHasher::new();
            // file_contents.hash(&mut hasher);

            // let current_hash = hasher.finish().to_string();
            // let previous_hash = retrieve_text(context, &job.unit, ".hash").unwrap_or_default();

            // let identical_hash = previous_hash == current_hash;
            // let object_exists =
            //     std::fs::metadata(internal_directory(context, &job.unit).with_extension("o"))
            //         .is_ok();

            // store_text(context, &job.unit, ".hash", &current_hash);

            let tokens = cx_lexer::lex_with_context(
                file_contents.as_str(),
                &file_path,
                &context.config.include_dirs,
                &context.config.predefined_macros,
            )?;

            let preparse_config = PreparseConfig::from_compiler_config(&context.config);
            let mut output = preparse(
                &preparse_config,
                TokenIter::new(&tokens, file_path.clone()),
                file_path.to_string_lossy().into_owned(),
                job.unit.namespace().clone(),
            )?;

            if !job.unit.is_std_lib() {
                output.imports.extend(
                    INTRINSIC_IMPORTS
                        .iter()
                        .map(|s| import_from_module_path(ModulePath::from_source_path(s))),
                );
            }
            context
                .module_db
                .preparse_registry
                .insert_module(output.module_symbols.clone());
            context
                .module_db
                .lex_tokens
                .insert(job.unit.namespace().clone(), tokens.into_boxed_slice());
            context
                .module_db
                .preparse_base
                .insert(job.unit.namespace().clone(), output);

            return Ok(JobResult::StandardSuccess);

            // FIXME: Cached compilation artifacts aren't currently supported.

            // return if identical_hash && object_exists {
            //     Some(JobResult::UnchangedSinceLastCompilation)
            // } else {
            //     Some(JobResult::StandardSuccess)
            // };
        }

        CompilationStep::Parse => {
            let pp_data = context.module_db.preparse_base.get(job.unit.namespace());
            let lexemes = context.module_db.lex_tokens.get(job.unit.namespace());

            let parsed_ast = parse_ast(
                TokenIter::new(&lexemes, job.unit.module().as_path().to_path_buf()),
                pp_data.as_ref(),
                &context.module_db.preparse_registry,
            )?;

            if !job.unit.is_std_lib() || context.config.verbose {
                dump_data(&parsed_ast);
            }

            context
                .module_db
                .hir
                .insert(job.unit.namespace().clone(), parsed_ast);
        }

        CompilationStep::HMIR => {
            let hir = context.module_db.hir.get(job.unit.namespace());
            let root_exports = context.module_db.exports_at_root(job.unit.namespace());

            let hmir = generate_hmir(
                hir.as_ref(),
                context.module_db.hmir.allocate(),
                job.unit.namespace().clone(),
                root_exports,
            )?;

            if !job.unit.is_std_lib() || context.config.verbose {
                dump_data(&hmir);
            }

            context.module_db.hmir.insert(hmir);
        }

        CompilationStep::MIR => {
            let hmir = context
                .module_db
                .hmir
                .get(job.unit.namespace())
                .expect("HMIR unit not found for its compilation unit");
            let settings = HMIRSettings::new(context.config.architecture)
                .with_explicit_return(require_explicit_return(context, &job.unit))
                .with_assertions(!context.config.unsafe_flags.no_assert);

            let env = HMIREnvironment::new(&context.module_db, hmir.as_ref(), settings);
            let mir = generate_mir(hmir.as_ref(), env)?;

            if !job.unit.is_std_lib() || context.config.verbose {
                dump_data(&mir);
            }

            if !context.config.unsafe_flags.no_analysis {
                analyze(&mir, MIRAnalysisOptions::default())?;
            }

            context
                .module_db
                .mir
                .insert(job.unit.namespace().clone(), mir);
        }

        CompilationStep::LMIR => {
            let mir = context.module_db.mir.take(job.unit.namespace());
            let lmir = generate_lmir(&mir)?;

            if !job.unit.is_std_lib() || context.config.verbose {
                dump_data(&lmir);
            }

            context
                .module_db
                .lmir
                .insert(job.unit.namespace().clone(), lmir);
        }

        CompilationStep::Codegen => {
            let lmir_arc;
            let lmir_owned;
            let lmir: &cx_lmir::LMIRUnit = if retain_lmir {
                lmir_arc = context.module_db.lmir.get(job.unit.namespace());
                &lmir_arc
            } else {
                lmir_owned = context.module_db.lmir.take(job.unit.namespace());
                &lmir_owned
            };
            let internal_directory = resource_path(context, &job.unit, ".o");
            if let Some(parent) = internal_directory.parent() {
                std::fs::create_dir_all(parent).map_err(|error| {
                    pipeline_error(
                        &catalogue::FILE_OPERATION,
                        (
                            "create".into(),
                            "object directory".into(),
                            Some(format!("{}", parent.display())),
                            format!("{}", error),
                        ),
                    )
                })?;
            }
            let internal_directory_str = internal_directory.to_str().ok_or(pipeline_error(
                &catalogue::PATH_ENCODING,
                "internal directory".into(),
            ))?;

            let buffer = match context.config.backend {
                CompilerBackend::LLVM => llvm_compile(
                    lmir,
                    internal_directory_str,
                    context.config.optimization_level,
                ),
                CompilerBackend::Cranelift => cranelift_compile(lmir, internal_directory_str),
            }?;

            let mut file =
                std::fs::File::create(&internal_directory).expect("Failed to create object file");

            file.lock_exclusive()
                .expect("Failed to lock object file for writing");
            file.write_all(&buffer)
                .expect("Failed to write object file");

            context
                .linking_files
                .lock()
                .expect("Deadlock on linking files mutex")
                .insert(internal_directory);
        }
    }

    Ok(JobResult::StandardSuccess)
}

#[derive(Debug, Clone)]
pub enum LSPErrors {
    SpannedError {
        compilation_unit: std::path::PathBuf,
        code: String,
        message: String,
        byte_start: usize,
        byte_end: usize,
        notes: Vec<String>,
    },
    FatalError {
        compilation_unit: std::path::PathBuf,
        code: String,
        message: String,
        line: Option<usize>,
    },
}

impl LSPErrors {
    pub(crate) fn new((job, error): JobFailure) -> Self {
        match error.source_span() {
            Some(span) => LSPErrors::SpannedError {
                compilation_unit: span.file,
                code: error.code(),
                message: error.message(),
                byte_start: span.byte_start,
                byte_end: span.byte_end,
                notes: error.notes().to_vec(),
            },
            None => LSPErrors::FatalError {
                compilation_unit: job.unit.module().as_path().to_path_buf(),
                code: error.code(),
                message: error.message(),
                line: None,
            },
        }
    }
}

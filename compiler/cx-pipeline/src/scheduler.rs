use crate::pipeline_error;
use crate::progress::ProgressReporter;
use cx_log::{CXResult, error::CXErr};
use cx_mir_analysis::{MIRAnalysisOptions, analyze};
use cx_parsing::preparse::PreparseConfig;
use cx_parsing::{decompose_ast, parse_ast, preparse};
use cx_pipeline_data::db::ModuleMap;
use cx_pipeline_data::internal_storage::retrieve_data;
use cx_pipeline_data::jobs::{
    CompilationJob, CompilationJobRequirement, CompilationStep, JobQueue,
};
use cx_pipeline_data::{CompilationUnit, GlobalCompilationContext};
use cx_safe_analyzer::FMIRContext;
use cx_thir::intrinsic_types::INTRINSIC_IMPORTS;
use cx_thir_lowering::generate_mir;
use cx_tokens::TokenIter;
use cx_typechecker::environment::TypeEnvironment;
use cx_typechecker::typecheck;
use cx_util::format::dump_data;
use cx_util::module_path::ModulePath;
use speedy::{LittleEndian, Readable, Writable};
use std::collections::{HashMap, HashSet};

pub(crate) fn scheduling_loop(
    context: &GlobalCompilationContext,
    initial_job: CompilationJob,
    reporter: &mut ProgressReporter,
) -> CXResult<()> {
    let mut queue = JobQueue::new();

    let mut compilation_exists = HashMap::new();

    queue.push_job(initial_job);
    reporter.add_total(1);

    // TODO: Parallelize this loop
    'queue: while !queue.is_empty() {
        let mut job = queue.pop_job().unwrap();
        context.module_db.register_unit(&job.unit);

        compilation_exists.insert(job.unit.clone(), job.compilation_exists);

        if job.compilation_exists {
            if load_precompiled_data(context, &job.unit).is_none() {
                job.compilation_exists = false;
                queue.push_job(job);
                continue;
            }

            for req in job.requirements.iter() {
                match compilation_exists.get(&req.unit) {
                    Some(false) => {
                        job.compilation_exists = false;
                        queue.push_job(job);
                        continue 'queue;
                    }
                    Some(true) => {}
                    _ => {
                        queue.push_job(job);
                        continue 'queue;
                    }
                }
            }

            reporter.skip_step(&job.unit.to_string());
            reporter.complete_step();
            queue.complete_all_unit_jobs(&job.unit);
            context.module_db.set_no_reexport(&job.unit);
            continue;
        }

        if !queue.requirements_complete(&job, |unit| import_units_for_unit(context, unit)) {
            queue.push_job(job);
            continue;
        }

        queue.complete_job(&job);

        let step_name = match job.step {
            CompilationStep::PreParse => "Lexing",
            CompilationStep::Parse => "Parsing",
            CompilationStep::Typechecking => "Typechecking",
            CompilationStep::MIRGen => "MIR generation",
        };
        reporter.start_step(step_name, &job.unit.to_string());

        let is_final = matches!(job.step, CompilationStep::MIRGen);

        for new_jobs in handle_job(context, job)?.into_iter() {
            reporter.add_total(1);
            queue.push_new_job(new_jobs);
        }

        reporter.complete_step();

        if is_final {
            reporter.increment_modules();
        }
    }

    Ok(())
}

fn import_jobs_for_unit(
    context: &GlobalCompilationContext,
    imports: &[ModulePath],
) -> CXResult<Vec<CompilationJob>> {
    let mut jobs = Vec::new();

    for import in imports {
        if !context.module_mode && !import.is_library_module() {
            return Err(pipeline_error(
                "COMPILATION ERROR",
                format!(
                    "Import '{}' is not available in single-file compilation mode. Only compiler library modules under `std::` may be imported here; use `cx build` for project/module imports.",
                    import.as_str().replace('/', "::")
                ),
            ));
        }

        jobs.push(CompilationJob::new(
            vec![],
            CompilationStep::PreParse,
            CompilationUnit::from_module_path(import.clone(), &context.config.working_directory),
        ));
    }

    Ok(jobs)
}

fn import_requirements_for_unit(
    context: &GlobalCompilationContext,
    imports: &[ModulePath],
    step: CompilationStep,
    shallow: bool,
) -> Vec<CompilationJobRequirement> {
    imports
        .iter()
        .map(|import| CompilationJobRequirement {
            unit: CompilationUnit::from_module_path(
                import.clone(),
                &context.config.working_directory,
            ),
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
        .get(&unit.namespace().clone())
        .map(|preparse| {
            preparse
                .imports
                .iter()
                .map(|import| {
                    CompilationUnit::from_module_path(
                        import.clone(),
                        &context.config.working_directory,
                    )
                })
                .collect()
        })
}

pub(crate) fn handle_job(
    context: &GlobalCompilationContext,
    mut job: CompilationJob,
) -> CXResult<Box<[CompilationJob]>> {
    let map_reqs_new_stage = |job: CompilationJob, new_step: CompilationStep, shallow: bool| {
        let new_requirements = job
            .requirements
            .into_iter()
            .map(|req| {
                CompilationJobRequirement {
                    unit: req.unit,

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

    match perform_job(context, &job)? {
        JobResult::StandardSuccess => {}
        JobResult::UnchangedSinceLastCompilation => job.compilation_exists = true,
    };

    match job.step {
        CompilationStep::PreParse => {
            let pp_data = context.module_db.preparse_base.get(&job.unit);
            let mut new_jobs = import_jobs_for_unit(context, &pp_data.imports)?;

            job.step = CompilationStep::Parse;
            job.requirements = import_requirements_for_unit(
                context,
                &pp_data.imports,
                CompilationStep::PreParse,
                true,
            );
            new_jobs.push(job);

            Ok(new_jobs.into())
        }
        CompilationStep::Parse => map_reqs_new_stage(job, CompilationStep::Typechecking, false),
        CompilationStep::Typechecking => map_reqs_new_stage(job, CompilationStep::MIRGen, true),
        CompilationStep::MIRGen => Ok([].into()),
    }
}

fn load_precompiled_data(
    _context: &GlobalCompilationContext,
    _unit: &CompilationUnit,
) -> Option<()> {
    fn _retrieve_map_data<'a, T>(
        context: &GlobalCompilationContext,
        map: &ModuleMap<T>,
        unit: &CompilationUnit,
    ) -> Option<()>
    where
        T: Clone + Readable<'a, LittleEndian> + Writable<LittleEndian>,
    {
        if let Some(data) = retrieve_data::<T>(context, unit, &map.storage_extension) {
            map.insert(unit.clone(), data);
            Some(())
        } else {
            None
        }
    }

    // retrieve_map_data(context, &context.module_db.preparse_incomplete, unit)?;
    // retrieve_map_data(context, &context.module_db.preparse_full, unit)?;

    Some(())
}

pub(crate) enum JobResult {
    StandardSuccess,

    #[allow(dead_code)]
    UnchangedSinceLastCompilation,
}

pub(crate) fn perform_job(
    context: &GlobalCompilationContext,
    job: &CompilationJob,
) -> CXResult<JobResult> {
    match job.step {
        CompilationStep::PreParse => {
            let file_path = job.unit.as_path().to_path_buf();
            let file_contents = std::fs::read_to_string(&file_path).map_err(|error| {
                pipeline_error(
                    "COMPILATION ERROR",
                    format!("Failed to read {}: {error}", file_path.display()),
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
            )?;

            let preparse_config = PreparseConfig::from_compiler_config(&context.config);
            let mut output = preparse(
                &preparse_config,
                TokenIter::new(&tokens, file_path),
                job.unit.to_string(),
                job.unit.namespace().as_namespace_path().clone(),
            )?;

            if !job.unit.is_std_lib() {
                output.imports.extend(
                    INTRINSIC_IMPORTS
                        .iter()
                        .map(|s| ModulePath::from_source_path(s)),
                );
            }
            context
                .module_db
                .preparse_registry
                .insert_module(output.module_symbols.clone());
            context
                .module_db
                .lex_tokens
                .insert(job.unit.clone(), tokens.into_boxed_slice());
            context
                .module_db
                .preparse_base
                .insert(job.unit.clone(), output);

            return Ok(JobResult::StandardSuccess);

            // FIXME: Cached compilation artifacts aren't currently supported.

            // return if identical_hash && object_exists {
            //     Some(JobResult::UnchangedSinceLastCompilation)
            // } else {
            //     Some(JobResult::StandardSuccess)
            // };
        }

        CompilationStep::Parse => {
            let pp_data = context.module_db.preparse_base.get(&job.unit);
            let lexemes = context.module_db.lex_tokens.get(&job.unit);

            let parsed_ast = parse_ast(
                TokenIter::new(&lexemes, job.unit.as_path().to_path_buf()),
                pp_data.as_ref(),
                &context.module_db.preparse_registry,
            )?;

            if !job.unit.is_std_lib() || context.config.verbose {
                dump_data(&parsed_ast);
            }

            let namespace = job.unit.namespace().as_namespace_path().clone();
            let (symbol_buckets, namespace_friends, generation_ast) =
                decompose_ast(&namespace, parsed_ast)?.destructure();

            for (namespace, bucket) in symbol_buckets {
                if let Some((namespace, _)) = context
                    .module_db
                    .symbol_registry
                    .insert_module(namespace, bucket)
                {
                    return Err(pipeline_error(
                        "COMPILATION ERROR",
                        format!(
                            "Duplicate module namespace found during decomposition: {namespace}"
                        ),
                    ));
                }
            }

            for (namespace, friend) in namespace_friends {
                context
                    .module_db
                    .symbol_registry
                    .insert_namespace_friend(namespace, friend);
            }

            context
                .module_db
                .generation_ast
                .insert(job.unit.clone(), generation_ast);
        }

        CompilationStep::Typechecking => {
            let self_ast = context.module_db.generation_ast.get(&job.unit);
            let namespace = job.unit.namespace().clone();

            let mut env = TypeEnvironment::new(&context.module_db, context.config.architecture);

            typecheck(&mut env, &namespace, &self_ast)?;

            let thir = env.finish_thir_unit(namespace)?;
            if !job.unit.is_std_lib() || context.config.verbose {
                dump_data(&thir.display_pretty());
            }

            // There is likely a better way to do this, but for now, we unconditionally generate FMIR no matter if analysis
            // is enabled to have a central source of truth for auditing safe functions for uncontained unsafe behavior.
            let mut fmir_context = FMIRContext::new_from(&thir, &context.module_db)?;

            if !job.unit.is_std_lib() || context.config.verbose {
                dump_data(&fmir_context);
            }

            if !context.config.unsafe_mode {
                fmir_context.apply_standard_analysis_passes()?;
            }

            context.module_db.thir.insert(job.unit.clone(), thir);
        }

        CompilationStep::MIRGen => {
            let thir = context.module_db.thir.get(&job.unit);
            let mir = generate_mir(thir.as_ref())?;
            let analysis = analyze(
                &mir,
                MIRAnalysisOptions {
                    validate: !context.config.unsafe_mode,
                },
            )
            .map_err(|error| {
                pipeline_error("ANALYSIS ERROR", format!("MIR analysis failed: {error}"))
            })?;

            if !job.unit.is_std_lib() || context.config.verbose {
                dump_data(&mir);
                dump_data(&analysis);
            }

            context.module_db.mir.insert(job.unit.clone(), mir);
        }
    }

    Ok(JobResult::StandardSuccess)
}

#[derive(Debug, Clone)]
pub enum LSPErrors {
    SpannedError {
        compilation_unit: std::path::PathBuf,
        message: String,
        byte_start: usize,
        byte_end: usize,
        notes: Vec<String>,
    },
    FatalError {
        compilation_unit: std::path::PathBuf,
        message: String,
        line: Option<usize>,
    },
}

/// Scheduling loop variant for LSP that collects errors instead of panicking.
///
/// This is similar to `scheduling_loop` but:
/// 1. Collects LSPErrors (both type errors and fatal errors) instead of panicking.
/// 2. Stops after Typechecking, before MIR generation.
/// 3. Stops after the first failed stage so dependents cannot observe missing data.
pub(crate) fn scheduling_loop_collect_errors(
    context: &GlobalCompilationContext,
    initial_job: CompilationJob,
    error_collector: &mut Vec<LSPErrors>,
    checked_files: &mut HashSet<std::path::PathBuf>,
) -> Option<()> {
    let mut queue = JobQueue::new();

    queue.push_job(initial_job);

    // TODO: Parallelize this loop
    while !queue.is_empty() {
        let job = queue.pop_job().unwrap();

        context.module_db.register_unit(&job.unit);
        // Skip incremental compilation logic for LSP - always recompile
        if !queue.requirements_complete(&job, |unit| import_units_for_unit(context, unit)) {
            queue.push_job(job);
            continue;
        }

        if matches!(job.step, CompilationStep::MIRGen) {
            continue;
        }

        checked_files.insert(job.unit.as_path().to_path_buf());
        match handle_job_collect_errors(context, &job, error_collector)? {
            HandleJobResult::Success(new_jobs) => {
                queue.complete_job(&job);
                for new_job in new_jobs {
                    queue.push_new_job(new_job);
                }
            }
            HandleJobResult::Failed => {
                // Continuing after a failed stage lets dependent jobs observe missing
                // intermediate data. Stop this check and report the original error.
                break;
            }
        }
    }

    Some(())
}

/// Result type for handle_job_collect_errors
enum HandleJobResult {
    Success(Box<[CompilationJob]>),
    Failed,
}

/// Handle a single job, collecting errors instead of panicking.
///
/// Returns either new jobs to enqueue or Failed if the current stage had errors.
fn handle_job_collect_errors(
    context: &GlobalCompilationContext,
    job: &CompilationJob,
    error_collector: &mut Vec<LSPErrors>,
) -> Option<HandleJobResult> {
    let map_reqs_new_stage = |new_step: CompilationStep, shallow: bool| -> Box<[CompilationJob]> {
        let new_requirements = job
            .requirements
            .iter()
            .map(|req| CompilationJobRequirement {
                unit: req.unit.clone(),
                step: job.step,
                shallow,
            })
            .collect::<Vec<_>>();

        [CompilationJob::new(
            new_requirements,
            new_step,
            job.unit.clone(),
        )]
        .into()
    };

    fn spanned_error(error: &CXErr) -> Option<LSPErrors> {
        let span = error.source_span()?;
        Some(LSPErrors::SpannedError {
            compilation_unit: span.file,
            message: error.message(),
            byte_start: span.byte_start,
            byte_end: span.byte_end,
            notes: vec![],
        })
    }

    // Perform the job and collect errors
    match perform_job(context, job) {
        Ok(_) => {}
        Err(e) => {
            let lsp_error = spanned_error(&e).unwrap_or(LSPErrors::FatalError {
                compilation_unit: job.unit.as_path().to_path_buf(),
                message: e.message(),
                line: None,
            });

            error_collector.push(lsp_error);
            return Some(HandleJobResult::Failed);
        }
    }

    // Generate next jobs based on the completed step
    match job.step {
        CompilationStep::PreParse => {
            let pp_data = context.module_db.preparse_base.get(&job.unit);

            let mut new_jobs = match import_jobs_for_unit(context, &pp_data.imports) {
                Ok(jobs) => jobs,
                Err(e) => {
                    let lsp_error = spanned_error(&e).unwrap_or(LSPErrors::FatalError {
                        compilation_unit: job.unit.as_path().to_path_buf(),
                        message: e.message(),
                        line: None,
                    });
                    error_collector.push(lsp_error);
                    return Some(HandleJobResult::Failed);
                }
            };

            // Add the next step for this job
            let mut next_job = job.clone();
            next_job.step = CompilationStep::Parse;
            next_job.requirements = import_requirements_for_unit(
                context,
                &pp_data.imports,
                CompilationStep::PreParse,
                true,
            );
            new_jobs.push(next_job);

            Some(HandleJobResult::Success(new_jobs.into()))
        }

        CompilationStep::Parse => Some(HandleJobResult::Success(map_reqs_new_stage(
            CompilationStep::Typechecking,
            false,
        ))),

        CompilationStep::Typechecking => {
            // Stop here for LSP - no need for MIR generation.
            Some(HandleJobResult::Success([].into()))
        }
        CompilationStep::MIRGen => Some(HandleJobResult::Success([].into())),
    }
}

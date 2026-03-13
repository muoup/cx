use crate::backends::{cranelift_compile, llvm_compile};
use crate::template_realizing::realize_templates;
use cx_ast::ast::VisibilityMode;
use cx_mir::intrinsic_types::INTRINSIC_IMPORTS;
use cx_mir_lowering::generate_lmir;
use cx_parsing::ParseErrorLog;
use cx_parsing::parse::parse_ast;
use cx_parsing::preparse::preparse;
use cx_pipeline_data::db::ModuleMap;
use cx_pipeline_data::directories::internal_directory;
use cx_pipeline_data::internal_storage::{resource_path, retrieve_data, retrieve_text, store_text};
use cx_pipeline_data::jobs::{
    CompilationJob, CompilationJobRequirement, CompilationStep, JobQueue,
};
use cx_pipeline_data::{CompilationUnit, CompilerBackend, GlobalCompilationContext};
use cx_safe_analyzer::FMIRContext;
use cx_tokens::TokenIter;
use cx_typechecker::environment::TypeEnvironment;
use cx_typechecker::gather_interface;
use cx_typechecker::log::TypeError;
use cx_typechecker::{complete_base_functions, complete_base_globals, typecheck};
use cx_util::CXErrorTrait;
use cx_util::format::dump_data;
use fs2::FileExt;
use speedy::{LittleEndian, Readable, Writable};
use std::collections::HashMap;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::io::Write;

pub(crate) fn scheduling_loop(
    context: &GlobalCompilationContext,
    initial_job: CompilationJob,
) -> Option<()> {
    let mut queue = JobQueue::new();

    let mut compilation_exists = HashMap::new();

    queue.push_job(initial_job);

    // TODO: Parallelize this loop
    'queue: while !queue.is_empty() {
        let mut job = queue.pop_job().unwrap();

        compilation_exists.insert(job.unit.clone(), job.compilation_exists);

        if job.compilation_exists {
            if load_precompiled_data(context, &job.unit).is_none() {
                job.compilation_exists = false;
                queue.push_job(job);
                continue;
            }

            for req in job.requirements.iter() {
                match compilation_exists.get(&req.unit) {
                    // previous compilation does not exist for a dependency, so this job must be recompiled
                    Some(false) => {
                        job.compilation_exists = false;
                        queue.push_job(job);
                        continue 'queue;
                    }

                    // previous compilation exists for a dependency, if all other dependencies are the same
                    // we may skip this job
                    Some(true) => {}

                    // all dependencies have not been processed yet, so we must check again later
                    _ => {
                        queue.push_job(job);
                        continue 'queue;
                    }
                }
            }

            println!(
                "Skipping job: {} at step: {:?} as it has already been compiled",
                job.unit, job.step
            );
            queue.complete_all_unit_jobs(&job.unit);
            context.module_db.set_no_reexport(&job.unit);
            context
                .linking_files
                .lock()
                .expect("Deadlock on linking files mutex")
                .insert(resource_path(context, &job.unit, ".o"));
            continue;
        }

        if !queue.requirements_complete(&job) {
            queue.push_job(job);
            continue;
        }

        queue.complete_job(&job);

        for new_jobs in handle_job(context, job)?.into_iter() {
            queue.push_new_job(new_jobs);
        }
    }

    Some(())
}

pub(crate) fn handle_job(
    context: &GlobalCompilationContext,
    mut job: CompilationJob,
) -> Option<Box<[CompilationJob]>> {
    let map_reqs_new_stage = |job: CompilationJob, new_step: CompilationStep| {
        let new_requirements = job
            .requirements
            .into_iter()
            .map(|req| {
                CompilationJobRequirement {
                    unit: req.unit,

                    // requirement for the next step of a standard job is that all imports
                    // have completed the step it has just completed
                    step: job.step,
                }
            })
            .collect::<Vec<_>>();

        Some(
            [CompilationJob::new(
                new_requirements,
                new_step,
                job.unit.clone(),
            )]
            .into(),
        )
    };

    match perform_job(context, &job)? {
        JobResult::StandardSuccess => {}
        JobResult::UnchangedSinceLastCompilation => job.compilation_exists = true,
    };

    match job.step {
        CompilationStep::PreParse => {
            let pp_data = context.module_db.preparse_base.get(&job.unit);

            let mut new_jobs = pp_data
                .imports
                .iter()
                .map(|import| {
                    CompilationJob::new(
                        vec![],
                        CompilationStep::PreParse,
                        CompilationUnit::from_rooted(
                            import.as_str(),
                            &context.config.working_directory,
                        ),
                    )
                })
                .collect::<Vec<_>>();

            job.step = CompilationStep::ASTParse;
            new_jobs.push(job);

            Some(new_jobs.into())
        }
        CompilationStep::ASTParse => map_reqs_new_stage(job, CompilationStep::InterfaceCombine),
        CompilationStep::InterfaceCombine => map_reqs_new_stage(job, CompilationStep::Typechecking),
        CompilationStep::Typechecking => map_reqs_new_stage(job, CompilationStep::LMIRGen),
        CompilationStep::LMIRGen => map_reqs_new_stage(job, CompilationStep::Codegen),
        CompilationStep::Codegen => Some([].into()),
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
            println!(
                "Failed to retrieve data for unit: {} with storage extension: {}",
                unit, map.storage_extension
            );
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
) -> Option<JobResult> {
    match job.step {
        CompilationStep::PreParse => {
            let file_path = job.unit.with_extension("cx");
            let file_contents = std::fs::read_to_string(&file_path)
                .unwrap_or_else(|_| panic!("File not found: {}", job.unit));

            let mut hasher = DefaultHasher::new();
            file_contents.hash(&mut hasher);

            let current_hash = hasher.finish().to_string();
            let previous_hash = retrieve_text(context, &job.unit, ".hash").unwrap_or_default();

            let _identical_hash = previous_hash == current_hash;
            let _object_exists =
                std::fs::metadata(internal_directory(context, &job.unit).with_extension("o"))
                    .is_ok();

            store_text(context, &job.unit, ".hash", &current_hash);

            let tokens = cx_lexer::lex(file_contents.as_str())?;

            let mut output = preparse(TokenIter::new(&tokens, file_path)).unwrap_or_else(|e| {
                e.pretty_print();
                panic!("Pre-parsing failed for unit: {}", job.unit);
            });
            output.module = job.unit.to_string();

            if !job.unit.as_str().contains("/std/") {
                output
                    .imports
                    .extend(INTRINSIC_IMPORTS.iter().map(|s| s.to_string()));
            }

            context
                .module_db
                .lex_tokens
                .insert(job.unit.clone(), tokens);
            context
                .module_db
                .preparse_base
                .insert(job.unit.clone(), output);

            return Some(JobResult::StandardSuccess);

            // FIXME: Cached compilation artifacts aren't currently supported.

            // return if identical_hash && object_exists {
            //     Some(JobResult::UnchangedSinceLastCompilation)
            // } else {
            //     Some(JobResult::StandardSuccess)
            // };
        }

        CompilationStep::ASTParse => {
            let mut pp_data = context.module_db.preparse_base.get_cloned(&job.unit);
            let lexemes = context.module_db.lex_tokens.get(&job.unit);

            for import in pp_data.imports.iter() {
                let other_pp_data =
                    context
                        .module_db
                        .preparse_base
                        .get(&CompilationUnit::from_rooted(
                            import.as_str(),
                            &context.config.working_directory,
                        ));
                let required_visiblity = VisibilityMode::Public;

                for resource in other_pp_data.type_idents.iter() {
                    if resource.visibility < required_visiblity {
                        continue;
                    };

                    pp_data.type_idents.push(resource.transfer(import));
                }
            }

            let parsed_ast = parse_ast(
                TokenIter::new(&lexemes, job.unit.with_extension("cx")),
                &pp_data,
            )
            .unwrap_or_else(|e| {
                e.pretty_print();
                panic!("AST parsing failed for unit: {}", job.unit);
            });

            if !job.unit.is_std_lib() {
                dump_data(&parsed_ast);
            }

            context
                .module_db
                .naive_ast
                .insert(job.unit.clone(), parsed_ast);
        }

        CompilationStep::InterfaceCombine => {
            gather_interface(context, &job.unit).unwrap_or_else(|e| {
                e.pretty_print();
                panic!("Interface combining failed for unit: {}", job.unit);
            });
        }

        CompilationStep::Typechecking => {
            let structure_data = context.module_db.base_mappings.get(&job.unit);
            let self_ast = context.module_db.naive_ast.get(&job.unit);
            let lexemes = context.module_db.lex_tokens.get(&job.unit);

            let mut env = TypeEnvironment::new(
                lexemes.as_ref(),
                job.unit.clone(),
                context.config.working_directory.clone(),
                &context.module_db,
            );

            complete_base_globals(&mut env, structure_data.as_ref()).unwrap_or_else(
                |e: Box<dyn CXErrorTrait>| {
                    e.pretty_print();
                    panic!("Completing base globals failed");
                },
            );
            complete_base_functions(&mut env, structure_data.as_ref()).unwrap_or_else(
                |e: Box<dyn CXErrorTrait>| {
                    e.pretty_print();
                    panic!("Completing base functions failed");
                },
            );
            typecheck(&mut env, structure_data.as_ref(), &self_ast).unwrap_or_else(
                |e: Box<dyn CXErrorTrait>| {
                    e.pretty_print();
                    panic!("Typechecking failed for unit: {}", job.unit);
                },
            );
            realize_templates(&job.unit, &mut env).unwrap_or_else(|e| {
                e.pretty_print();
                panic!("Template realization failed for unit: {}", job.unit);
            });

            let mir = env.finish_mir_unit().unwrap_or_else(|e| {
                e.pretty_print();
                panic!("MIR generation failed for unit: {}", job.unit);
            });

            if !job.unit.is_std_lib() {
                dump_data(&mir);
            }

            if context.config.analysis {
                let mut fmir_context = FMIRContext::new_from(&mir).unwrap_or_else(|e| {
                    e.pretty_print();
                    panic!("FMIR generation failed for unit: {}", job.unit);
                });

                if !job.unit.is_std_lib() {
                    dump_data(&fmir_context);
                }

                fmir_context
                    .apply_standard_analysis_passes(job.unit.as_path())
                    .unwrap_or_else(|e| {
                        e.pretty_print();
                        panic!("FMIR analysis failed for unit: {}", job.unit);
                    });
            }

            context.module_db.mir.insert(job.unit.clone(), mir);
        }

        CompilationStep::LMIRGen => {
            let mir = context.module_db.mir.take(&job.unit);
            let bc = match generate_lmir(&mir) {
                Ok(bc) => bc,
                Err(e) => {
                    e.pretty_print();
                    panic!("LMIR generation failed for unit: {}", job.unit);
                }
            };

            if !job.unit.is_std_lib() {
                dump_data(&bc);
            }

            context.module_db.lmir.insert(job.unit.clone(), bc);
        }

        CompilationStep::Codegen => {
            let lmir = context.module_db.lmir.take(&job.unit);
            let internal_directory = internal_directory(context, &job.unit).with_extension("o");

            let buffer = match context.config.backend {
                CompilerBackend::LLVM => llvm_compile(
                    &lmir,
                    internal_directory.to_str()?,
                    context.config.optimization_level,
                )
                .expect("LLVM code generation failed"),
                CompilerBackend::Cranelift => {
                    cranelift_compile(&lmir, internal_directory.to_str()?)
                        .expect("Cranelift code generation failed")
                }
            };

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

    Some(JobResult::StandardSuccess)
}

/// Error type for LSP that includes both type errors and fatal errors
#[derive(Debug, Clone)]
pub enum LSPErrorSpan {
    TokenRange { start: usize, end: usize },
    ByteRange { start: usize, end: usize },
}

#[derive(Debug, Clone)]
pub enum LSPErrors {
    TypeError(TypeError),
    SpannedError {
        compilation_unit: std::path::PathBuf,
        message: String,
        span: LSPErrorSpan,
        notes: Vec<String>,
    },
    FatalError {
        compilation_unit: std::path::PathBuf,
        message: String,
        line: Option<usize>,
    },
}

/// Result type for jobs that can collect errors instead of panicking
pub(crate) enum JobResultCollect {
    StandardSuccess,
    FatalError(LSPErrors),
}

/// Scheduling loop variant for LSP that collects errors instead of panicking.
///
/// This is similar to `scheduling_loop` but:
/// 1. Collects LSPErrors (both type errors and fatal errors) instead of panicking
/// 2. Stops after Typechecking (no LMIRGen or Codegen)
/// 3. Continues processing other jobs even when errors are found
pub(crate) fn scheduling_loop_collect_errors(
    context: &GlobalCompilationContext,
    initial_job: CompilationJob,
    error_collector: &mut Vec<LSPErrors>,
) -> Option<()> {
    let mut queue = JobQueue::new();
    let mut compilation_exists = HashMap::new();

    queue.push_job(initial_job);

    // TODO: Parallelize this loop
    while !queue.is_empty() {
        let job = queue.pop_job().unwrap();

        compilation_exists.insert(job.unit.clone(), job.compilation_exists);

        // Skip incremental compilation logic for LSP - always recompile
        if !queue.requirements_complete(&job) {
            queue.push_job(job);
            continue;
        }

        queue.complete_job(&job);

        // Stop after Typechecking for LSP
        if matches!(
            job.step,
            CompilationStep::LMIRGen | CompilationStep::Codegen
        ) {
            continue;
        }

        match handle_job_collect_errors(context, &job, error_collector)? {
            HandleJobResult::Success(new_jobs) => {
                for new_job in new_jobs {
                    queue.push_new_job(new_job);
                }
            }
            HandleJobResult::Continue => {
                // Job had errors but continue processing other jobs
            }
        }
    }

    Some(())
}

/// Result type for handle_job_collect_errors
enum HandleJobResult {
    Success(Box<[CompilationJob]>),
    Continue,
}

/// Handle a single job, collecting errors instead of panicking.
///
/// Returns either new jobs to enqueue or Continue if the job had errors
/// but we should continue processing other jobs.
fn handle_job_collect_errors(
    context: &GlobalCompilationContext,
    job: &CompilationJob,
    error_collector: &mut Vec<LSPErrors>,
) -> Option<HandleJobResult> {
    let map_reqs_new_stage = |new_step: CompilationStep| -> Box<[CompilationJob]> {
        let new_requirements = job
            .requirements
            .iter()
            .map(|req| CompilationJobRequirement {
                unit: req.unit.clone(),
                step: job.step,
            })
            .collect::<Vec<_>>();

        [CompilationJob::new(
            new_requirements,
            new_step,
            job.unit.clone(),
        )]
        .into()
    };

    // Perform the job and collect errors
    match perform_job_collect_errors(context, job) {
        JobResultCollect::StandardSuccess => {}
        JobResultCollect::FatalError(e) => {
            error_collector.push(e);
            return Some(HandleJobResult::Continue);
        }
    }

    // Generate next jobs based on the completed step
    match job.step {
        CompilationStep::PreParse => {
            let pp_data = context.module_db.preparse_base.get(&job.unit);

            let mut new_jobs: Vec<CompilationJob> = pp_data
                .imports
                .iter()
                .map(|import| {
                    CompilationJob::new(
                        vec![],
                        CompilationStep::PreParse,
                        CompilationUnit::from_rooted(
                            import.as_str(),
                            &context.config.working_directory,
                        ),
                    )
                })
                .collect();

            // Add the next step for this job
            let mut next_job = job.clone();
            next_job.step = CompilationStep::ASTParse;
            next_job.requirements = Vec::new();
            new_jobs.push(next_job);

            Some(HandleJobResult::Success(new_jobs.into()))
        }
        CompilationStep::ASTParse => Some(HandleJobResult::Success(map_reqs_new_stage(
            CompilationStep::InterfaceCombine,
        ))),
        CompilationStep::InterfaceCombine => Some(HandleJobResult::Success(map_reqs_new_stage(
            CompilationStep::Typechecking,
        ))),
        CompilationStep::Typechecking => {
            // Stop here for LSP - no need for bytecode/codegen
            Some(HandleJobResult::Success([].into()))
        }
        CompilationStep::LMIRGen | CompilationStep::Codegen => {
            Some(HandleJobResult::Success([].into()))
        }
    }
}

/// Perform a single compilation job, collecting errors instead of panicking.
///
/// This is a variant of `perform_job` that returns errors instead of panicking.
fn perform_job_collect_errors(
    context: &GlobalCompilationContext,
    job: &CompilationJob,
) -> JobResultCollect {
    fn line_start_byte(contents: &str, index: usize) -> usize {
        let safe_index = index.min(contents.len());
        contents[..safe_index]
            .rfind('\n')
            .map(|idx| idx + 1)
            .unwrap_or(0)
    }

    fn line_content_start_byte(contents: &str, index: usize) -> usize {
        let line_start = line_start_byte(contents, index);
        let line_end = contents[line_start..]
            .find('\n')
            .map(|offset| line_start + offset)
            .unwrap_or(contents.len());
        let line = &contents[line_start..line_end];
        let first_non_whitespace = line
            .char_indices()
            .find(|(_, ch)| !ch.is_whitespace())
            .map(|(offset, _)| offset)
            .unwrap_or(0);

        line_start + first_non_whitespace
    }

    fn spanned_error(error: &dyn CXErrorTrait) -> Option<LSPErrors> {
        if let Some(parse_error) = error.as_any().downcast_ref::<ParseErrorLog>() {
            let file_contents = std::fs::read_to_string(&parse_error.file).ok()?;
            let anchor_token = parse_error
                .previous_token
                .as_ref()
                .unwrap_or(&parse_error.token);
            let start = line_content_start_byte(&file_contents, anchor_token.start_index);
            let end = anchor_token
                .end_index
                .max(anchor_token.start_index.saturating_add(1));

            return Some(LSPErrors::SpannedError {
                compilation_unit: parse_error.file.clone(),
                message: parse_error.message.clone(),
                span: LSPErrorSpan::ByteRange { start, end },
                notes: Vec::new(),
            });
        }

        if let (Some(compilation_unit), Some(token_start), Some(token_end)) = (
            error.compilation_unit(),
            error.token_start(),
            error.token_end(),
        ) {
            return Some(LSPErrors::SpannedError {
                compilation_unit,
                message: error.error_message(),
                span: LSPErrorSpan::TokenRange {
                    start: token_start,
                    end: token_end,
                },
                notes: error.notes(),
            });
        }

        None
    }

    match job.step {
        CompilationStep::PreParse => {
            let file_path = job.unit.with_extension("cx");
            let file_contents = match std::fs::read_to_string(&file_path) {
                Ok(c) => c,
                Err(e) => {
                    return JobResultCollect::FatalError(LSPErrors::FatalError {
                        compilation_unit: file_path.clone(),
                        message: format!("Failed to read file: {}", e),
                        line: None,
                    });
                }
            };

            let tokens = match cx_lexer::lex(&file_contents) {
                Some(t) => t,
                None => {
                    return JobResultCollect::FatalError(LSPErrors::FatalError {
                        compilation_unit: file_path.clone(),
                        message: "Lexing failed: unknown error".to_string(),
                        line: None,
                    });
                }
            };

            let mut output = match preparse(TokenIter::new(&tokens, file_path.clone())) {
                Ok(p) => p,
                Err(e) => {
                    return JobResultCollect::FatalError(spanned_error(e.as_ref()).unwrap_or(
                        LSPErrors::FatalError {
                            compilation_unit: file_path.clone(),
                            message: format!("Pre-parsing failed: {}", e.error_message()),
                            line: None,
                        },
                    ));
                }
            };
            output.module = job.unit.to_string();

            if !job.unit.as_str().contains("/std/") {
                output
                    .imports
                    .extend(INTRINSIC_IMPORTS.iter().map(|s| s.to_string()));
            }

            context
                .module_db
                .lex_tokens
                .insert(job.unit.clone(), tokens);
            context
                .module_db
                .preparse_base
                .insert(job.unit.clone(), output);

            JobResultCollect::StandardSuccess
        }

        CompilationStep::ASTParse => {
            let mut pp_data = context.module_db.preparse_base.get_cloned(&job.unit);
            let lexemes = context.module_db.lex_tokens.get(&job.unit);

            // Combine imports
            for import in pp_data.imports.iter() {
                let other_pp_data =
                    context
                        .module_db
                        .preparse_base
                        .get(&CompilationUnit::from_rooted(
                            import.as_str(),
                            &context.config.working_directory,
                        ));
                let required_visiblity = VisibilityMode::Public;

                for resource in other_pp_data.type_idents.iter() {
                    if resource.visibility < required_visiblity {
                        continue;
                    }

                    pp_data.type_idents.push(resource.transfer(import));
                }
            }

            let parsed_ast = match parse_ast(
                TokenIter::new(&lexemes, job.unit.with_extension("cx")),
                &pp_data,
            ) {
                Ok(ast) => ast,
                Err(e) => {
                    let file_path = job.unit.with_extension("cx");
                    return JobResultCollect::FatalError(spanned_error(e.as_ref()).unwrap_or(
                        LSPErrors::FatalError {
                            compilation_unit: file_path,
                            message: format!("AST parsing failed: {}", e.error_message()),
                            line: None,
                        },
                    ));
                }
            };

            context
                .module_db
                .naive_ast
                .insert(job.unit.clone(), parsed_ast);

            JobResultCollect::StandardSuccess
        }

        CompilationStep::InterfaceCombine => {
            match gather_interface(context, &job.unit) {
                Ok(_) => {}
                Err(e) => {
                    return JobResultCollect::FatalError(LSPErrors::FatalError {
                        compilation_unit: job.unit.as_path().to_path_buf(),
                        message: format!("Interface combining failed: {}", e.error_message()),
                        line: None,
                    });
                }
            }
            JobResultCollect::StandardSuccess
        }

        CompilationStep::Typechecking => {
            let structure_data = context.module_db.base_mappings.get(&job.unit);
            let self_ast = context.module_db.naive_ast.get(&job.unit);
            let lexemes = context.module_db.lex_tokens.get(&job.unit);

            let mut env = TypeEnvironment::new(
                lexemes.as_ref(),
                job.unit.clone(),
                context.config.working_directory.clone(),
                &context.module_db,
            );

            // Collect errors from each typecheck stage
            match complete_base_globals(&mut env, structure_data.as_ref()) {
                Ok(_) => {}
                Err(e) => {
                    return JobResultCollect::FatalError(spanned_error(e.as_ref()).unwrap_or(
                        LSPErrors::FatalError {
                            compilation_unit: job.unit.as_path().to_path_buf(),
                            message: e.error_message(),
                            line: None,
                        },
                    ));
                }
            }

            match complete_base_functions(&mut env, structure_data.as_ref()) {
                Ok(_) => {}
                Err(e) => {
                    return JobResultCollect::FatalError(spanned_error(e.as_ref()).unwrap_or(
                        LSPErrors::FatalError {
                            compilation_unit: job.unit.as_path().to_path_buf(),
                            message: e.error_message(),
                            line: None,
                        },
                    ));
                }
            }

            match typecheck(&mut env, structure_data.as_ref(), &self_ast) {
                Ok(_) => {}
                Err(e) => {
                    return JobResultCollect::FatalError(spanned_error(e.as_ref()).unwrap_or(
                        LSPErrors::FatalError {
                            compilation_unit: job.unit.as_path().to_path_buf(),
                            message: e.error_message(),
                            line: None,
                        },
                    ));
                }
            }

            match realize_templates(&job.unit, &mut env) {
                Ok(_) => {}
                Err(e) => {
                    return JobResultCollect::FatalError(spanned_error(e.as_ref()).unwrap_or(
                        LSPErrors::FatalError {
                            compilation_unit: job.unit.as_path().to_path_buf(),
                            message: e.error_message(),
                            line: None,
                        },
                    ));
                }
            }
            JobResultCollect::StandardSuccess
        }

        // These steps are not executed for LSP
        CompilationStep::LMIRGen | CompilationStep::Codegen => JobResultCollect::StandardSuccess,
    }
}

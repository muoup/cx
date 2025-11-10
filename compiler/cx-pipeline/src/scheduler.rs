use crate::backends::{cranelift_compile, llvm_compile};
use crate::template_realizing::realize_templates;
use cx_lexer_data::TokenIter;
use cx_mir::generate_bytecode;
use cx_parsing::parse::parse_ast;
use cx_parsing::preparse::preparse;
use cx_parsing_data::ast::VisibilityMode;
use cx_pipeline_data::db::ModuleMap;
use cx_pipeline_data::directories::internal_directory;
use cx_pipeline_data::internal_storage::{resource_path, retrieve_data, retrieve_text, store_text};
use cx_pipeline_data::jobs::{
    CompilationJob, CompilationJobRequirement, CompilationStep, JobQueue,
};
use cx_pipeline_data::{CompilationUnit, CompilerBackend, GlobalCompilationContext};
use cx_typechecker::environment::TCEnvironment;
use cx_typechecker::type_checking::{complete_base_functions, complete_base_globals, typecheck};
use cx_typechecker::gather_interface;
use cx_typechecker_data::ast::TCAST;
use cx_typechecker_data::intrinsic_types::INTRINSIC_IMPORTS;
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
                        CompilationUnit::from_str(import.as_str()),
                    )
                })
                .collect::<Vec<_>>();

            job.step = CompilationStep::ASTParse;
            new_jobs.push(job);

            Some(new_jobs.into())
        }
        CompilationStep::ASTParse => map_reqs_new_stage(job, CompilationStep::InterfaceCombine),
        CompilationStep::InterfaceCombine => {
            map_reqs_new_stage(job, CompilationStep::Typechecking)
        }
        CompilationStep::Typechecking => map_reqs_new_stage(job, CompilationStep::BytecodeGen),
        CompilationStep::BytecodeGen => map_reqs_new_stage(job, CompilationStep::Codegen),
        CompilationStep::Codegen => Some([].into()),
    }
}

fn load_precompiled_data(_context: &GlobalCompilationContext, _unit: &CompilationUnit) -> Option<()> {
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

            let identical_hash = previous_hash == current_hash;
            let object_exists =
                std::fs::metadata(internal_directory(context, &job.unit).with_extension("o"))
                    .is_ok();

            store_text(context, &job.unit, ".hash", &current_hash);

            let tokens = cx_lexer::lex(file_contents.as_str())?;

            let mut output = preparse(TokenIter::new(&tokens, file_path))
                .unwrap_or_else(|e| {
                    e.pretty_print();
                    panic!("Pre-parsing failed for unit: {}", job.unit);
                });
            output.module = job.unit.to_string();

            if !job.unit.as_str().contains("std") {
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

            return if identical_hash && object_exists {
                Some(JobResult::UnchangedSinceLastCompilation)
            } else {
                Some(JobResult::StandardSuccess)
            };
        }

        CompilationStep::ASTParse => {
            let mut pp_data = context.module_db.preparse_base.get_cloned(&job.unit);
            let lexemes = context.module_db.lex_tokens.get(&job.unit);

            for import in pp_data.imports.iter() {
                let other_pp_data = context
                    .module_db
                    .preparse_base
                    .get(&CompilationUnit::from_str(import.as_str()));
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
            ).unwrap_or_else(|e| {
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
            gather_interface(context, &job.unit)
                .unwrap_or_else(|e| {
                    e.pretty_print();
                    panic!("Interface combining failed for unit: {}", job.unit);
                });
        }

        CompilationStep::Typechecking => {
            let structure_data = context.module_db.base_mappings.get(&job.unit);
            let self_ast = context.module_db.naive_ast.get(&job.unit);
            let lexemes = context.module_db.lex_tokens.get(&job.unit);

            let mut env = TCEnvironment::new(
                lexemes.as_ref(),
                job.unit.clone(),
                &context.module_db,
            );
            
            complete_base_globals(&mut env, structure_data.as_ref())
                .unwrap_or_else(|e| {
                    e.pretty_print();
                    panic!("Completing base globals failed");
                });
            complete_base_functions(&mut env, structure_data.as_ref())
                .unwrap_or_else(|e| {
                    e.pretty_print();
                    panic!("Completing base functions failed");
                });
            typecheck(&mut env, structure_data.as_ref(), &self_ast)
                .unwrap_or_else(|e| {
                    e.pretty_print();
                    panic!("Typechecking failed for unit: {}", job.unit);
                });
            realize_templates(&job.unit, &mut env)
                .unwrap_or_else(|e| {
                    e.pretty_print();
                    panic!("Template realization failed for unit: {}", job.unit);
                });

            let tc_ast = TCAST {
                source_file: self_ast.file_path.clone(),

                type_map: env.realized_types,
                fn_map: env.realized_fns,
                global_variables: env.realized_globals.into_values().collect(),

                function_defs: env.declared_functions,
            };

            if !job.unit.is_std_lib() {
                dump_data(&tc_ast);
            }

            context
                .module_db
                .typechecked_ast
                .insert(job.unit.clone(), tc_ast);
        }

        CompilationStep::BytecodeGen => {
            let tc_ast = context.module_db.typechecked_ast.take(&job.unit);

            let bytecode = generate_bytecode(tc_ast).expect("Bytecode generation failed");

            if !job.unit.is_std_lib() {
                dump_data(&bytecode);
            }

            context
                .module_db
                .bytecode
                .insert(job.unit.clone(), bytecode);
        }

        CompilationStep::Codegen => {
            let bytecode = context.module_db.bytecode.take(&job.unit);
            let mut internal_directory = internal_directory(context, &job.unit);
            internal_directory.push(".o");

            let buffer = match context.config.backend {
                CompilerBackend::LLVM => llvm_compile(
                    &bytecode,
                    internal_directory.to_str()?,
                    context.config.optimization_level,
                )
                .expect("LLVM code generation failed"),

                CompilerBackend::Cranelift => {
                    cranelift_compile(&bytecode, internal_directory.to_str()?)
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

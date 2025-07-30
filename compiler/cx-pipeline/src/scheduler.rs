use std::collections::{HashMap, HashSet, VecDeque};
use std::path::PathBuf;
use cx_compiler_ast::lex::lex;
use cx_compiler_ast::parse::parse_ast;
use cx_compiler_ast::preparse::preparse;
use cx_compiler_ast::preprocessor::preprocess;
use cx_compiler_bytecode::generate_bytecode;
use cx_compiler_typechecker::type_check;
use cx_compiler_typechecker::typemap_collapsing::collapse_typemap;
use cx_data_ast::parse::ast::CXAST;
use cx_data_ast::parse::parser::{TokenIter, VisibilityMode};
use cx_data_pipeline::{CompilationUnit, CompilerBackend, GlobalCompilationContext};
use cx_data_pipeline::directories::{file_path, internal_directory};
use cx_data_pipeline::jobs::{CompilationJob, CompilationJobRequirement, CompilationStep, JobProgressMap};
use crate::backends::{cranelift_compile, llvm_compile};

pub(crate) fn scheduling_loop(context: &GlobalCompilationContext, initial_job: CompilationJob) -> Option<()> {
    let mut progress_map: JobProgressMap = JobProgressMap::new();
    let mut pending_jobs: VecDeque<CompilationJob> = VecDeque::new();

    pending_jobs.push_front(initial_job);

    // TODO: Parallelize this loop
    'pending_loop: while !pending_jobs.is_empty() {
        let job = pending_jobs.pop_front().unwrap();

        for requirement in job.requirements.iter() {
            if !progress_map.step_complete(&requirement.unit, requirement.step) {
                pending_jobs.push_back(job);
                continue 'pending_loop;
            }
        }

        progress_map.insert_progress(job.unit.clone(), job.step)?;

        for new_req in handle_job(context, job)?.into_iter() {
            if !progress_map.job_complete(&new_req) {
                pending_jobs.push_back(new_req);
            } else {
                println!("Job already completed: {:?}", new_req);
            }
        }
    }

    Some(())
}

pub(crate) fn handle_job(
    context: &GlobalCompilationContext,
    job: CompilationJob
) -> Option<Box<[CompilationJob]>> {
    let map_reqs_new_stage = |job: CompilationJob, new_step: CompilationStep| {
        let new_requirements = job.requirements.into_iter()
            .map(|req| {
                CompilationJobRequirement {
                    unit: req.unit,

                    // requirement for the next step of a standard job is that all imports
                    // have completed the step it has just completed
                    step: job.step
                }
            })
            .collect::<Vec<_>>();

        Some(
            [
                CompilationJob::new(
                    new_requirements, new_step,
                    job.unit.clone()
                )
            ].into()
        )
    };

    perform_job(context, &job)?;

    match job.step {
        CompilationStep::PreParse => {
            let imports = context.request_imports(&job.unit)?
                .clone();

            let mut new_jobs = imports.iter()
                .map(|import| {
                    CompilationJob::new(
                        vec![],
                        CompilationStep::PreParse,
                        CompilationUnit::new(import.as_str()),
                    )
                })
                .collect::<Vec<_>>();

            let requirements = imports.into_iter()
                .map(|import| {
                    CompilationJobRequirement {
                        unit: CompilationUnit::new(import.as_str()),
                        step: CompilationStep::PreParse,
                    }
                })
                .collect::<Vec<_>>();

            new_jobs.push(
                CompilationJob::new(
                    requirements,
                    CompilationStep::ASTParse,
                    job.unit.clone()
                )
            );

            Some(new_jobs.into())
        },
        CompilationStep::ASTParse => {
            map_reqs_new_stage(job, CompilationStep::TypeCheck)
        },
        CompilationStep::TypeCheck => {
            map_reqs_new_stage(job, CompilationStep::BytecodeGen)
        },
        CompilationStep::BytecodeGen => {
            map_reqs_new_stage(job, CompilationStep::Codegen)
        },
        CompilationStep::Codegen => {
            Some([].into())
        },
    }
}

pub(crate) fn perform_job(
    context: &GlobalCompilationContext,
    job: &CompilationJob
) -> Option<()> {
    let get_lexemes = |unit: &CompilationUnit| {
        let file_contents = std::fs::read_to_string(unit.to_string())
            .unwrap_or_else(|_| panic!("File not found: {}", unit));
        let preprocess = preprocess(file_contents.as_str());
        lex(preprocess.as_str())
    };

    match job.step {
        CompilationStep::PreParse => {
            let tokens = get_lexemes(&job.unit);
            
            let output = preparse(
                TokenIter {
                    slice: &tokens,
                    index: 0,
                }
            ).unwrap_or_else(|| panic!("Preparse failed: {}", job.unit));

            let mut db = context.request_access_mut();

            db.import_data.insert(job.unit.clone(), output.imports);
            db.naive_type_data.insert(job.unit.clone(), output.type_definitions);
            db.function_data.insert(job.unit.clone(), output.function_definitions);
        },

        CompilationStep::ASTParse => {
            let imports = context.request_imports(&job.unit)?;
            let lexemes = get_lexemes(&job.unit);

            let mut db = context.request_access_mut();

            let mut self_type_map = db.naive_type_data.get_cloned(&job.unit)?;
            let self_function_map = db.function_data.get_cloned(&job.unit)?;

            for import in imports.into_iter() {
                let import_types = db.naive_type_data.get(&CompilationUnit::new(import.as_str()))?;

                for (name, _type) in import_types.iter() {
                    if _type.visibility_mode != VisibilityMode::Public { continue; };

                    self_type_map.insert(name.clone(), _type.clone());
                }
            }

            drop(db); // Release the mutable access while unused

            let base_ast = CXAST {
                type_map: self_type_map,
                function_map: self_function_map.clone(),

                ..Default::default()
            };

            let parsed_ast = parse_ast(TokenIter::new(&lexemes), base_ast)
                .expect("AST parsing failed");

            let mut db = context.request_access_mut();
            db.naive_ast.insert(job.unit.clone(), parsed_ast);
        },

        CompilationStep::TypeCheck => {
            let mut db = context.request_access_mut();

            let mut self_ast = db.naive_ast.get_cloned(&job.unit)?;
            let self_type_map = db.naive_type_data.get_cloned(&job.unit)?;

            let import_type_maps = db.import_data.get_cloned(&job.unit)?
                .into_iter()
                .map(|import| {
                    db.naive_type_data.get_cloned(&CompilationUnit::new(import.as_str()))
                        .expect("Import type map not found")
                })
                .collect::<Vec<_>>();
            let import_function_maps = db.import_data.get_cloned(&job.unit)?
                .into_iter()
                .map(|import| {
                    db.function_data.get_cloned(&CompilationUnit::new(import.as_str()))
                        .expect("Import function map not found")
                })
                .collect::<Vec<_>>();

            drop(db); // Release the mutable access while unused
            
            self_ast.type_map = collapse_typemap(&self_type_map, &import_type_maps)?;

            for fn_map in import_function_maps {
                for (name, function) in fn_map.iter() {
                    self_ast.function_map.insert(name.clone(), function.clone());
                }
            }
            
            let data = type_check(&mut self_ast)
                .expect("Type checking failed");

            let mut db = context.request_access_mut();

            db.typechecked_ast.insert(job.unit.clone(), self_ast);
            db.typecheck_data.insert(job.unit.clone(), data);
        },

        CompilationStep::BytecodeGen => {
            let mut db = context.request_access_mut();

            let self_ast = db.typechecked_ast.get_cloned(&job.unit)?;
            let typecheck_data = db.typecheck_data.get_cloned(&job.unit)?;

            let bytecode = generate_bytecode(self_ast, typecheck_data)
                .expect("Bytecode generation failed");

            db.bytecode_data.insert(job.unit.clone(), bytecode);
        },

        CompilationStep::Codegen => {
            let mut db = context.request_access_mut();

            let bytecode = db.bytecode_data.get_cloned(&job.unit)?;
            let mut internal_directory = internal_directory(&PathBuf::from(job.unit.to_string()));
            internal_directory.set_extension("o");
            
            match context.config.backend {
                CompilerBackend::LLVM => llvm_compile(&bytecode, internal_directory.to_str()?, context.config.optimization_level)
                    .expect("LLVM code generation failed"),

                CompilerBackend::Cranelift => cranelift_compile(&bytecode, internal_directory.to_str()?)
                    .expect("Cranelift code generation failed"),
            }
            
            context.linking_files.lock().expect("Deadlock on linking files mutex")
                .push(internal_directory);
        },

        _ => println!("Performing job for step: {:?} on unit: {}", job.step, job.unit),
    }

    Some(())
}
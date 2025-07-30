use crate::backends::{cranelift_compile, llvm_compile};
use cx_compiler_ast::lex::lex;
use cx_compiler_ast::parse::parse_ast;
use cx_compiler_ast::preparse::preparse;
use cx_compiler_ast::preprocessor::preprocess;
use cx_compiler_bytecode::generate_bytecode;
use cx_compiler_typechecker::type_check;
use cx_compiler_typechecker::typemap_collapsing::collapse_typemap;
use cx_data_ast::parse::ast::CXAST;
use cx_data_ast::parse::parser::{TokenIter, VisibilityMode};
use cx_data_pipeline::directories::internal_directory;
use cx_data_pipeline::jobs::{CompilationJob, CompilationJobRequirement, CompilationStep, JobQueue};
use cx_data_pipeline::{CompilationUnit, CompilerBackend, GlobalCompilationContext};
use std::path::PathBuf;

pub(crate) fn scheduling_loop(context: &GlobalCompilationContext, initial_job: CompilationJob) -> Option<()> {
    let mut queue = JobQueue::new();

    queue.push_job(initial_job);
    
    // TODO: Parallelize this loop
    while !queue.is_empty() {
        let job = queue.pop_job().unwrap();

        if !queue.requirements_complete(&job) {
            queue.push_job(job);
            continue;
        }
        
        queue.complete_job(&job);
        
        // println!("Handling job: {:?}", job);
        for new_jobs in handle_job(context, job)?.into_iter() {
            queue.push_new_job(new_jobs);
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
            let imports = context.module_db.import_data
                .get_cloned(&job.unit);

            let mut new_jobs = imports.iter()
                .map(|import| {
                    CompilationJob::new(
                        vec![],
                        CompilationStep::PreParse,
                        CompilationUnit::new(import.as_str()),
                    )
                })
                .collect::<Vec<_>>();
            
            let self_job = CompilationJob::new(
                new_jobs.iter().map(|req| req.as_requirement()).collect(),
                CompilationStep::ASTParse,
                job.unit.clone()
            );
            
            new_jobs.push(self_job);

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

            context.module_db.lex_tokens
                .insert(job.unit.clone(), tokens);
            context.module_db.import_data
                .insert(job.unit.clone(), output.imports);
            context.module_db.naive_type_data
                .insert(job.unit.clone(), output.type_definitions);
            context.module_db.function_data
                .insert(job.unit.clone(), output.function_definitions);
        },

        CompilationStep::ASTParse => {
            let mut self_type_map = context.module_db.naive_type_data
                .get_cloned(&job.unit);
            let self_function_map = context.module_db.function_data
                .get_cloned(&job.unit);
            let lexemes = context.module_db.lex_tokens
                .take(&job.unit);
            
            let imports = context.module_db.import_data
                .get(&job.unit);
            
            for import in imports.iter() {
                let import_types = context.module_db.naive_type_data
                    .get(&CompilationUnit::new(import.as_str()));

                for (name, _type) in import_types.iter() {
                    if _type.visibility_mode != VisibilityMode::Public { continue; };

                    self_type_map.insert(name.clone(), _type.clone());
                }
            }

            let base_ast = CXAST {
                type_map: self_type_map,
                function_map: self_function_map,

                ..Default::default()
            };

            let parsed_ast = parse_ast(TokenIter::new(&lexemes), base_ast)
                .expect("AST parsing failed");

            context.module_db.lex_tokens.insert(job.unit.clone(), lexemes);
            context.module_db.naive_ast.insert(job.unit.clone(), parsed_ast);
        },

        CompilationStep::TypeCheck => {
            let mut self_ast = context.module_db.naive_ast
                .take(&job.unit);
            let self_type_map = context.module_db.naive_type_data
                .get_cloned(&job.unit);

            let import_type_maps = context.module_db.import_data.get(&job.unit)
                .iter()
                .map(|import| {
                    context.module_db.naive_type_data.get_cloned(&CompilationUnit::new(import.as_str()))
                })
                .collect::<Vec<_>>();
            let import_function_maps = context.module_db.import_data.get_cloned(&job.unit)
                .into_iter()
                .map(|import| {
                    context.module_db.function_data.get_cloned(&CompilationUnit::new(import.as_str()))
                })
                .collect::<Vec<_>>();
            
            self_ast.type_map = collapse_typemap(&self_type_map, &import_type_maps)?;

            for fn_map in import_function_maps {
                for (name, function) in fn_map.iter() {
                    self_ast.function_map.insert(name.clone(), function.clone());
                }
            }
            
            let data = type_check(&mut self_ast)
                .expect("Type checking failed");

            context.module_db.typechecked_ast.insert(job.unit.clone(), self_ast);
            context.module_db.typecheck_data.insert(job.unit.clone(), data);
        },

        CompilationStep::BytecodeGen => {
            let self_ast = context.module_db.typechecked_ast.get_cloned(&job.unit);
            let typecheck_data = context.module_db.typecheck_data.get_cloned(&job.unit);

            let bytecode = generate_bytecode(self_ast, typecheck_data)
                .expect("Bytecode generation failed");

            context.module_db.bytecode_data.insert(job.unit.clone(), bytecode);
        },

        CompilationStep::Codegen => {
            let bytecode = context.module_db.bytecode_data.get_cloned(&job.unit);
            let mut internal_directory = internal_directory(&PathBuf::from(job.unit.to_string()));
            internal_directory.set_extension("o");
            
            match context.config.backend {
                CompilerBackend::LLVM => llvm_compile(&bytecode, internal_directory.to_str()?, context.config.optimization_level)
                    .expect("LLVM code generation failed"),

                CompilerBackend::Cranelift => cranelift_compile(&bytecode, internal_directory.to_str()?)
                    .expect("Cranelift code generation failed"),
            }
            
            context.linking_files.lock().expect("Deadlock on linking files mutex")
                .insert(internal_directory);
        },

        _ => todo!("Performing job for step: {:?} on unit: {}", job.step, job.unit),
    }

    Some(())
}
use speedy::{LittleEndian, Readable, Writable};
use crate::backends::{cranelift_compile, llvm_compile};
use cx_compiler_ast::parse::parse_ast;
use cx_compiler_ast::preparse::preparse;
use cx_compiler_bytecode::generate_bytecode;
use cx_compiler_typechecker::type_check;
use cx_compiler_typechecker::typemap_collapsing::collapse_typemap;
use cx_data_ast::parse::ast::CXAST;
use cx_data_ast::parse::parser::VisibilityMode;
use cx_data_pipeline::db::ModuleMap;
use cx_data_pipeline::directories::{file_path, internal_directory};
use cx_data_pipeline::internal_storage::{resource_path, retrieve_data, retrieve_text, store_text};
use cx_data_pipeline::jobs::{CompilationJob, CompilationJobRequirement, CompilationStep, JobQueue};
use cx_data_pipeline::{CompilationUnit, CompilerBackend, GlobalCompilationContext};
use std::collections::{HashMap, HashSet};
use std::hash::{DefaultHasher, Hash, Hasher};
use cx_compiler_lexer::lex::lex;
use cx_compiler_lexer::preprocessor::preprocess;
use cx_data_ast::parse::intrinsic_types::INTRINSIC_IMPORTS;
use cx_data_ast::parse::maps::CXDestructorMap;
use cx_data_ast::parse::type_mapping::{contextualize_fn_map, contextualize_type_map};
use cx_data_lexer::TokenIter;
use cx_util::format::{dump_all, dump_data};
use crate::template_realizing::realize_templates;

pub(crate) fn scheduling_loop(context: &GlobalCompilationContext, initial_job: CompilationJob) -> Option<()> {
    let mut queue = JobQueue::new();

    let mut compilation_exists = HashMap::new();

    queue.push_job(initial_job);
    
    // TODO: Parallelize this loop
    'queue: while !queue.is_empty() {
        let mut job = queue.pop_job().unwrap();
        
        compilation_exists.insert(job.unit.clone(), job.compilation_exists);
        
        if !cfg!(debug_assertions) && job.compilation_exists {
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
                    },

                    // previous compilation exists for a dependency, if all other dependencies are the same
                    // we may skip this job
                    Some(true) => {},

                    // all dependencies have not been processed yet, so we must check again later
                    _ => {
                        queue.push_job(job);
                        continue 'queue;
                    },
                }
            }

            println!("Skipping job: {} at step: {:?} as it has already been compiled", job.unit, job.step);
            queue.complete_all_unit_jobs(&job.unit);
            context.module_db.set_no_reexport(&job.unit);
            context.linking_files.lock()
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
    mut job: CompilationJob
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

    match perform_job(context, &job)? {
        JobResult::StandardSuccess => {},
        JobResult::UnchangedSinceLastCompilation => job.compilation_exists = true
    };

    match job.step {
        CompilationStep::PreParse => {
            let pp_data = context.module_db.preparse_contents
                .get_cloned(&job.unit);
            
            let mut new_jobs = pp_data.imports.iter()
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

fn load_precompiled_data(context: &GlobalCompilationContext, unit: &CompilationUnit) -> Option<()> {
    fn retrieve_map_data<'a, T>(context: &GlobalCompilationContext, map: &ModuleMap<T>, unit: &CompilationUnit) -> Option<()>
        where T: Clone + Readable<'a, LittleEndian> + Writable<LittleEndian>
    {
        if let Some(data) = retrieve_data::<T>(context, unit, &map.storage_extension) {
            map.insert(unit.clone(), data);
            Some(())
        } else {
            println!("Failed to retrieve data for unit: {} with storage extension: {}", unit, map.storage_extension);
            None
        }
    }
    
    // retrieve_map_data(context, &context.module_db.naive_type_data, unit)?;
    // retrieve_map_data(context, &context.module_db.naive_function_data, unit)?;

    Some(())
}

pub(crate) enum JobResult {
    StandardSuccess,
    UnchangedSinceLastCompilation
}

pub(crate) fn perform_job(
    context: &GlobalCompilationContext,
    job: &CompilationJob
) -> Option<JobResult> {
    match job.step {
        CompilationStep::PreParse => {
            let file_path = job.unit.with_extension("cx");
            let file_contents = std::fs::read_to_string(file_path)
                .unwrap_or_else(|_| panic!("File not found: {}", job.unit));

            let mut hasher = DefaultHasher::new();
            file_contents.hash(&mut hasher);
            
            let current_hash = hasher.finish().to_string();
            let previous_hash = retrieve_text(context, &job.unit, ".hash")
                .unwrap_or(String::new());
            
            let identical_hash = previous_hash == current_hash;
            
            store_text(context, &job.unit, ".hash", &current_hash);

            let preprocess = preprocess(file_contents.as_str());
            let tokens = lex(preprocess.as_str());

            let mut output = preparse(
                TokenIter {
                    slice: &tokens,
                    index: 0,
                }
            ).unwrap_or_else(|| panic!("Preparse failed: {}", job.unit));

            if !job.unit.as_str().contains("std") {
                output.imports
                    .extend(
                        INTRINSIC_IMPORTS.iter().map(|s| s.to_string())
                    );
            }
            
            context.module_db.lex_tokens
                .insert(job.unit.clone(), tokens);
            context.module_db.preparse_contents
                .insert(job.unit.clone(), output);
            
            return if identical_hash {
                Some(JobResult::UnchangedSinceLastCompilation)
            } else {
                Some(JobResult::StandardSuccess)
            };
        },

        CompilationStep::ASTParse => {
            let mut pp_data = context.module_db.preparse_contents
                .get_cloned(&job.unit);
            let lexemes = context.module_db.lex_tokens
                .get(&job.unit);
            
            for import in pp_data.imports.iter() {
                let other_pp_data = context.module_db.preparse_contents
                    .get(&CompilationUnit::from_str(import.as_str()));

                for (name, _type) in other_pp_data.type_definitions.iter() {
                    if _type.visibility != VisibilityMode::Public { continue; };

                    pp_data.type_definitions.insert(name.clone(), _type.clone());
                }

                for (name, visibility, func) in other_pp_data.function_definitions.iter() {
                    if visibility != &VisibilityMode::Public { continue; };
                    
                    pp_data.function_definitions.push((name.clone(), VisibilityMode::Private, func.clone()));
                }
            }
            
            let cx_type_map = contextualize_type_map(
                &pp_data.type_definitions, &pp_data.type_templates
            ).expect("Type map contextualization failed");
            
            let cx_fn_map = contextualize_fn_map(
                &cx_type_map, &pp_data.function_definitions, &pp_data.function_templates
            ).expect("Function map contextualization failed");
            
            let cx_destructor_defs = pp_data.destructor_definitions
                .iter()
                .filter_map(|name| Some((cx_type_map.get(name.as_str()).cloned()?, name.clone())))
                .collect::<CXDestructorMap>();

            let base_ast = CXAST {
                type_map: cx_type_map, 
                function_map: cx_fn_map,
                destructor_map: cx_destructor_defs,
                
                ..Default::default()
            };

            let parsed_ast = parse_ast(TokenIter::new(&lexemes), base_ast)
                .expect("AST parsing failed");

            dump_data(&parsed_ast);

            context.module_db.naive_ast.insert(job.unit.clone(), parsed_ast);
        },

        CompilationStep::TypeCheck => {
            let lexemes = context.module_db.lex_tokens
                .get(&job.unit);
            let mut self_ast = context.module_db.naive_ast
                .take(&job.unit);
            
            // TODO: Get templates from other imported modules
            
            let data = type_check(&lexemes, &mut self_ast)
                .expect("Type checking failed");

            dump_data(&self_ast);

            context.module_db.typechecked_ast.insert(job.unit.clone(), self_ast);
            context.module_db.typecheck_data.insert(job.unit.clone(), data);
        },

        CompilationStep::BytecodeGen => {
            realize_templates(context, &job.unit)
                .expect("Template realizing failed");
            
            let self_ast = context.module_db.typechecked_ast.take(&job.unit);
            let typecheck_data = context.module_db.typecheck_data.take(&job.unit);
            
            let bytecode = generate_bytecode(self_ast, typecheck_data)
                .expect("Bytecode generation failed");

            dump_data(&bytecode);

            context.module_db.bytecode_data.insert(job.unit.clone(), bytecode);
        },

        CompilationStep::Codegen => {
            let bytecode = context.module_db.bytecode_data.take(&job.unit);
            let mut internal_directory = internal_directory(context, &job.unit);
            internal_directory.push(".o");
            
            match context.config.backend {
                CompilerBackend::LLVM => llvm_compile(&bytecode, internal_directory.to_str()?, context.config.optimization_level)
                    .expect("LLVM code generation failed"),

                CompilerBackend::Cranelift => cranelift_compile(&bytecode, internal_directory.to_str()?)
                    .expect("Cranelift code generation failed"),
            }
            
            context.linking_files.lock().expect("Deadlock on linking files mutex")
                .insert(internal_directory);
        }
    }

    Some(JobResult::StandardSuccess)
}
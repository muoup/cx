use speedy::{LittleEndian, Readable, Writable};
use crate::backends::{cranelift_compile, llvm_compile};
use cx_compiler_ast::parse::parse_ast;
use cx_compiler_ast::preparse::preparse;
use cx_compiler_mir::generate_bytecode;
use cx_data_ast::parse::ast::CXAST;
use cx_data_ast::parse::parser::VisibilityMode;
use cx_data_pipeline::db::ModuleMap;
use cx_data_pipeline::directories::internal_directory;
use cx_data_pipeline::internal_storage::{resource_path, retrieve_data, retrieve_text, store_text};
use cx_data_pipeline::jobs::{CompilationJob, CompilationJobRequirement, CompilationStep, JobQueue};
use cx_data_pipeline::{CompilationUnit, CompilerBackend, GlobalCompilationContext};
use std::collections::{HashMap, HashSet};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::io::Write;
use fs2::FileExt;
use cx_compiler_typechecker_new::precontextualizing::{contextualize_fn_map, contextualize_globals, contextualize_type_map};
use cx_compiler_lexer::lex::lex;
use cx_compiler_lexer::preprocessor::preprocess;
use cx_compiler_typechecker_new::environment::TCEnvironment;
use cx_compiler_typechecker_new::typecheck;
use cx_data_typechecker::intrinsic_types::INTRINSIC_IMPORTS;
use cx_data_lexer::TokenIter;
use cx_data_typechecker::ast::{TCBaseMappings, TCAST};
use cx_util::format::dump_data;
use cx_util::scoped_map::ScopedMap;
use crate::template_realizing::realize_templates;

pub(crate) fn scheduling_loop(context: &GlobalCompilationContext, initial_job: CompilationJob) -> Option<()> {
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
            let pp_data = context.module_db.preparse_incomplete
                .get(&job.unit);
            
            let mut new_jobs = pp_data.imports.iter()
                .map(|import| {
                    CompilationJob::new(
                        vec![],
                        CompilationStep::PreParse,
                        CompilationUnit::from_str(import.as_str()),
                    )
                })
                .collect::<Vec<_>>();
            
            job.step = CompilationStep::ImportCombine;
            new_jobs.push(job);

            Some(new_jobs.into())
        },
        CompilationStep::ImportCombine => {
            map_reqs_new_stage(job, CompilationStep::ASTParse)
        },
        CompilationStep::ASTParse => {
            map_reqs_new_stage(job, CompilationStep::TypeCompletion)
        },
        CompilationStep::TypeCompletion => {
            map_reqs_new_stage(job, CompilationStep::Typechecking)
        },
        CompilationStep::Typechecking => {
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
    
    retrieve_map_data(context, &context.module_db.preparse_incomplete, unit)?;
    retrieve_map_data(context, &context.module_db.preparse_full, unit)?;

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
                .unwrap_or_default();
            
            let identical_hash = previous_hash == current_hash;
            let object_exists = std::fs::metadata(
                internal_directory(context, &job.unit).with_extension("o")
            ).is_ok();
            
            store_text(context, &job.unit, ".hash", &current_hash);

            let preprocess = preprocess(file_contents.as_str());
            let tokens = lex(preprocess.as_str());

            let mut output = preparse(
                TokenIter {
                    slice: &tokens,
                    index: 0,
                }
            ).unwrap_or_else(|| panic!("Preparse failed: {}", job.unit));
            output.module = job.unit.to_string();

            if !job.unit.as_str().contains("std") {
                output.imports
                    .extend(
                        INTRINSIC_IMPORTS.iter().map(|s| s.to_string())
                    );
            }
            
            context.module_db.lex_tokens
                .insert(job.unit.clone(), tokens);
            context.module_db.preparse_incomplete
                .insert(job.unit.clone(), output);
            
            return if identical_hash && object_exists {
                Some(JobResult::UnchangedSinceLastCompilation)
            } else {
                Some(JobResult::StandardSuccess)
            };
        },
        
        CompilationStep::ImportCombine => {
            let mut pp_data = context.module_db.preparse_incomplete
                .get_cloned(&job.unit);

            for import in pp_data.imports.iter() {
                let other_pp_data = context.module_db.preparse_incomplete
                    .get(&CompilationUnit::from_str(import.as_str()));
                let required_visiblity = VisibilityMode::Public;

                for (name, _type) in other_pp_data.type_definitions
                    .standard.iter() {
                    if _type.visibility < required_visiblity { continue; };

                    pp_data.type_definitions.insert_standard(name.clone(), _type.transfer(import));
                }

                for (name, template) in other_pp_data.type_definitions
                    .templates.iter() {
                    if template.visibility < required_visiblity { continue; };
                    
                    pp_data.type_definitions.insert_template(name.clone(), template.transfer(import));
                }

                for (name, prototype) in other_pp_data.function_definitions
                    .standard.iter() {
                    if prototype.visibility < required_visiblity { continue; };

                    pp_data.function_definitions.insert_standard(name.clone(), prototype.transfer(import));
                }
                
                for (name, template) in other_pp_data.function_definitions
                    .templates.iter() {
                    if template.visibility < required_visiblity { continue; };
                    
                    pp_data.function_definitions.insert_template(name.clone(), template.transfer(import));
                }
            }

            context.module_db.preparse_full
                .insert(job.unit.clone(), pp_data);
        },

        CompilationStep::ASTParse => {
            let pp_data = context.module_db.preparse_full
                .get(&job.unit);
            let lexemes = context.module_db.lex_tokens
                .get(&job.unit);
            
            let base_ast = CXAST {
                type_map: pp_data.type_definitions.to_owned(), 
                function_map: pp_data.function_definitions.to_owned(),
                
                ..Default::default()
            };

            let parsed_ast = parse_ast(TokenIter::new(&lexemes), base_ast)
                .expect("AST parsing failed");

            if !job.unit.is_std_lib() {
                dump_data(&parsed_ast);
            }

            context.module_db.naive_ast.insert(job.unit.clone(), parsed_ast);
        },

        CompilationStep::TypeCompletion => {
            let self_ast = context.module_db.naive_ast
                .get(&job.unit);
            
            let mut type_data = contextualize_type_map(&context.module_db, &self_ast.type_map)
                .expect("Failed to contextualize type map");
            let fn_data = contextualize_fn_map(&context.module_db, &self_ast.function_map, &mut type_data, &self_ast.type_map)
                .expect("Failed to contextualize function map");
            let global_variables = contextualize_globals(&context.module_db, &mut type_data, &self_ast.type_map, &self_ast)
                .expect("Failed to contextualize global variables");

            let completed_data = TCBaseMappings {
                type_data, fn_data, global_variables
            };

            context.module_db.structure_data.insert(job.unit.clone(), completed_data.clone());
        },

        CompilationStep::Typechecking => {
            let structure_data = context.module_db.structure_data
                .get(&job.unit);
            let self_ast = context.module_db.naive_ast
                .get(&job.unit);

            let mut env = TCEnvironment::new(structure_data.as_ref());
            typecheck(&mut env, &self_ast)
                .expect("Typechecking failed");

            realize_templates(context, &job.unit, &mut env)
                .expect("Template realization failed");

            env.realized_types.extend(structure_data.type_data.standard.clone());
            env.realized_fns.extend(structure_data.fn_data.standard.clone());
            env.realized_globals.extend(structure_data.global_variables.clone());

            let tc_ast = TCAST {
                source_file: self_ast.file_path.clone(),

                type_map: env.realized_types,
                fn_map: env.realized_fns,
                global_variables: env.realized_globals
                    .into_iter()
                    .map(|(_, gv)| gv)
                    .collect(),

                destructors_required: env.deconstructors.into_iter()
                    .collect::<_>(),
                function_defs: env.declared_functions
            };

            if !job.unit.is_std_lib() {
                dump_data(&tc_ast);
            }

            context.module_db.typechecked_ast.insert(job.unit.clone(), tc_ast);
        },

        CompilationStep::BytecodeGen => {
            let tc_ast = context.module_db.typechecked_ast.take(&job.unit);
            
            let bytecode = generate_bytecode(tc_ast)
                .expect("Bytecode generation failed");

            if !job.unit.is_std_lib() {
                dump_data(&bytecode);
            }

            context.module_db.bytecode.insert(job.unit.clone(), bytecode);
        },

        CompilationStep::Codegen => {
            let bytecode = context.module_db.bytecode.take(&job.unit);
            let mut internal_directory = internal_directory(context, &job.unit);
            internal_directory.push(".o");
            
            let buffer = match context.config.backend {
                CompilerBackend::LLVM => llvm_compile(&bytecode, internal_directory.to_str()?, context.config.optimization_level)
                    .expect("LLVM code generation failed"),

                CompilerBackend::Cranelift => cranelift_compile(&bytecode, internal_directory.to_str()?)
                    .expect("Cranelift code generation failed"),
            };

            let mut file = std::fs::File::create(&internal_directory)
                .expect("Failed to create object file");
            file.lock_exclusive()
                .expect("Failed to lock object file for writing");

            file.write_all(&buffer)
                .expect("Failed to write object file");
            context.linking_files.lock()
                .expect("Deadlock on linking files mutex")
                .insert(internal_directory);
        }
    }

    Some(JobResult::StandardSuccess)
}
use std::collections::{HashMap, HashSet, VecDeque};
use cx_compiler_ast::lex::lex;
use cx_compiler_ast::preparse::preparse;
use cx_compiler_ast::preprocessor::preprocess;
use cx_data_ast::parse::parser::TokenIter;
use cx_data_pipeline::{CompilationUnit, GlobalCompilationContext};
use cx_data_pipeline::jobs::{CompilationJob, CompilationJobRequirement, CompilationStep, JobProgressMap};

pub(crate) fn scheduling_loop(context: &GlobalCompilationContext, initial_job: CompilationJob) -> Option<()> {
    let mut progress_map: JobProgressMap = JobProgressMap::new();
    let mut pending_jobs: VecDeque<CompilationJob> = VecDeque::new();

    pending_jobs.push_front(initial_job);

    // TODO: Parallelize this loop
    'pending_loop: while !pending_jobs.is_empty() {
        let job = pending_jobs.pop_front().unwrap();

        for requirement in job.requirements.iter() {
            if !progress_map.get_progress(&requirement.unit, requirement.step) {
                pending_jobs.push_back(job);
                continue 'pending_loop;
            }
        }

        progress_map.insert_progress(job.unit.clone(), job.step)?;
        let new_jobs = handle_job(context, job)?;

        for new_req in new_jobs.into_iter() {
            if !progress_map.get_job_progress(&new_req) {
                pending_jobs.push_back(new_req);
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
                    step: CompilationStep::PreParseStage2,
                }
            })
            .collect::<Vec<_>>();

        Some(
            [
                CompilationJob::new(
                    new_requirements,
                    CompilationStep::ASTParse,
                    job.unit.clone()
                )
            ].into()
        )
    };
    
    perform_job(context, &job)?;

    match job.step {
        CompilationStep::PreParseStage1 => {
            let imports = context.request_imports(&job.unit)?
                .clone();
            
            let mut new_jobs = imports.iter()
                .map(|import| {
                    CompilationJob::new(
                        vec![],
                        CompilationStep::PreParseStage1,
                        CompilationUnit::from(import.clone()),
                    )
                })
                .collect::<Vec<_>>();

            let requirements = imports.into_iter()
                .map(|import| {
                    CompilationJobRequirement {
                        unit: CompilationUnit::from(import),
                        step: CompilationStep::PreParseStage1,
                    }
                })
                .collect::<Vec<_>>();

            new_jobs.push(
                CompilationJob::new(
                    requirements,
                    CompilationStep::PreParseStage2,
                    job.unit.clone()
                )
            );

            Some(new_jobs.into())
        },
        CompilationStep::PreParseStage2 => {
            map_reqs_new_stage(job, CompilationStep::ASTParse)
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
    match job.step {
        CompilationStep::PreParseStage1 => {
            println!("PreParseStage 1 for unit: {}", job.unit);
            
            let file_contents = std::fs::read_to_string(job.unit.to_string())
                .unwrap_or_else(|_| panic!("File not found: {}", job.unit));
            let preprocess = preprocess(file_contents.as_str());
            let lex = lex(preprocess.as_str());
            
            let mut token_iter = TokenIter {
                slice: &lex,
                index: 0,
            };
            
            let output = preparse(&mut token_iter)?;
            
            println!("PreParseStage 1 completed for unit: {}", job.unit);
            println!("Output: {:?}", output);
            
            context.request_lock()
                .and_then(|mut db| {
                    db.import_data.insert(job.unit.clone(), output.imports);
                    db.type_data.insert(job.unit.clone(), output.type_definitions);
                    
                    Some(())
                });
        },
        
        _ => println!("Performing job for step: {:?} on unit: {}", job.step, job.unit),
    }
    
    Some(())
}
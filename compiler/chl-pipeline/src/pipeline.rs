use crate::scheduler::scheduling_loop;
use chl_log::error::CHLError;
use chl_pipeline_data::jobs::{CompilationJob, CompilationStep, JobQueue};
use chl_pipeline_data::{CompilationUnit, GlobalCompilationContext};
use chl_util::format::with_dump_directory;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FailurePolicy {
    Halt,
    Continue,
}

pub struct PipelineFailure {
    pub job: CompilationJob,
    pub error: CHLError,
}

pub struct Pipeline<'a> {
    context: &'a GlobalCompilationContext,
    queue: JobQueue,
    until: CompilationStep,
    policy: FailurePolicy,
}

impl<'a> Pipeline<'a> {
    pub fn new(context: &'a GlobalCompilationContext, until: CompilationStep) -> Self {
        Self {
            context,
            queue: JobQueue::new(),
            until,
            policy: FailurePolicy::Halt,
        }
    }

    pub fn policy(mut self, policy: FailurePolicy) -> Self {
        self.policy = policy;
        self
    }

    pub fn run(
        &mut self,
        roots: impl IntoIterator<Item = CompilationUnit>,
        on_start: impl FnMut(&CompilationJob),
    ) -> Result<(), Vec<PipelineFailure>> {
        let config = &self.context.config;
        for unit in roots {
            self.queue.push_new_job(CompilationJob::new(vec![], CompilationStep::PreParse, unit));
        }

        with_dump_directory(config.internal_directory.clone(), config.dump, || {
            scheduling_loop(self.context, &mut self.queue, self.until, self.policy, on_start)
        })
    }
}

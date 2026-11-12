use crate::scheduler::scheduling_loop;
use cx_log::error::CXError;
use cx_pipeline_data::jobs::{CompilationJob, CompilationStep};
use cx_pipeline_data::{CompilationUnit, GlobalCompilationContext};
use cx_util::format::with_dump_directory;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FailurePolicy {
    Halt,
    Continue,
}

pub struct PipelineFailure {
    pub job: CompilationJob,
    pub error: CXError,
}

pub struct Pipeline<'a> {
    context: &'a GlobalCompilationContext,
    roots: Vec<CompilationUnit>,
    until: CompilationStep,
    policy: FailurePolicy,
}

impl<'a> Pipeline<'a> {
    pub fn new(context: &'a GlobalCompilationContext, until: CompilationStep) -> Self {
        Self {
            context,
            roots: Vec::new(),
            until,
            policy: FailurePolicy::Halt,
        }
    }

    pub fn roots(mut self, units: impl IntoIterator<Item = CompilationUnit>) -> Self {
        self.roots.extend(units);
        self
    }

    pub fn policy(mut self, policy: FailurePolicy) -> Self {
        self.policy = policy;
        self
    }

    pub fn run(self, on_start: impl FnMut(&CompilationJob)) -> Result<(), Vec<PipelineFailure>> {
        let config = &self.context.config;
        let jobs = self
            .roots
            .into_iter()
            .map(|unit| CompilationJob::new(vec![], CompilationStep::PreParse, unit));

        with_dump_directory(config.internal_directory.clone(), config.dump, || {
            scheduling_loop(self.context, jobs, self.until, self.policy, on_start)
        })
    }
}
